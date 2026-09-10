# ============================================================
#  test_phase5.R — Standalone verification runner
#  Run this from your project root:  Rscript R/test_phase5.R
#  It tests both pedotransfer (soil water) and phosphorus
#  calculations against the known Wanulcas.xlsm cached values.
# ============================================================

source("R/pedotransfer.R")
source("R/phosphorus.R")

pass <- 0L; fail <- 0L
check <- function(name, computed, expected, tol = 1e-6) {
  ok <- abs(computed - expected) < tol * max(1, abs(expected))
  tag <- if (ok) "PASS" else "FAIL"
  if (!ok) fail <<- fail + 1L else pass <<- pass + 1L
  cat(sprintf("  [%s] %-25s  R=%.10f  xlsm=%.10f\n", tag, name, computed, expected))
}

cat(strrep("=",60), "\n")
cat(" PEDOTRANSFER VERIFICATION\n")
cat(strrep("=",60), "\n")

# --- Inputs from xlsm Pedotransfer sheet ---
clay <- 23.12; silt <- 63.41; orgC <- 1.25; bd <- 1.393
top <- 0; cec <- 10.2; ph <- 4.55

# --- Method 2: Tomasella-Hodnett (tropical) ---
cat("\nMethod 2 (Tomasella-Hodnett, tropical soils):\n")
vg2 <- pedotransfer_vg(clay, silt, orgC, bd, top, cec, ph, method = 2)
check("Theta_sat",   vg2$theta_sat, 0.423998)
check("Ksat",        vg2$ksat,     18.326252)
check("Alpha",       vg2$alpha,     1.370195)
check("lambda",      vg2$lambda,   -2.800276)
check("n",           vg2$n,         1.454541)
check("Theta_resid", vg2$theta_res, 0.209117)

# --- Method 1: Wosten (temperate) ---
cat("\nMethod 1 (Wosten, temperate soils):\n")
vg1 <- pedotransfer_vg(clay, silt, orgC, bd, top, cec, ph, method = 1)
check("Theta_sat",   vg1$theta_sat, 0.448048)
check("Ksat",        vg1$ksat,     18.326252)
check("Alpha",       vg1$alpha,     0.020366)
check("lambda",      vg1$lambda,   -2.800276)
check("n",           vg1$n,         1.158693)
check("Theta_resid", vg1$theta_res, 0.0)

# --- Retention curve sanity ---
cat("\nRetention curve (Hodnett):\n")
rc <- vg_retention_curve(vg2)
cat(sprintf("  Curve has %d points, pF range %.1f-%.1f\n", nrow(rc), min(rc$pF), max(rc$pF)))
cat(sprintf("  Theta at pF=0 (h=1cm): %.4f\n", rc$theta[1]))
cat(sprintf("  Theta at pF=4.2:       %.4f\n", rc$theta[nrow(rc)]))
# At pF=0, h=1cm so theta < theta_sat; theta_sat is the asymptote at h->0.
# Check theta is monotonically decreasing (physically correct).
is_decreasing <- all(diff(rc$theta) <= 0)
if (is_decreasing) {
  cat("  [PASS] Retention curve is monotonically decreasing\n"); pass <- pass + 1L
} else { cat("  [FAIL] Curve not monotonic\n"); fail <- fail + 1L }
# Check theta at pF=0 is in a plausible range (between theta_res and theta_sat)
if (rc$theta[1] > vg2$theta_res && rc$theta[1] <= vg2$theta_sat + 0.01) {
  cat("  [PASS] Theta(pF=0) within physical bounds\n"); pass <- pass + 1L
} else { cat("  [FAIL] Theta(pF=0) outside bounds\n"); fail <- fail + 1L }

cat("\n", strrep("=",60), "\n")
cat(" PHOSPHORUS VERIFICATION\n")
cat(strrep("=",60), "\n")

# --- Inputs from xlsm Phosphorus sheet ---
# Soil type 12 = custom, with these looked-up coefficients:
coefs <- list(SorbMax1=10.26, SorbMax2=0.08, SorbAff1=1555, SorbAff2=3633.6, ThetaSat=0.4)
bd_l1 <- 1.37
pbray_l1 <- 5.38  # P-Bray layer 1, zone 1 (mg/kg)

cat("\nP-Bray -> N_PStInit (P-Bray method, layer 1, zone 1):\n")
conv <- pbray_conversion(1)
init11 <- n_pstinit(pbray_l1, bd_l1, coefs, conv)
check("N_PStInit11", init11, 0.23398251314654825, tol = 1e-10)

cat("\nLangmuir sorption curve:\n")
conc_grid <- p_conc_grid()
pm <- langmuir_pmobile(conc_grid, coefs$SorbMax1, coefs$SorbMax2,
                        coefs$SorbAff1, coefs$SorbAff2, coefs$ThetaSat)
check("PStSMin (B93)",  pm[2],           0.00048733836248279327, tol = 1e-10)
check("PStSMax (B143)", pm[length(pm)],  8.445201114197138,      tol = 1e-10)

cat("\nKa (apparent partition coefficient):\n")
ka <- langmuir_ka(conc_grid, coefs$SorbMax1, coefs$SorbMax2,
                   coefs$SorbAff1, coefs$SorbAff2, coefs$ThetaSat)
cat(sprintf("  Ka has %d values; Ka[1]=%.2f (should be ~16244.61)\n", length(ka$ka), ka$ka[1]))
if (abs(ka$ka[1] - 16244.61) < 1) {
  cat("  [PASS] Ka[1] matches\n"); pass <- pass + 1L
} else { cat("  [FAIL] Ka[1] mismatch\n"); fail <- fail + 1L }

# --- Layer 2 check ---
cat("\nLayer 2 (same soil type, same P-Bray, BD=1.38):\n")
init12 <- n_pstinit(5.56, 1.38, coefs, conv)
check("N_PStInit12", init12, 0.2433474591883967, tol = 1e-10)

# --- Summary ---
cat("\n", strrep("=",60), "\n")
cat(sprintf(" RESULTS:  %d PASSED,  %d FAILED\n", pass, fail))
cat(strrep("=",60), "\n")
if (fail > 0) {
  cat("\n*** FAILURES DETECTED — please report the output above ***\n")
  quit(status = 1)
} else {
  cat("\nAll calculations verified against Wanulcas.xlsm.\n")
  cat("Both pedotransfer and phosphorus modules are correct.\n")
}

# ============================================================
#  TRY YOUR OWN VALUES
#  Uncomment and edit the section below, then re-run this file.
# ============================================================

cat("\n", strrep("=",60), "\n")
cat(" CUSTOM INPUT EXAMPLES (edit these to try your own values)\n")
cat(strrep("=",60), "\n")

# ---- PEDOTRANSFER: change soil texture ----
cat("\n--- Pedotransfer with custom inputs ---\n")
my_vg <- pedotransfer_vg(
  clay   = 23.12,     # % clay           (change this)
  silt   = 63.41,     # % silt           (change this)
  orgC   = 1.25,      # % organic carbon (change this)
  bd     = 1.393,     # bulk density g/cm3 (change this)
  top    = 0,         # 1 = topsoil, 0 = subsoil
  cec    = 10.2,      # CEC cmol/kg (only matters for method 2)
  ph     = 4.55,      # pH          (only matters for method 2)
  method = 2          # 1 = Wosten (temperate), 2 = Tomasella-Hodnett (tropical)
)
cat("  Theta_sat     =", round(my_vg$theta_sat, 6), "\n")
cat("  Ksat (cm/d)   =", round(my_vg$ksat, 4), "\n")
cat("  Alpha (cm-1)  =", round(my_vg$alpha, 6), "\n")
cat("  lambda        =", round(my_vg$lambda, 6), "\n")
cat("  n             =", round(my_vg$n, 6), "\n")
cat("  Theta_resid   =", round(my_vg$theta_res, 6), "\n")
cat("  Sand (derived)=", round(my_vg$sand, 2), "%\n")

# Retention curve
rc <- vg_retention_curve(my_vg)
cat("\n  Water retention curve (first 10 rows):\n")
print(head(rc, 10))

# ---- PHOSPHORUS: change P-Bray and bulk density ----
cat("\n--- Phosphorus with custom inputs ---\n")
my_pbray   <- 5.38      # P-Bray in mg/kg     (change this)
my_bd      <- 1.37      # bulk density g/cm3   (change this)
my_method  <- 1         # 1 = P-Bray, 2 = P-water

# Soil type coefficients: pick from the table or set your own
# Available types: 1-4 = custom, 5 = Light Clay, 6 = Basin Clay,
#                  7 = Light Sand, 8 = Sitiung1
# To see all types, run: print(phos_soil_types)
my_soil <- phos_soil_types[phos_soil_types$id == 8, ]  # change the id
cat("  Using soil type:", my_soil$name, "\n")
cat("  SorbMax1=", my_soil$SorbMax1, " SorbMax2=", my_soil$SorbMax2,
    " SorbAff1=", my_soil$SorbAff1, " SorbAff2=", my_soil$SorbAff2, "\n")

# Or set completely custom coefficients:
# my_soil <- list(SorbMax1=10.26, SorbMax2=0.08, SorbAff1=1555, SorbAff2=3633.6, ThetaSat=0.4)

conv <- pbray_conversion(my_method)
init <- n_pstinit(my_pbray, my_bd, my_soil, conv)
bounds <- pst_bounds(my_soil)
cat("\n  N_PStInit  =", format(init, digits = 10), "\n")
cat("  PStSMin    =", format(bounds$PStSMin, digits = 10), "\n")
cat("  PStSMax    =", format(bounds$PStSMax, digits = 10), "\n")

# Try multiple P-Bray values at once to see the effect:
cat("\n  N_PStInit at different P-Bray values (BD =", my_bd, "):\n")
for (pb in c(2, 5, 10, 20, 50, 100)) {
  val <- n_pstinit(pb, my_bd, my_soil, conv)
  cat(sprintf("    P-Bray = %5.1f mg/kg  ->  N_PStInit = %.6f\n", pb, val))
}

# Try different bulk densities with fixed P-Bray:
cat("\n  N_PStInit at different bulk densities (P-Bray =", my_pbray, "):\n")
for (b in c(1.0, 1.2, 1.4, 1.6, 1.8)) {
  val <- n_pstinit(my_pbray, b, my_soil, conv)
  cat(sprintf("    BD = %.1f g/cm3  ->  N_PStInit = %.6f\n", b, val))
}
