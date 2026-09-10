# ============================================================
#  phosphorus.R  —  Phosphorus sorption / N_PStParam (Phase 5)
#
#  Translated from Wanulcas.xlsm "Phosphorus" sheet.
#  The engine variable N_PStParam is built from a dual Langmuir
#  sorption isotherm. Users input P-Bray (mg/kg) per layer/zone;
#  the server converts to P-solution (mg/cm3) and computes the
#  sorption curve and initial P stocks.
#  The engine (wanulcas.R) is NOT touched.
# ============================================================

# --- Soil-type coefficient lookup table ---
# 8 predefined soil types: SorbMax1, SorbMax2, SorbAff1, SorbAff2, ThetaSat
phos_soil_types <- data.frame(
  id   = 1:13,
  name = c("Your 1st","Your 2nd","Your 3rd","Your 4th",
           "Light Clay","Basin Clay","Light Sand","Sitiung1",
           "R.Bujang U","Baganding","LampungBMSF","Gajrug","Sepungur"),
  SorbMax1  = c(1.859, 2.1, 3.2, 4.3, 0.087, 0.15, 0.16, 4.71,
                9.9, 1.8, 4.51, 10.26, 1.859),
  SorbMax2  = c(0, 0, 0, 0, 0.18, 0.49, 0.91, 1.95,
                0.02, 0.08, 0.05, 0.08, 0),
  SorbAff1  = c(2010, 2500, 3000, 3200, 5000, 16000, 500, 523,
                700, 463, 565, 1555, 2010),
  SorbAff2  = c(0, 0, 0, 0, 20, 130, 8.5, 666.4,
                1884, 550, 725.1, 3633.6, 0),
  ThetaSat  = rep(0.4, 13),
  stringsAsFactors = FALSE
)

# --- P-Bray to P-solution conversion ---
# method 1: P-Bray  -> divide by  10, multiply by 0.02
# method 2: P-water -> divide by  60, multiply by 1.0
# formula: P_solution = P_Bray * BD / (1000 * divisor) ... but the xlsm
# uses AN7=divisor, AN8=multiplier in the Langmuir formula directly.
# We keep the xlsm's exact structure.
pbray_conversion <- function(method = 1) {
  if (method == 1) list(divisor = 10, multiplier = 0.02, label = "P-Bray")
  else             list(divisor = 60, multiplier = 1.0,  label = "P-water")
}

# --- Dual Langmuir sorption isotherm ---
# P_mobile(c) = SorbMax1*SorbAff1*c/(1+SorbAff1*c) +
#               SorbMax2*SorbAff2*c/(1+SorbAff2*c) + c*ThetaSat
# where c = P concentration in soil solution (mg cm-3)
langmuir_pmobile <- function(c, sm1, sm2, sa1, sa2, theta_sat) {
  sm1*sa1*c/(1+sa1*c) + sm2*sa2*c/(1+sa2*c) + c*theta_sat
}

# Ka (apparent partition coefficient) = delta(P_mobile)/delta(c)
langmuir_ka <- function(c, sm1, sm2, sa1, sa2, theta_sat) {
  # finite difference over the standard concentration grid
  conc <- p_conc_grid()
  pm <- langmuir_pmobile(conc, sm1, sm2, sa1, sa2, theta_sat)
  ka <- diff(pm) / diff(conc)
  list(conc = conc[-1], ka = ka, pmobile = pm)
}

# Standard concentration grid (A92:A143 in xlsm) — 52 values
p_conc_grid <- function() {
  c(0, 3e-08, 1e-07, 5e-07, 1e-06, 5e-06, 1e-05, 3e-05, 6e-05, 9e-05,
    1e-04, 1.2e-04, 1.4e-04, 1.6e-04, 1.8e-04, 2e-04, 2.2e-04, 2.4e-04,
    2.6e-04, 2.8e-04, 3e-04, 3.5e-04, 4e-04, 4.5e-04, 5e-04, 5.5e-04,
    6e-04, 6.5e-04, 7e-04, 7.5e-04, 8e-04, 8.5e-04, 9e-04, 9.5e-04,
    1.05e-03, 1.15e-03, 1.25e-03, 1.35e-03, 1.45e-03, 1.55e-03, 1.65e-03,
    1.75e-03, 1.85e-03, 1.95e-03, 2.05e-03, 2.15e-03, 2.25e-03, 2.35e-03,
    2.45e-03, 2.55e-03, 2.65e-03, 2.75e-03, 2.85e-03)
}

# --- N_PStInit: initial P stock per layer×zone ---
# formula: PBray * BD / (1000 * divisor) *
#   (SorbMax1*SorbAff1*multiplier/(1+SorbAff1*multiplier*PBray*BD/(1000*divisor)) +
#    SorbMax2*SorbAff2*multiplier/(1+SorbAff2*multiplier*PBray*BD/(1000*divisor)))
n_pstinit <- function(pbray, bd, coefs, conv) {
  x <- pbray * bd / (1000 * conv$divisor)
  m <- conv$multiplier
  sm1 <- coefs$SorbMax1; sa1 <- coefs$SorbAff1
  sm2 <- coefs$SorbMax2; sa2 <- coefs$SorbAff2
  x * (sm1*sa1*m/(1+sa1*m*x) + sm2*sa2*m/(1+sa2*m*x))
}

# --- PStSMin / PStSMax per layer ---
pst_bounds <- function(coefs) {
  conc <- p_conc_grid()
  pm <- langmuir_pmobile(conc, coefs$SorbMax1, coefs$SorbMax2,
                         coefs$SorbAff1, coefs$SorbAff2, coefs$ThetaSat)
  list(PStSMin = pm[2], PStSMax = pm[length(pm)])
  # pm[2] = value at first non-zero concentration (3e-08)
  # pm[length] = value at max concentration (0.00285)
}

# --- Full N_PStParam builder for 4 layers × 4 zones ---
# pbray_mat: 4×4 matrix (rows=layers, cols=zones), mg/kg
# soil_types: integer vector length 4 (one per layer)
# bd: numeric vector length 4 (bulk density per layer)
# method: 1=P-Bray, 2=P-water
build_n_pstparam <- function(pbray_mat, soil_types, bd, method = 1) {
  conv <- pbray_conversion(method)
  nlayer <- 4; nzone <- 4
  init <- matrix(0, nlayer, nzone)
  psmin <- numeric(nlayer)
  psmax <- numeric(nlayer)
  ka_list <- vector("list", nlayer)
  
  for (lay in 1:nlayer) {
    coefs <- phos_soil_types[phos_soil_types$id == soil_types[lay], ]
    if (nrow(coefs) == 0) coefs <- phos_soil_types[8, ]  # fallback
    for (z in 1:nzone) {
      init[lay, z] <- n_pstinit(pbray_mat[lay, z], bd[lay], coefs, conv)
    }
    bounds <- pst_bounds(coefs)
    psmin[lay] <- bounds$PStSMin
    psmax[lay] <- bounds$PStSMax
    ka_list[[lay]] <- langmuir_ka(p_conc_grid(), coefs$SorbMax1, coefs$SorbMax2,
                                   coefs$SorbAff1, coefs$SorbAff2, coefs$ThetaSat)
  }
  
  list(N_PStInit = init, PStSMin = psmin, PStSMax = psmax, Ka = ka_list)
}
