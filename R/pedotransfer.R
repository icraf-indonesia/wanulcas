# ============================================================
#  pedotransfer.R  —  Soil-water pedotransfer functions
#
#  Methods:
#    method 1 = Wosten et al. 1998        (temperate soils)
#    method 2 = Tomasella & Hodnett 2002  (tropical soils)
#
#  These compute van Genuchten hydraulic parameters from soil texture.
#  The engine (wanulcas.R) is NOT touched: the results are meant to be
#  used in the server to build the water-retention inputs the engine reads.
# ============================================================

# clay, silt, orgC : percent (%). sand is derived = 100 - clay - silt
# bd    : bulk density (g cm-3)
# top   : 1 = topsoil, 0 = subsoil
# cec   : cmol kg-1  (Tomasella-Hodnett only)
# ph    :            (Tomasella-Hodnett only)
# method: 1 = Wosten, 2 = Tomasella-Hodnett
pedotransfer_vg <- function(clay, silt, orgC, bd,
                            top = 0, cec = 10, ph = 5.5, method = 2) {
  sand <- 100 - clay - silt
  om   <- 1.7 * orgC          # % organic matter (C31 = 1.7 * OrgC)
  ln <- log; ex <- exp

  if (method == 1) {
    # ---- Wosten et al. 1998 (temperate) ----
    theta_sat <- (0.7919 + 0.001691*clay - 0.29619*bd - 0.000001491*silt^2 +
                  0.0000821*om^2 + 0.02427/clay + 0.01113/silt + 0.01472*ln(silt) -
                  0.0000733*om*clay - 0.000619*bd*clay - 0.001183*bd*om -
                  0.0001664*top*silt)
    ksat <- ex(7.755 + 0.0352*silt + 0.93*top - 0.967*bd^2 - 0.000484*clay^2 -
               0.000322*silt^2 + 0.001/silt - 0.0748/om - 0.643*ln(silt) -
               0.01398*bd*clay - 0.1673*bd*om + 0.02986*top*clay - 0.03305*top*silt)
    alpha <- ex(-14.96 + 0.03135*clay + 0.0351*silt + 0.646*om + 15.29*bd -
                0.192*top - 4.671*bd^2 - 0.000781*clay^2 - 0.00687*om^2 +
                0.0449/om + 0.0663*ln(silt) + 0.1482*ln(om) - 0.04546*bd*silt -
                0.4852*bd*om + 0.00673*top*clay)
    e <- ex(0.0202 + 0.0006193*clay^2 - 0.001136*om^2 - 0.2316*ln(om) -
            0.03544*bd*clay + 0.00283*bd*silt + 0.0488*bd*om)
    lambda <- ((10*e) - 10) / (1 + e)
    n <- ex(-25.23 - 0.02195*clay + 0.0074*silt - 0.194*om + 45.5*bd - 7.24*bd^2 +
            0.0003658*clay^2 + 0.002885*om^2 - 12.81/bd - 0.1524/silt - 0.01958/om -
            0.2876*ln(silt) - 0.0709*ln(om) - 44.6*ln(bd) - 0.02264*bd*clay +
            0.0896*bd*om + 0.00718*top*clay) + 1
    theta_res <- 0
  } else {
    # ---- Tomasella & Hodnett 2002 (tropical) ----
    theta_sat <- (81.799 + 0.099*clay - 31.42*bd + 0.018*cec + 0.451*ph -
                  0.0005*sand*clay) / 100
    ksat <- ex(7.755 + 0.0352*silt + 0.93*top - 0.967*bd^2 - 0.000484*clay^2 -
               0.000322*silt^2 + 0.001/silt - 0.0748/om - 0.643*ln(silt) -
               0.01398*bd*clay - 0.1673*bd*om + 0.02986*top*clay - 0.03305*top*silt)
    alpha <- ex((-2.294 - 3.526*silt + 2.44*orgC - 0.076*cec - 11.331*ph +
                 0.019*silt^2) / 100) * 10
    e <- ex(0.0202 + 0.0006193*clay^2 - 0.001136*om^2 - 0.2316*ln(om) -
            0.03544*bd*clay + 0.00283*bd*silt + 0.0488*bd*om)
    lambda <- ((10*e) - 10) / (1 + e)
    n <- ex((62.986 - 0.833*clay - 0.592*orgC + 0.593*ph + 0.007*clay^2 -
             0.014*silt*sand) / 100)
    theta_res <- (22.733 - 0.164*sand + 0.235*cec - 0.831*ph + 0.0018*clay^2 +
                  0.0026*sand*clay) / 100
  }

  list(sand = sand, theta_sat = theta_sat, theta_res = theta_res,
       ksat = ksat, alpha = alpha, lambda = lambda, n = n)
}

# van Genuchten water-retention curve: theta as a function of matric head h (cm).
#   theta(h) = theta_r + (theta_s - theta_r) / (1 + (alpha*h)^n)^(1 - 1/n)
# pF = log10(|h| in cm). Returns a data.frame(pF, h_cm, theta).
vg_retention_curve <- function(vg, pF = seq(0, 4.2, by = 0.1)) {
  h <- 10^pF                                  # matric head (cm)
  m <- 1 - 1/vg$n
  theta <- vg$theta_res +
    (vg$theta_sat - vg$theta_res) / (1 + (vg$alpha * h)^vg$n)^m
  data.frame(pF = pF, h_cm = h, theta = theta)
}

# Convenience: theta at a given pF (e.g. field capacity ~ pF 2.0-2.5, wilting pF 4.2)
vg_theta_at_pF <- function(vg, pF) {
  h <- 10^pF; m <- 1 - 1/vg$n
  vg$theta_res + (vg$theta_sat - vg$theta_res) / (1 + (vg$alpha * h)^vg$n)^m
}

# ============================================================
#  UI + Server coupling helpers
# ============================================================

# Build the water retention graph data in the format rv_graph expects:
# a 2-column data.frame (x_val, y_val) for each subvar.
# Returns a named list of data.frames keyed by graph_subvar id.
pedotransfer_to_graph <- function(vg, graph_subvars) {
  # The water retention curve: theta(pF) for each layer
  # W_PhiTheta: x = theta, y = pF (pressure head in cm as phi = 10^pF)
  rc <- vg_retention_curve(vg, pF = seq(0, 4.2, by = 0.1))
  
  # For now returns the VG parameters as a summary.
  # The actual graph update requires matching the exact x-grid the engine expects,
  # which varies per graph variable. That coupling is done in the server observer.
  list(
    theta_sat = vg$theta_sat,
    ksat      = vg$ksat,
    alpha     = vg$alpha,
    n         = vg$n,
    lambda    = vg$lambda,
    theta_res = vg$theta_res,
    curve     = rc
  )
}

# ============================================================
#  Zonelayer table reshape helpers
#  Displays 16-value zonelayer arrays as a 4-row (Layer) × 4-col (Zone)
#  table instead of a flat 16-row list. Visual only — the underlying
#  data stays in the same flat format the engine expects.
# ============================================================

# Flat zonelayer vector (16 values) → 4×4 data.frame (rows=layers, cols=zones)
zonelayer_to_matrix <- function(vals, var_name = "value") {
  # ordering: zone varies fastest within each layer block
  mat <- matrix(vals, nrow = 4, ncol = 4, byrow = TRUE)
  df <- as.data.frame(mat)
  names(df) <- paste0("Zone_", 1:4)
  df <- cbind(Layer = 1:4, df)
  df
}

# 4×4 data.frame (from edited table) → flat 16-value vector
matrix_to_zonelayer <- function(df) {
  # drop Layer column, convert back to flat vector (row-major = layer blocks)
  mat <- as.matrix(df[, -1, drop = FALSE])
  as.vector(t(mat))  # zone varies fastest
}

# ============================================================
#  Field capacity based on critical K value
#  Mirrors the Pedotransfer sheet: find theta where K(theta)=K_crit
#  using the van Genuchten retention + Mualem conductivity curves.
# ============================================================
vg_field_capacity_kcrit <- function(vg, k_crit = 0.1, med_sand = 290) {
  ts <- vg$theta_sat; tr <- vg$theta_res
  alpha <- vg$alpha; n <- vg$n; lambda <- vg$lambda; ksat <- vg$ksat
  m <- 1 - 1/n
  # pF grid 0..6 step 0.1 (matches xlsm rows 81..142; row 81 = pF 0)
  pF <- seq(0, 6, by = 0.1)
  P  <- -(10^pF)                        # pressure head (cm), negative
  ah <- abs(alpha * P)
  theta <- tr + (ts - tr) / (1 + ah^n)^m
  # Mualem-van Genuchten conductivity.
  # NOTE: the WaNuLCAS Pedotransfer sheet has a fill-down quirk: from its row 82
  # onward the conductivity denominator uses theta(pF=0) as the exponent base
  # instead of n.
  theta_pF0 <- theta[1]                 # xlsm D81
  num <- ((1 + ah^n)^m - ah^(n - 1))^2
  base <- ifelse(seq_along(pF) == 1, n, theta_pF0)
  den <- (1 + ah^base)^(m * (lambda + 2))
  Kval <- ksat * num / den
  logK <- log10(Kval)
  logKcrit <- log10(k_crit)
  dif <- logK - logKcrit
  # xlsm: interpolate theta at dif=0 between the negative dif closest to 0
  # (maxneg) and the positive dif closest to 0 (minpos).
  neg <- dif[dif <= 0]; pos <- dif[dif >= 0]
  if (length(neg) == 0 || length(pos) == 0) return(theta[which.min(abs(dif))])
  dif_maxneg <- max(neg); dif_minpos <- min(pos)
  theta_maxneg <- theta[which(dif == dif_maxneg)[1]]
  theta_minpos <- theta[which(dif == dif_minpos)[1]]
  if (theta_maxneg == theta_minpos) return(theta_maxneg)
  theta_maxneg - dif_maxneg * (theta_minpos - theta_maxneg) /
                              (dif_minpos - dif_maxneg)
}
