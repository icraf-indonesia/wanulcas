# ============================================================
#  crash_guard.R  —  server-side robustness helpers
#  Purpose: a bad, blank, or "undefined" parameter must NEVER
#           terminate the Shiny session. The app stays alive and
#           reports the problem instead of greying out.
#  NOTE: nothing here touches the wanulcas.R engine (backbone).
#  Place this file in R/ and source it from global.R.
# ============================================================

# Replace NA / NULL / non-finite scalar parameters with their model
# defaults, so an "undefined" input can never reach the engine.
#   vars     : named list of scalar parameters (from rv_var)
#   defaults : wanulcas_params_def$vars
sanitize_scalar_vars <- function(vars, defaults) {           # crashguard
  for (nm in names(vars)) {                                  # crashguard
    v <- vars[[nm]]                                          # crashguard
    bad <- is.null(v) || length(v) == 0 ||                   # crashguard
           (is.numeric(v) && !all(is.finite(v))) ||          # crashguard
           (length(v) >= 1 && is.na(v[1]))                   # crashguard
    if (isTRUE(bad) && !is.null(defaults[[nm]])) {           # crashguard
      vars[[nm]] <- defaults[[nm]]                           # crashguard
    }                                                        # crashguard
  }                                                          # crashguard
  vars                                                       # crashguard
}                                                            # crashguard

# Run the engine but never propagate an error into the reactive graph.
# On success returns the normal wanulcas output object.
# On failure returns list(.error = <message>) — a marker the server
# detects (see is_sim_error) and turns into a user alert.
safe_run_wanulcas <- function(...) {                         # crashguard
  tryCatch(                                                  # crashguard
    run_wanulcas(...),                                       # crashguard
    error = function(e) {                                    # crashguard
      msg <- conditionMessage(e)                             # crashguard
      message("[WaNuLCAS] simulation stopped: ", msg)        # crashguard
      list(.error = msg)                                     # crashguard
    }                                                        # crashguard
  )                                                          # crashguard
}                                                            # crashguard

# TRUE when a run returned the error marker rather than real output.
is_sim_error <- function(x) {                                # crashguard
  is.list(x) && !is.null(x[[".error"]])                      # crashguard
}                                                            # crashguard
