###############################################################################
# universe.R  --  THE ONE PLACE THE STUDY'S UNIVERSE IS DEFINED
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# Sourced by 3_callreport.R, M2_callreport.R, rq_load.R, maps_legacy_shim.R and
# every script that exports or quotes a number. Change the definition HERE and
# nowhere else.
#
# Why this file exists: on 7 Oct 2026 a list of "468 rural credit unions" sent
# to CURE contained five privately insured (type 3) institutions. The type
# filter had been written into the master build but not into the pipeline
# actually being run. A definition that lives in one script and is applied in
# another is not a definition; it is a hope.
###############################################################################

## ---- the definition ---------------------------------------------------------
UNIVERSE <- list(
  charter_types = c(1L, 2L),   # 1 = federal CU; 2 = federally insured state CU. 3 = privately insured: OUT.
  first_year    = 2010L,
  asset_floor   = 0,           # NO floor on the universe. est_sample (>= $1m) is for regressions only.
  est_floor     = 1e6
)

## ---- apply it ---------------------------------------------------------------
## Works on numeric or character cu_type (haven can return either).
apply_universe <- function(cr) {
  ct <- suppressWarnings(as.integer(as.character(cr$cu_type)))
  n0 <- nrow(cr); k0 <- length(unique(cr$cu_number))
  cr <- cr[ct %in% UNIVERSE$charter_types & year >= UNIVERSE$first_year & assets_tot >= UNIVERSE$asset_floor]
  cr[, est_sample := assets_tot >= UNIVERSE$est_floor]
  cat(sprintf("Universe applied: charter types %s, year >= %d. Dropped %s rows / %d credit unions.\n",
              paste(UNIVERSE$charter_types, collapse = "+"), UNIVERSE$first_year,
              format(n0 - nrow(cr), big.mark = ","), k0 - length(unique(cr$cu_number))))
  cr
}

## ---- assert it --------------------------------------------------------------
## Call at the top of ANY script that produces a number, a map, a slide or a
## file. It refuses to continue if cr is not the universe. No exceptions, no
## soft mode.
assert_universe <- function(cr, where = "this script") {
  ct <- suppressWarnings(as.integer(as.character(cr$cu_type)))
  bad <- cr[!ct %in% UNIVERSE$charter_types, unique(cu_number)]
  if (length(bad)) stop(sprintf(
    "UNIVERSE VIOLATION in %s: cr contains %d credit union(s) outside charter types %s (e.g. cu_number %s). ",
    where, length(bad), paste(UNIVERSE$charter_types, collapse = "/"), paste(head(bad, 5), collapse = ", ")),
    "Run cr <- apply_universe(cr) first.", call. = FALSE)
  if (any(cr$year < UNIVERSE$first_year)) stop("UNIVERSE VIOLATION in ", where, ": rows before ", UNIVERSE$first_year, call. = FALSE)
  if (!"est_sample" %in% names(cr)) stop("UNIVERSE VIOLATION in ", where, ": est_sample flag missing -- cr did not pass through apply_universe()", call. = FALSE)
  cat(sprintf("  [ok] universe gate (%s): %d credit unions, charter types %s only\n",
              where, length(unique(cr$cu_number)), paste(sort(unique(ct)), collapse = "/")))
  invisible(TRUE)
}

## ---- and the same for sites, so no map or office count can include them -----
assert_sites_universe <- function(sites, cr, where = "this script") {
  if (!"in_cr" %in% names(sites)) stop("UNIVERSE VIOLATION in ", where, ": sites lacks in_cr", call. = FALSE)
  stray <- sites[in_cr == TRUE & !cu_number %in% unique(cr$cu_number), uniqueN(cu_number)]
  if (stray) stop("UNIVERSE VIOLATION in ", where, ": ", stray, " credit unions flagged in_cr are not in cr", call. = FALSE)
  cat(sprintf("  [ok] sites gate (%s): in_cr matches cr\n", where)); invisible(TRUE)
}
