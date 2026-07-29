###############################################################################
# panel_prep.R
#
# Shared preparation for the ROAD Act Sec. 909 study. Loads the Stata Call
# Report panel, attaches the statutory rural classification, applies sample
# filters, and constructs outcomes.
#
# Sourced by:  exhibits_descriptive.R, q1_rural_vs_small.R, rural_maps.R
#
# Usage:
#     source("panel_prep.R")
#     P  <- prep_panel(panel_dir = "path/to/dta")
#     cr <- P$cr; uic <- P$uic
#
# The panel is named `cr`, NOT `dt`. `dt` masks stats::dt and produces
# "object of type 'closure' is not subsettable" whenever the object is absent.
###############################################################################

library(data.table)
source("rural_definition.R")

if (!requireNamespace("haven", quietly = TRUE))
  stop("Reading .dta requires haven. Run: install.packages(\"haven\")", call. = FALSE)

## ---------------------------------------------------------------------------
## Stata helpers
## ---------------------------------------------------------------------------
## Inspect Stata value labels BEFORE they are zapped -- use this to confirm the
## coding of cu_type, limited_inc, ismdi, shortform.
stata_value_labels <- function(file, vars = c("cu_type","limited_inc","ismdi","shortform")) {
  x <- haven::read_dta(file, n_max = 100)
  for (v in intersect(vars, names(x))) {
    lb <- attr(x[[v]], "labels")
    cat("\n", v, " (", class(x[[v]])[1], ")\n", sep = "")
    if (is.null(lb)) cat("  no value labels; observed: ",
                         paste(head(sort(unique(x[[v]])), 10), collapse = ", "), "\n", sep = "")
    else print(lb)
  }
  invisible(NULL)
}

## haven_labelled columns break median(), cut(), and factor() quietly.
.strip_haven <- function(x) {
  x <- haven::zap_labels(x); x <- haven::zap_label(x)
  x <- haven::zap_formats(x); x <- haven::zap_widths(x)
  setDT(x); x[]
}

NUMERIC_FIELDS <- c("assets_tot","assets_avg","networth","networth_tot","pcanetworth",
                    "roa","members","members_pot","lns_tot","lns_mbl","lns_comm",
                    "dep_tot","fte","netmarg","netintmrg","costfds","yldavgloans",
                    "dq_rate","chg_tot_ratio","acquiredcu_ct",
                    "year","quarter","state_code","county_code")

## ---------------------------------------------------------------------------
## Load and stack
## ---------------------------------------------------------------------------
load_panel <- function(dir, cache = file.path(dir, "_stacked_panel.rds"), refresh = FALSE) {
  if (file.exists(cache) && !refresh) {
    message("Reading cached stacked panel: ", cache); return(readRDS(cache))
  }
  f <- list.files(dir, pattern = "OCE_CallReport_\\d{4}q[1-4]\\.dta$", full.names = TRUE)
  if (!length(f)) stop("No OCE_CallReport_*.dta files found in ", dir, call. = FALSE)
  message("Reading ", length(f), " Stata files (slow the first time only)")

  lst <- vector("list", length(f))
  for (i in seq_along(f)) {
    lst[[i]] <- .strip_haven(haven::read_dta(f[i]))
    if (i %% 10 == 0 || i == length(f)) message("  ", i, "/", length(f))
  }
  names(lst) <- basename(f)

  ## Type drift across quarters: a field stored as string one year and numeric
  ## another makes rbindlist fail or coerce silently.
  cls <- rbindlist(lapply(names(lst), function(nm)
    data.table(file = nm, col = names(lst[[nm]]),
               cls = vapply(lst[[nm]], function(v) class(v)[1], character(1)))))
  conf <- cls[, .(types = paste(sort(unique(cls)), collapse = "/"),
                  n_types = uniqueN(cls)), by = col][n_types > 1]
  if (nrow(conf)) {
    message("\nColumns with inconsistent types across quarters (harmonizing to character):")
    print(head(conf[order(-n_types)], 20))
    fwrite(conf, file.path(dir, "_type_conflicts.csv"))
    message("Full list written to _type_conflicts.csv -- these are also candidates ",
            "for the 2017/2022 schedule-break testing in Q9.")
    for (i in seq_along(lst))
      lst[[i]][, (intersect(conf$col, names(lst[[i]]))) :=
                 lapply(.SD, as.character), .SDcols = intersect(conf$col, names(lst[[i]]))]
  }

  out <- rbindlist(lst, fill = TRUE, use.names = TRUE)
  num <- intersect(NUMERIC_FIELDS, names(out))
  out[, (num) := lapply(.SD, function(v) suppressWarnings(as.numeric(as.character(v)))),
      .SDcols = num]
  bad <- out[, vapply(.SD, function(v) mean(is.na(v)), numeric(1)), .SDcols = num]
  if (any(bad > 0.5))
    warning("Over half missing after numeric coercion: ",
            paste(names(bad)[bad > 0.5], collapse = ", "), call. = FALSE)

  saveRDS(out, cache)
  message("Stacked panel cached to ", cache)
  out[]
}

## ---------------------------------------------------------------------------
## Full preparation
## ---------------------------------------------------------------------------
prep_panel <- function(panel_dir,
                       cache_dir  = "data/raw",
                       start_year = 2010,
                       min_assets = 1e6,
                       winsor     = c(0.01, 0.99),
                       drop_shortform = FALSE,
                       scale_pct  = NA,     # NA = auto-detect; or set 1 / 100
                       refresh    = FALSE) {

  cr <- load_panel(panel_dir, refresh = refresh)
  setDT(cr)

  req <- c("cu_number","year","quarter","assets_tot","networth","roa","members",
           "lns_tot","state_code","county_code","cu_type","limited_inc","ismdi")
  miss <- setdiff(req, names(cr))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)

  cr[, qidx := as.integer(year) * 4L + as.integer(quarter)]
  setorder(cr, cu_number, qidx)
  message("Panel: ", uniqueN(cr$cu_number), " credit unions, ",
          uniqueN(cr$qidx), " quarters, ", nrow(cr), " CU-quarters")

  ## --- rural classification ------------------------------------------------
  uic <- uic_setup(cache_dir = cache_dir)
  cr  <- build_fips(cr)
  check_join_hazards(cr)
  cr  <- harmonize_fips(cr, uic24 = uic$u2024)
  cr  <- attach_rural(cr, uic$u2024, label = "rural_2024")
  cr  <- attach_rural(cr, uic$u2013, label = "rural_2013")
  cr[, rural := rural_2024]

  unmatched <- cr[is.na(rural), .N]
  if (unmatched) {
    message("Dropping ", unmatched, " CU-quarters with no rural classification")
    print(cr[is.na(rural), .N, by = .(fips, state_code)][order(-N)][1:10])
    cr <- cr[!is.na(rural)]
  }

  ## --- filters and contaminants -------------------------------------------
  cr <- cr[year >= start_year & assets_tot >= min_assets]

  if ("acquiredcu_ct" %in% names(cr)) {
    cr[, acq := as.integer(!is.na(acquiredcu_ct) & acquiredcu_ct > 0)]
    cr[, acq_window := as.integer(frollsum(acq, 5, align = "right", fill = 0) > 0), by = cu_number]
  } else { cr[, acq_window := 0L]; warning("acquiredcu_ct not found", call. = FALSE) }

  ## Short-form status correlates with BOTH size and rurality, so dropping
  ## these biases exactly the comparison being made. Keep by default.
  if ("shortform" %in% names(cr)) {
    cr[, sf := as.integer(shortform %in% c(1, "1", "Y", TRUE))]
    message("Short-form share: overall ", round(100*mean(cr$sf, na.rm=TRUE), 1),
            "% | rural ", round(100*mean(cr[rural==1]$sf, na.rm=TRUE), 1),
            "% | non-rural ", round(100*mean(cr[rural==0]$sf, na.rm=TRUE), 1), "%")
    if (drop_shortform) cr <- cr[sf == 0]
  } else cr[, sf := 0L]

  if ("foicu_impute" %in% names(cr))
    cr <- cr[is.na(foicu_impute) | foicu_impute %in% c(0, "0", "N", FALSE)]

  cr[, cecl_flag := if ("cecl" %in% names(cr)) as.integer(cecl %in% c(1,"1","Y",TRUE)) else 0L]

  ## --- units ---------------------------------------------------------------
  med_nw <- cr[, median(networth, na.rm = TRUE)]
  if (is.na(scale_pct)) {
    scale_pct <- if (med_nw < 1) 100 else 1
    message("Units: median networth = ", round(med_nw, 4), " -> SCALE = ", scale_pct)
  }
  cr[, `:=`(networth_pct = networth * scale_pct, roa_pct = roa * scale_pct)]

  ## --- outcomes ------------------------------------------------------------
  lagq <- function(x, k, idx) x[match(idx - k, idx)]
  cr[, `:=`(assets_l4  = lagq(assets_tot, 4L, qidx),
            members_l4 = lagq(members,    4L, qidx),
            lns_l4     = lagq(lns_tot,    4L, qidx)), by = cu_number]
  cr[, `:=`(g_assets  = 100 * (assets_tot / assets_l4  - 1),
            g_members = 100 * (members    / members_l4 - 1),
            g_loans   = 100 * (lns_tot    / lns_l4     - 1))]

  wins <- function(x, p = winsor) { q <- quantile(x, p, na.rm = TRUE); pmin(pmax(x, q[1]), q[2]) }
  OUTCOMES <- c("g_assets","networth_pct","roa_pct","g_members","g_loans")
  cr[, (OUTCOMES) := lapply(.SD, wins), .SDcols = OUTCOMES]

  cr[, `:=`(ln_assets = log(assets_tot),
            lid = as.integer(limited_inc %in% c(1,"1","Y",TRUE)),
            mdi = as.integer(ismdi       %in% c(1,"1","Y",TRUE)),
            fed = as.integer(cu_type     %in% c(1,"1")),   # CONFIRM with stata_value_labels()
            qtr = factor(qidx),
            st  = factor(state_code))]

  cr[, first_q := min(qidx), by = cu_number]
  cr[, age_q := qidx - first_q]

  BANDS <- c(0, 10e6, 50e6, 100e6, 500e6, 1e9, Inf)
  BLAB  <- c("<$10M","$10-50M","$50-100M","$100-500M","$500M-1B",">$1B")
  cr[, band := cut(assets_tot, BANDS, labels = BLAB, right = FALSE)]

  list(cr = cr, uic = uic, outcomes = OUTCOMES, bands = BLAB, scale = scale_pct)
}
