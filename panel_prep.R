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
## Load
## ---------------------------------------------------------------------------
## Handles one large stacked .dta or several per-period files. Filenames vary
## (OCE_CallReport_20260331.dta, OCE_CallReport_2025q3.dta, ...) so the pattern
## is deliberately permissive and the error lists what is actually present.
##
## MEMORY. A 4-5 GB Stata file will not read whole on a 32 GB workstation:
## Stata stores compactly, R expands every numeric to an 8-byte double, so peak
## usage runs several times the file size. The fix is to read only the columns
## the study uses -- roughly 45 of 275, which cuts the footprint by most of an
## order of magnitude. KEEP_COLS below is that list.

FILE_PATTERN <- "^OCE_CallReport_.*\\.dta$"

KEEP_COLS <- c(
  ## identifiers and period
  "cu_number","join_number","year","quarter","cycle_date","q_period",
  ## geography
  "state","state_code","county_code","city","zip_code_char5","smsa",
  "congdistrict","region",
  ## charter type and designations
  "cu_type","limited_inc","ismdi","shortform","foicu_impute","cecl",
  ## scale
  "assets_tot","assets_avg","members","members_pot","fte",
  ## balance sheet
  "lns_tot","dep_tot","networth","networth_tot","pcanetworth","subdebt",
  ## lending detail (Q3 / ag work)
  "lns_mbl","lns_mbl_part723","lns_comm","lns_re_1_tot","lns_auto","lns_cc",
  ## performance
  "roa","netmarg","netintmrg","costfds","yldavgloans",
  ## credit risk
  "dq_rate","dq_mbl","dq_comm","chg_tot_ratio","netchgoffs",
  ## merger / exit
  "outcome","reason","acquiredcu","acquiredcu_ct","acquiredcu_ytd",
  "join_number_pointer"
)

REQUIRED_COLS <- c("cu_number","year","quarter","assets_tot","networth","roa",
                   "members","lns_tot","state_code","county_code","cu_type",
                   "limited_inc","ismdi")

## What is in the file, without reading it. Run this first on a new build.
peek_dta <- function(file) {
  hdr <- haven::read_dta(file, n_max = 0)
  cat("\nfile   : ", basename(file), "\n", sep = "")
  cat("size   : ", round(file.size(file) / 1e9, 2), " GB\n", sep = "")
  cat("columns: ", ncol(hdr), "\n", sep = "")
  miss <- setdiff(REQUIRED_COLS, names(hdr))
  cat("required columns missing: ", if (length(miss)) paste(miss, collapse = ", ") else "none", "\n", sep = "")
  cat("of ", length(KEEP_COLS), " requested columns, ",
      length(intersect(KEEP_COLS, names(hdr))), " are present\n", sep = "")
  cat("not found (harmless if unused): ",
      paste(setdiff(KEEP_COLS, names(hdr)), collapse = ", "), "\n\n", sep = "")
  invisible(names(hdr))
}

.read_dta_cols <- function(file, keep, n_max = Inf) {
  ## col_select keeps peak memory down by orders of magnitude on wide files
  out <- try(haven::read_dta(file, col_select = tidyselect::all_of(keep), n_max = n_max),
             silent = TRUE)
  if (inherits(out, "try-error")) {
    warning("col_select failed; falling back to a full read. This may exhaust RAM ",
            "on a large file.", call. = FALSE)
    out <- haven::read_dta(file, n_max = n_max)
    out <- out[, intersect(keep, names(out)), drop = FALSE]
  }
  .strip_haven(out)
}

load_panel <- function(dir, cache = file.path(dir, "_stacked_panel.rds"),
                       refresh = FALSE, n_max = Inf, keep = KEEP_COLS) {

  if (file.exists(cache) && !refresh && is.infinite(n_max)) {
    message("Reading cached panel: ", cache); return(readRDS(cache))
  }

  f <- list.files(dir, pattern = FILE_PATTERN, full.names = TRUE)
  if (!length(f)) {
    all_dta <- list.files(dir, pattern = "\\.dta$", ignore.case = TRUE)
    stop("No files matching ", FILE_PATTERN, " in:\n  ", dir, "\n",
         if (length(all_dta))
           paste0("\n.dta files that ARE there:\n  ", paste(all_dta, collapse = "\n  "),
                  "\n\nEdit FILE_PATTERN in panel_prep.R to match.")
         else "\nNo .dta files at all in that folder -- check the path.",
         call. = FALSE)
  }

  message("Found ", length(f), " file(s), ",
          round(sum(file.size(f)) / 1e9, 2), " GB total")
  avail <- names(haven::read_dta(f[1], n_max = 0))
  keep  <- intersect(keep, avail)
  miss  <- setdiff(REQUIRED_COLS, avail)
  if (length(miss))
    stop("Required columns absent from the file: ", paste(miss, collapse = ", "),
         "\n  Run peek_dta(\"", f[1], "\") to see what is available.", call. = FALSE)
  message("Reading ", length(keep), " of ", length(avail), " columns",
          if (is.finite(n_max)) paste0(", first ", n_max, " rows") else "")

  lst <- vector("list", length(f))
  for (i in seq_along(f)) {
    lst[[i]] <- .read_dta_cols(f[i], keep, n_max)
    message("  ", i, "/", length(f), "  (", nrow(lst[[i]]), " rows)")
  }

  if (length(f) > 1) {
    cls <- rbindlist(lapply(seq_along(lst), function(i)
      data.table(col = names(lst[[i]]),
                 cls = vapply(lst[[i]], function(v) class(v)[1], character(1)))))
    conf <- cls[, .(types = paste(sort(unique(cls)), collapse = "/"),
                    n_types = uniqueN(cls)), by = col][n_types > 1]
    if (nrow(conf)) {
      message("\nColumns with inconsistent types across files (harmonizing to character):")
      print(head(conf[order(-n_types)], 20))
      fwrite(conf, file.path(dir, "_type_conflicts.csv"))
      for (i in seq_along(lst))
        lst[[i]][, (intersect(conf$col, names(lst[[i]]))) :=
                   lapply(.SD, as.character), .SDcols = intersect(conf$col, names(lst[[i]]))]
    }
  }

  out <- rbindlist(lst, fill = TRUE, use.names = TRUE)
  rm(lst); gc()

  num <- intersect(NUMERIC_FIELDS, names(out))
  out[, (num) := lapply(.SD, function(v) suppressWarnings(as.numeric(as.character(v)))),
      .SDcols = num]
  bad <- out[, vapply(.SD, function(v) mean(is.na(v)), numeric(1)), .SDcols = num]
  if (any(bad > 0.5)) {
    sparse <- names(bad)[bad > 0.5]
    warning("Over half missing after numeric coercion: ",
            paste(sparse, collapse = ", "), call. = FALSE)
    ## Usually a SCHEDULE CHANGE, not corrupt data: a field that did not exist
    ## before a Call Report revision is missing for every earlier year. Print
    ## coverage by year so the distinction is visible rather than assumed.
    cat("\nNon-missing share by year for sparse fields:\n")
    print(dcast(melt(out[, c("year", sparse), with = FALSE], id.vars = "year")[
                , .(pct = round(100 * mean(!is.na(value)), 0)), by = .(year, variable)],
                year ~ variable, value.var = "pct"))
    cat("A clean 0-to-100 step at one year is a schedule break -- record it for Q9.\n\n")
  }

  if (is.infinite(n_max)) {
    saveRDS(out, cache); message("Cached to ", cache)
  }
  message("Loaded ", nrow(out), " CU-quarters, ", ncol(out), " columns, ",
          round(as.numeric(object.size(out)) / 1e9, 2), " GB in memory")
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
                       refresh    = FALSE,
                       n_max      = Inf) {  # set e.g. 50000 for a smoke test

  cr <- load_panel(panel_dir, refresh = refresh, n_max = n_max)
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

  ## Connecticut: county -> planning-region change would otherwise drop the
  ## entire state. Resolved on the evidence, not by a fabricated crosswalk.
  cr  <- resolve_ct(cr, uic$u2013, uic$u2024)

  cr[, rural := rural_2024]

  diagnose_unmatched(cr)
  unmatched <- cr[is.na(rural), .N]
  if (unmatched) {
    message("Dropping ", unmatched, " CU-quarters (",
            round(100 * unmatched / nrow(cr), 2), "%) with no rural classification")
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
