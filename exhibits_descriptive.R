###############################################################################
# exhibits_descriptive.R
#
# EXECUTIVE EXHIBIT PACK -- Rural Credit Unions Study (ROAD Act Sec. 909)
#
# Eleven tables, in narrative order. Run BEFORE any econometrics: each answers
# a question a Board member or committee staffer would actually ask, and each
# is intended to survive into the report.
#
#   E1  Headline     -- the rural universe in one row
#   E2  Asymmetry    -- half the counties, a fraction of the people
#   E3  Size         -- rural CUs by asset band (sets up Q1)
#   E4  Trend        -- counts, members, assets by year, indexed
#   E5  Flows        -- entry and exit by year
#   E6  Performance  -- the three statutory outcomes, pooled
#   E7  Perf x size  -- the same outcomes within asset band (the Q1 teaser)
#   E8  Geography    -- states ranked by rural CU presence
#   E9  Designations -- LID / MDI / charter type
#   E10 MBL headroom -- distance to the statutory cap (the Q3 preview)
#   E11 Fragility    -- reclassification exposure
#
# PATHS come from 00_run.R. Run that first; do not set them here.
# Outputs: console, CSVs, and a formatted .xlsx workbook for circulation.
###############################################################################

library(data.table)

## ---------------------------------------------------------------------------
## 0. PATHS AND PANEL -- inherited from 00_run.R
## ---------------------------------------------------------------------------

need_vars <- c("CODE_DIR", "PANEL_DIR", "CACHE_DIR", "OUT_DIR")
absent <- need_vars[!vapply(need_vars, exists, logical(1))]
if (length(absent))
  stop("Missing path variable(s): ", paste(absent, collapse = ", "),
       "\n  Run 00_run.R first -- it sets CODE_DIR, PANEL_DIR, CACHE_DIR, OUT_DIR.",
       call. = FALSE)

EX_DIR <- file.path(OUT_DIR, "exhibits")
dir.create(EX_DIR, recursive = TRUE, showWarnings = FALSE)

## Reuse the prepared panel if it is already in the environment. prep_panel()
## is not cheap and 00_run.R has usually run it already.
if (exists("cr") && is.data.table(cr) && "rural" %in% names(cr)) {
  message("Using the prepared panel already in the environment (", nrow(cr), " CU-quarters)")
  if (!exists("uic")) stop("`cr` present but `uic` is not -- re-run prep_panel().", call. = FALSE)
} else {
  source(file.path(CODE_DIR, "panel_prep.R"))
  P   <- prep_panel(PANEL_DIR, cache_dir = CACHE_DIR)
  cr  <- P$cr
  uic <- P$uic
}

LATEST <- cr[, max(qidx)]
cur    <- cr[qidx == LATEST]
cat("\nLatest quarter in panel: ", cur[1, year], "Q", cur[1, quarter], "\n", sep = "")

EX <- list()

## ---------------------------------------------------------------------------
## E1. HEADLINE
## ---------------------------------------------------------------------------
EX$E1_headline <- cur[, .(
  credit_unions    = uniqueN(cu_number),
  members_mn       = sum(members,    na.rm = TRUE) / 1e6,
  assets_bn        = sum(assets_tot, na.rm = TRUE) / 1e9,
  median_assets_mn = median(assets_tot, na.rm = TRUE) / 1e6
), by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural"))]

tot <- EX$E1_headline[, lapply(.SD, sum), .SDcols = c("credit_unions","members_mn","assets_bn")]
EX$E1_headline[, `:=`(pct_of_cus     = round(100 * credit_unions / tot$credit_unions, 1),
                      pct_of_members = round(100 * members_mn    / tot$members_mn, 1),
                      pct_of_assets  = round(100 * assets_bn     / tot$assets_bn, 1))]
cat("\n=== E1. The rural credit union universe ===\n"); print(EX$E1_headline)

## ---------------------------------------------------------------------------
## E2. ASYMMETRY -- the framing number for the whole report
## ---------------------------------------------------------------------------
u24 <- uic$u2024[as.integer(substr(fips, 1, 2)) <= 56]
EX$E2_asymmetry <- data.table(
  measure = c("Counties", "Population (2020)", "Credit unions", "Members", "Assets"),
  rural = c(u24[rural == 1, .N],
            u24[rural == 1, sum(pop, na.rm = TRUE)],
            cur[rural == 1, uniqueN(cu_number)],
            cur[rural == 1, sum(members,    na.rm = TRUE)],
            cur[rural == 1, sum(assets_tot, na.rm = TRUE)]),
  total = c(u24[, .N],
            u24[, sum(pop, na.rm = TRUE)],
            cur[, uniqueN(cu_number)],
            cur[, sum(members,    na.rm = TRUE)],
            cur[, sum(assets_tot, na.rm = TRUE)]))
EX$E2_asymmetry[, rural_share_pct := round(100 * rural / total, 1)]
cat("\n=== E2. Rural share, by measure ===\n"); print(EX$E2_asymmetry)
cat("The gap between the county share and the population share is why rural\n")
cat("institutions are small. Say it aloud in any briefing.\n")

## ---------------------------------------------------------------------------
## E3. SIZE DISTRIBUTION
## ---------------------------------------------------------------------------
EX$E3_size <- dcast(cur[, .(n = uniqueN(cu_number)), by = .(band, rural)],
                    band ~ rural, value.var = "n", fill = 0)
setnames(EX$E3_size, c("0","1"), c("nonrural_n","rural_n"))
EX$E3_size[, `:=`(rural_pct    = round(100 * rural_n    / sum(rural_n), 1),
                  nonrural_pct = round(100 * nonrural_n / sum(nonrural_n), 1),
                  rural_share_of_band = round(100 * rural_n / (rural_n + nonrural_n), 1))]
cat("\n=== E3. Asset size distribution ===\n"); print(EX$E3_size)

## ---------------------------------------------------------------------------
## E4. TREND
## ---------------------------------------------------------------------------
yr <- cr[quarter == 4 | qidx == LATEST,
         .(credit_unions = uniqueN(cu_number),
           members_mn    = sum(members,    na.rm = TRUE) / 1e6,
           assets_bn     = sum(assets_tot, na.rm = TRUE) / 1e9),
         by = .(year, segment = fifelse(rural == 1L, "Rural", "Non-rural"))]
setorder(yr, segment, year)
yr[, `:=`(cu_index  = round(100 * credit_unions / first(credit_unions), 1),
          mem_index = round(100 * members_mn    / first(members_mn), 1),
          ast_index = round(100 * assets_bn     / first(assets_bn), 1)), by = segment]
EX$E4_trend <- yr
cat("\n=== E4. Charter count, indexed to first year = 100 ===\n")
print(dcast(yr, year ~ segment, value.var = "cu_index"))
cat("Tracking lines = consolidation, not rural decline. Say so if that is what\n")
cat("the data show.\n")

## ---------------------------------------------------------------------------
## E5. FLOWS
## ---------------------------------------------------------------------------
first_last <- cr[, .(first_q = min(qidx), last_q = max(qidx),
                     seg = fifelse(rural[which.max(qidx)] == 1L, "Rural", "Non-rural")),
                 by = cu_number]
p_end <- cr[, max(qidx)]; p_start <- cr[, min(qidx)]
exits   <- first_last[last_q  < p_end,   .(exits   = .N), by = .(year = last_q  %/% 4L, seg)]
entries <- first_last[first_q > p_start, .(entries = .N), by = .(year = first_q %/% 4L, seg)]
EX$E5_flows <- merge(entries, exits, by = c("year","seg"), all = TRUE)
setnafill(EX$E5_flows, fill = 0, cols = c("entries","exits"))
EX$E5_flows[, net := entries - exits]
cat("\n=== E5. Entries and exits by year ===\n"); print(EX$E5_flows[order(seg, year)])
cat("'exits' pools mergers, liquidations, and conversions -- split with the\n")
cat("outcome/reason fields before this goes in the report.\n")

## ---------------------------------------------------------------------------
## E6. PERFORMANCE
## ---------------------------------------------------------------------------
## networth is used throughout: pcanetworth was discontinued at the 2022Q1
## Call Report modernization and does not span the panel.
perf_vars <- intersect(c("g_assets","g_members","g_loans","networth_pct","roa_pct",
                         "netintmrg","dq_rate"), names(cr))
recent <- cr[qidx > LATEST - 4]
EX$E6_performance <- rbind(
  recent[, c(.(basis = "median (unweighted)"), lapply(.SD, median, na.rm = TRUE)),
         by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural")), .SDcols = perf_vars],
  recent[, c(.(basis = "mean (asset-weighted)"),
             lapply(.SD, function(x) weighted.mean(x, assets_tot, na.rm = TRUE))),
         by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural")), .SDcols = perf_vars])
cat("\n=== E6. Performance, trailing four quarters ===\n"); print(EX$E6_performance)

## ---------------------------------------------------------------------------
## E7. PERFORMANCE WITHIN SIZE BAND -- the most important table here
## ---------------------------------------------------------------------------
pbs <- recent[, lapply(.SD, median, na.rm = TRUE),
              by = .(band, rural), .SDcols = c("g_assets","networth_pct","roa_pct")]
EX$E7_perf_by_size <- dcast(melt(pbs, id.vars = c("band","rural")),
                            band + variable ~ rural, value.var = "value")
setnames(EX$E7_perf_by_size, c("0","1"), c("nonrural","rural"))
EX$E7_perf_by_size[, gap := round(rural - nonrural, 3)]
setorder(EX$E7_perf_by_size, variable, band)
cat("\n=== E7. Rural minus non-rural, WITHIN asset band ===\n"); print(EX$E7_perf_by_size)
cat("Compare `gap` across bands within each outcome. Shrinking toward zero =\n")
cat("a size story. Stable and non-zero = a rural story.\n")

## ---------------------------------------------------------------------------
## E8. GEOGRAPHY
## ---------------------------------------------------------------------------
EX$E8_states <- cur[, .(rural_cus = sum(rural == 1L), total_cus = .N,
                        rural_assets_bn = sum(assets_tot[rural == 1L], na.rm = TRUE) / 1e9),
                    by = .(state = state_code)][order(-rural_cus)]
EX$E8_states[, rural_share_pct := round(100 * rural_cus / total_cus, 1)]
cat("\n=== E8. Top 15 states by rural credit union count ===\n")
print(head(EX$E8_states, 15))

## ---------------------------------------------------------------------------
## E9. DESIGNATIONS -- and why they matter for Sec. 909(c)(2)
## ---------------------------------------------------------------------------
## Low-income designated credit unions are EXEMPT from the statutory member
## business lending cap, 12 U.S.C. 1757a(b). A high rural LID rate means much
## of the rural population sits outside the cap already -- read this before
## assuming the cap binds (E10, and Q3).
##
## `shortform` is omitted: the field is present in the Stata build but 100% NA,
## so a short-form column here would read as 0% and mislead.
EX$E9_designations <- cur[, .(credit_unions = uniqueN(cu_number),
                              lid_pct = round(100 * mean(lid), 1),
                              mdi_pct = round(100 * mean(mdi), 1),
                              federal_charter_pct = round(100 * mean(fed), 1)),
                          by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural"))]
cat("\n=== E9. Designations ===\n"); print(EX$E9_designations)
cat("LID carries an exemption from the MBL cap. Note the rural rate.\n")

## ---------------------------------------------------------------------------
## E10. MBL HEADROOM -- descriptive preview of Q3
## ---------------------------------------------------------------------------
## Statutory cap, 12 U.S.C. 1757a(a): the lesser of 1.75x actual net worth, or
## 1.75x the minimum net worth to be well capitalized (7% of assets), i.e.
## 12.25% of assets. Which prong binds depends on the institution's net worth
## ratio, so the cap is INSTITUTION-SPECIFIC -- a single 12.25% line is wrong.
##
## lns_mbl_part723 is used because it is the Part 723 statutory concept and has
## complete 2010-2026 coverage. lns_comm is a supervisory category that begins
## in 2018 and is NOT the right measure for the cap.
##
## This is descriptive only. The formal bunching estimator lives in Q3.

if (all(c("lns_mbl_part723","networth_tot") %in% names(cur))) {
  m <- cur[!is.na(lns_mbl_part723) & !is.na(networth_tot) & assets_tot > 0]
  m[, `:=`(cap = pmin(1.75 * networth_tot, 0.1225 * assets_tot))]
  m[, `:=`(mbl_share_assets = 100 * lns_mbl_part723 / assets_tot,
           utilization      = fifelse(cap > 0, lns_mbl_part723 / cap, NA_real_),
           exempt           = lid == 1L)]

  EX$E10_mbl_headroom <- m[, .(
    credit_unions   = uniqueN(cu_number),
    pct_with_mbl    = round(100 * mean(lns_mbl_part723 > 0), 1),
    median_mbl_share_assets = round(median(mbl_share_assets), 2),
    pct_over_half_cap  = round(100 * mean(utilization > 0.50, na.rm = TRUE), 1),
    pct_over_75pct_cap = round(100 * mean(utilization > 0.75, na.rm = TRUE), 1),
    pct_over_90pct_cap = round(100 * mean(utilization > 0.90, na.rm = TRUE), 1)
  ), by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural"),
            cap_status = fifelse(exempt, "LID - exempt from cap", "subject to cap"))]
  setorder(EX$E10_mbl_headroom, cap_status, segment)

  cat("\n=== E10. Member business lending against the statutory cap ===\n")
  print(EX$E10_mbl_headroom)
  cat("If few non-exempt rural CUs sit near their cap, the cap is not what is\n")
  cat("limiting rural business lending -- and Q3 should say so plainly.\n")
} else {
  message("E10 skipped: lns_mbl_part723 or networth_tot not in the panel")
}

## ---------------------------------------------------------------------------
## E11. FRAGILITY -- how much of 'rural' depends on the vintage
## ---------------------------------------------------------------------------
EX$E11_reclassification <- cur[rural_2013 != rural_2024,
  .(credit_unions = uniqueN(cu_number),
    assets_bn = sum(assets_tot, na.rm = TRUE) / 1e9,
    members   = sum(members, na.rm = TRUE)),
  by = .(moved = fifelse(rural_2013 == 1L, "lost rural status", "gained rural status"))]
cat("\n=== E11. Institutions affected by UIC vintage change alone ===\n")
print(EX$E11_reclassification)
cat("These change category with no change in their business whatsoever.\n")

## ---------------------------------------------------------------------------
## WRITE OUT
## ---------------------------------------------------------------------------
for (nm in names(EX)) fwrite(EX[[nm]], file.path(EX_DIR, paste0(nm, ".csv")))

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb  <- openxlsx::createWorkbook()
  hdr <- openxlsx::createStyle(fgFill = "#10263B", fontColour = "white",
                               textDecoration = "bold", halign = "left",
                               border = "TopBottomLeftRight")
  num <- openxlsx::createStyle(numFmt = "#,##0.0")
  for (nm in names(EX)) {
    sh <- substr(nm, 1, 31)
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, EX[[nm]], headerStyle = hdr)
    openxlsx::addStyle(wb, sh, num, rows = 2:(nrow(EX[[nm]]) + 1),
                       cols = which(vapply(EX[[nm]], is.numeric, logical(1))),
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::setColWidths(wb, sh, cols = 1:ncol(EX[[nm]]), widths = "auto")
    openxlsx::freezePane(wb, sh, firstRow = TRUE)
  }
  f <- file.path(EX_DIR, "rural_cu_exhibit_pack.xlsx")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  message("Workbook written: ", normalizePath(f))
} else {
  message("Install openxlsx for the formatted workbook: install.packages(\"openxlsx\")")
}

cat("\n", strrep("=", 74), "\n  THE STORY, IN ORDER\n", strrep("=", 74), "\n", sep = "")
cat("  E2  Rural is ~half of counties but a small share of people.\n")
cat("  E3  So rural credit unions are small -- structurally, not incidentally.\n")
cat("  E4  Their number is falling; E4 says whether faster than the system.\n")
cat("  E5  And whether that is exit or an absence of entry.\n")
cat("  E6  They look weaker on the three statutory outcomes.\n")
cat("  E7  But E7 says how much of that is size rather than place.\n")
cat("  E9  Many are already LID, hence outside the MBL cap.\n")
cat("  E10 And few of the rest may be anywhere near it.\n")
cat("  E11 Some of 'rural' is an artifact of where OMB drew lines.\n")
cat(strrep("=", 74), "\n")
cat("Exhibits written to ", normalizePath(EX_DIR), "\n", sep = "")
