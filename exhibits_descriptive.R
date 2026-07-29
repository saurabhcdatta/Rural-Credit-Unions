###############################################################################
# exhibits_descriptive.R
#
# EXECUTIVE EXHIBIT PACK -- Rural Credit Unions Study (ROAD Act Sec. 909)
#
# Ten tables, in narrative order. Run this BEFORE any econometrics. Each table
# answers one question a Board member or committee staffer would actually ask,
# and each is intended to survive into the report.
#
#   E1  Headline    -- the rural universe in one row
#   E2  Asymmetry   -- half the counties, a fraction of the people
#   E3  Size        -- rural CUs by asset band (sets up Q1)
#   E4  Trend       -- counts, members, assets by year, indexed
#   E5  Flows       -- entry, merger, liquidation by year
#   E6  Performance -- the three statutory outcomes, pooled
#   E7  Perf x size -- the same outcomes within asset band (the Q1 teaser)
#   E8  Geography   -- states ranked by rural CU presence
#   E9  Designations-- LID / MDI / short-form overlap with rural
#   E10 Fragility   -- reclassification exposure
#
# Outputs: console, CSVs, and a formatted .xlsx workbook for circulation.
###############################################################################

library(data.table)
source("panel_prep.R")

CFG <- list(
  panel_dir = "path/to/dta",     # folder holding OCE_CallReport_*.dta
  cache_dir = "data/raw",
  out_dir   = "out/exhibits"
)
dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)

P   <- prep_panel(CFG$panel_dir, cache_dir = CFG$cache_dir)
cr  <- P$cr
uic <- P$uic

LATEST <- cr[, max(qidx)]
cur    <- cr[qidx == LATEST]
cat("\nLatest quarter in panel: ", cur[1, year], "Q", cur[1, quarter], "\n", sep = "")

EX <- list()   # collected for the workbook

## ---------------------------------------------------------------------------
## E1. HEADLINE -- the rural universe in one row
## ---------------------------------------------------------------------------
EX$E1_headline <- cur[, .(
  credit_unions = uniqueN(cu_number),
  members_mn    = sum(members,    na.rm = TRUE) / 1e6,
  assets_bn     = sum(assets_tot, na.rm = TRUE) / 1e9,
  median_assets_mn = median(assets_tot, na.rm = TRUE) / 1e6
), by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural"))]

tot <- EX$E1_headline[, lapply(.SD, sum), .SDcols = c("credit_unions","members_mn","assets_bn")]
EX$E1_headline[, `:=`(pct_of_cus     = 100 * credit_unions / tot$credit_unions,
                      pct_of_members = 100 * members_mn    / tot$members_mn,
                      pct_of_assets  = 100 * assets_bn     / tot$assets_bn)]

cat("\n=== E1. The rural credit union universe ===\n"); print(EX$E1_headline)

## ---------------------------------------------------------------------------
## E2. ASYMMETRY -- the single most useful framing number in the study
## ---------------------------------------------------------------------------
## Rural is roughly half of all counties but a small share of the population.
## Everything about scale in this report follows from that.

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
cat("Read the gap between the county share and the population share aloud in\n")
cat("any briefing -- it is the reason rural institutions are small.\n")

## ---------------------------------------------------------------------------
## E3. SIZE DISTRIBUTION -- sets up Q1 before any regression
## ---------------------------------------------------------------------------
EX$E3_size <- dcast(cur[, .(n = uniqueN(cu_number)), by = .(band, rural)],
                    band ~ rural, value.var = "n", fill = 0)
setnames(EX$E3_size, c("0","1"), c("nonrural_n","rural_n"))
EX$E3_size[, `:=`(rural_pct    = round(100 * rural_n    / sum(rural_n), 1),
                  nonrural_pct = round(100 * nonrural_n / sum(nonrural_n), 1))]
EX$E3_size[, rural_share_of_band := round(100 * rural_n / (rural_n + nonrural_n), 1)]

cat("\n=== E3. Asset size distribution ===\n"); print(EX$E3_size)
cat("If rural CUs cluster in the bottom bands, every raw performance gap in\n")
cat("this pack is partly a size gap. E7 is where that gets separated.\n")

## ---------------------------------------------------------------------------
## E4. TREND -- indexed so rural and non-rural are directly comparable
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

cat("\n=== E4. Trend, indexed to first year = 100 ===\n")
print(dcast(yr, year ~ segment, value.var = "cu_index"))
cat("If the two index lines track each other, this is consolidation, not\n")
cat("rural decline -- and the report should say so plainly.\n")

## ---------------------------------------------------------------------------
## E5. FLOWS -- where the change in charter counts actually comes from
## ---------------------------------------------------------------------------
first_last <- cr[, .(first_q = min(qidx), last_q = max(qidx),
                     seg = fifelse(rural[which.max(qidx)] == 1L, "Rural", "Non-rural")),
                 by = cu_number]
panel_end <- cr[, max(qidx)]; panel_start <- cr[, min(qidx)]

exits <- first_last[last_q < panel_end, .(exits = .N),
                    by = .(year = last_q %/% 4L, seg)]
entries <- first_last[first_q > panel_start, .(entries = .N),
                      by = .(year = first_q %/% 4L, seg)]
EX$E5_flows <- merge(entries, exits, by = c("year","seg"), all = TRUE)
setnafill(EX$E5_flows, fill = 0, cols = c("entries","exits"))
EX$E5_flows[, net := entries - exits]

cat("\n=== E5. Entries and exits by year ===\n"); print(EX$E5_flows[order(seg, year)])
cat("NOTE: 'exits' pools mergers, liquidations, and charter conversions. Split\n")
cat("them with the outcome/reason fields before this goes in the report.\n")

## ---------------------------------------------------------------------------
## E6. PERFORMANCE -- the three statutory outcomes, pooled
## ---------------------------------------------------------------------------
## Unweighted AND asset-weighted, because they tell different stories and the
## difference is itself informative.

perf_vars <- intersect(c("g_assets","g_members","g_loans","networth_pct","roa_pct",
                         "netintmrg","dq_rate"), names(cr))
recent <- cr[qidx > LATEST - 4]

EX$E6_performance <- rbind(
  recent[, c(.(basis = "median (unweighted)"),
             lapply(.SD, median, na.rm = TRUE)),
         by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural")), .SDcols = perf_vars],
  recent[, c(.(basis = "mean (asset-weighted)"),
             lapply(.SD, function(x) weighted.mean(x, assets_tot, na.rm = TRUE))),
         by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural")), .SDcols = perf_vars])

cat("\n=== E6. Performance, trailing four quarters ===\n"); print(EX$E6_performance)

## ---------------------------------------------------------------------------
## E7. PERFORMANCE WITHIN SIZE BAND -- the Q1 teaser
## ---------------------------------------------------------------------------
## This is the most important table in the pack. If the gap collapses across
## bands, the report is about scale, not rurality.

pbs <- recent[, lapply(.SD, median, na.rm = TRUE),
              by = .(band, rural), .SDcols = c("g_assets","networth_pct","roa_pct")]
EX$E7_perf_by_size <- dcast(
  melt(pbs, id.vars = c("band","rural")), band + variable ~ rural, value.var = "value")
setnames(EX$E7_perf_by_size, c("0","1"), c("nonrural","rural"))
EX$E7_perf_by_size[, gap := round(rural - nonrural, 3)]
setorder(EX$E7_perf_by_size, variable, band)

cat("\n=== E7. Rural minus non-rural, WITHIN asset band ===\n"); print(EX$E7_perf_by_size)
cat("Compare the `gap` column across bands within each outcome. Shrinking\n")
cat("toward zero = a size story. Stable and non-zero = a rural story.\n")

## ---------------------------------------------------------------------------
## E8. GEOGRAPHY -- where rural credit unions are
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
## business lending cap (12 U.S.C. 1757a(b)). If rural CUs carry LID at a high
## rate, a large share of them are already outside the cap -- which changes
## what the barrier analysis can conclude.

EX$E9_designations <- cur[, .(credit_unions = uniqueN(cu_number),
                              lid_pct  = round(100 * mean(lid), 1),
                              mdi_pct  = round(100 * mean(mdi), 1),
                              fed_pct  = round(100 * mean(fed), 1),
                              shortform_pct = round(100 * mean(sf), 1)),
                          by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural"))]

cat("\n=== E9. Designations ===\n"); print(EX$E9_designations)
cat("LID carries an exemption from the MBL cap -- read this before assuming\n")
cat("the cap binds rural institutions (Q3).\n")

## ---------------------------------------------------------------------------
## E10. FRAGILITY -- how much of 'rural' depends on the vintage
## ---------------------------------------------------------------------------
EX$E10_reclassification <- cur[rural_2013 != rural_2024,
  .(credit_unions = uniqueN(cu_number),
    assets_bn = sum(assets_tot, na.rm = TRUE) / 1e9,
    members   = sum(members, na.rm = TRUE)),
  by = .(moved = fifelse(rural_2013 == 1L, "lost rural status", "gained rural status"))]

cat("\n=== E10. Institutions affected by UIC vintage change alone ===\n")
print(EX$E10_reclassification)
cat("These change category with no change in their business whatsoever.\n")

## ---------------------------------------------------------------------------
## WRITE OUT
## ---------------------------------------------------------------------------
for (nm in names(EX)) fwrite(EX[[nm]], file.path(CFG$out_dir, paste0(nm, ".csv")))

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  hdr <- openxlsx::createStyle(fgFill = "#10263B", fontColour = "white",
                               textDecoration = "bold", halign = "left", border = "TopBottomLeftRight")
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
  f <- file.path(CFG$out_dir, "rural_cu_exhibit_pack.xlsx")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  message("Workbook written: ", normalizePath(f))
} else {
  message("Install openxlsx for the formatted workbook: install.packages(\"openxlsx\")")
}

cat("\n", strrep("=", 74), "\n  THE STORY, IN ORDER\n", strrep("=", 74), "\n", sep = "")
cat("  E2  Rural is ~half of counties but a small share of people.\n")
cat("  E3  So rural credit unions are small -- structurally, not incidentally.\n")
cat("  E4  Their number is falling. E4 says whether faster than the system.\n")
cat("  E5  And says whether that is exit or absence of entry.\n")
cat("  E6  They look weaker on the three statutory outcomes.\n")
cat("  E7  But E7 says how much of that is size rather than place.\n")
cat("  E9  Many are already LID, hence outside the MBL cap.\n")
cat("  E10 And some of 'rural' is an artifact of where OMB drew lines.\n")
cat(strrep("=", 74), "\n")
