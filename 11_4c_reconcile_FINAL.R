###############################################################################
# 11_4c_reconcile_FINAL.R
#
# THE LAST ERROR: SITES THAT OPENED *AND* CLOSED INSIDE THE WINDOW
#
#   Residual went from (Rural +1, Urban -1) to (Rural -1, Urban +1). Flipping
#   sign by the same magnitude means the correction overshot by exactly as much
#   as it fixed -- so a third group of sites is being counted that should not be
#   counted at all.
#
#   That group is sites that opened AFTER 2013Q1 and closed BEFORE 2026Q1. They
#   are present at NEITHER endpoint, so they contribute nothing to either level.
#   But `opened` is first_q > QA and `closed` is last_q < QZ, and both are TRUE
#   for these sites -- so they were being added to openings AND to closings.
#
#   For a site that never crossed the rural line the two entries cancel. For one
#   that crossed, they land in different segments and do not.
#
#   Your disagree list shows exactly three such sites:
#     15516 / 22933   Urban -> Rural
#     68187 / 53072   Urban -> Rural
#     68385 / 51985   Rural -> Urban
#
#   Two push Rural implied up by one each and Urban down by one each; the third
#   does the reverse. Net distortion: Rural +1, Urban -1 -- which is precisely
#   the residual, with the sign reversed.
#
# THE FIX
#   For the LEVEL identity, count only openings that survived to the end, and
#   only closings that were present at the start. Sites that came and went
#   inside the window are excluded from both, because they touch neither
#   endpoint.
#
#   T8 and T9 keep the full event counts, because a site that opened and later
#   closed really did open and really did close. The two bases are reported
#   side by side so the difference is on the record.
#
# Run in place of the reconciliation section.
###############################################################################

## ---- the four lifecycle groups ---------------------------------------------
## Naming them explicitly is what makes the identity checkable by eye.

life[, grp := fifelse(!opened & !closed, "throughout",
              fifelse( opened & !closed, "opened, still open",
              fifelse(!opened &  closed, "open at start, closed",
                                         "opened and closed inside window")))]

cat("\n=== T6b. Sites by lifecycle group ===\n")
print(life[, .(sites = .N, crossed_rural_line = sum(crossed)), by = grp][order(-sites)])
cat("\nOnly the first three groups touch an endpoint level. The fourth is present\n")
cat("at neither 2013Q1 nor 2026Q1 and must be excluded from the level identity.\n")

## ---- level-consistent flows -------------------------------------------------
## Openings join the END level  -> count those still open at QZ, by status at QZ.
## Closings leave the START level -> count those open at QA, by status at QA.

lvl <- bb[qidx %in% c(QA, QZ),
          .(offices = .N),
          by = .(qidx, seg = fifelse(rural_site == 1L, "Rural", "Urban"))]
lvl <- dcast(lvl, seg ~ qidx, value.var = "offices")
setnames(lvl, as.character(c(QA, QZ)), c("start", "end"))

open_lvl <- life[opened == TRUE & closed == FALSE,
                 .(open_lvl = .N),
                 by = .(seg = fifelse(r_last  == 1L, "Rural", "Urban"))]
close_lvl <- life[closed == TRUE & opened == FALSE,
                  .(close_lvl = .N),
                  by = .(seg = fifelse(r_first == 1L, "Rural", "Urban"))]

## Crossers present at BOTH endpoints shift between level counts with no event.
cross_both <- life[crossed == TRUE & opened == FALSE & closed == FALSE]
mig <- data.table(
  seg        = c("Rural", "Urban"),
  arrivals   = c(cross_both[r_last  == 1L, .N], cross_both[r_last  == 0L, .N]),
  departures = c(cross_both[r_first == 1L, .N], cross_both[r_first == 0L, .N]))

rec <- Reduce(function(a, b) merge(a, b, by = "seg", all = TRUE),
              list(lvl, open_lvl, close_lvl, mig))
for (v in c("open_lvl", "close_lvl", "arrivals", "departures"))
  rec[is.na(get(v)), (v) := 0L]

rec[, implied  := open_lvl - close_lvl + arrivals - departures]
rec[, actual   := end - start]
rec[, residual := actual - implied]

cat("\n=== T8b. Reconciliation: do the flows explain the levels? ===\n")
print(rec[, .(seg, start, end, actual,
              openings = open_lvl, closings = close_lvl,
              arrivals, departures, implied, residual)])

if (any(rec$residual != 0L)) {
  cat("\nSTILL NOT ZERO. Next things to check, in order:\n")
  cat("  1. is qidx exactly year*4 + quarter for every row of bb?\n")
  cat("  2. does any site appear twice at the same qidx (duplicate site_id)?\n")
  cat("  3. is rural_site ever NA inside bb after the 11.1 filter?\n")
  print(bb[, .N, by = .(cu_number, site_id, qidx)][N > 1][1:10])
  stop("Flow-level identity failed; residual = ",
       paste(rec$seg, rec$residual, collapse = "; "), call. = FALSE)
}
cat("\nIdentity closes exactly for both segments.\n")

## ---- T9, on both bases ------------------------------------------------------
## Event counts answer "how much churn was there". Level counts answer "where
## did the network end up". Reporting both stops anyone reconciling them badly.

evt <- rbind(
  life[opened == TRUE, .(Openings = .N),
       by = .(seg = fifelse(r_first == 1L, "Rural", "Urban"))],
  life[closed == TRUE, .(Closings = .N),
       by = .(seg = fifelse(r_last  == 1L, "Rural", "Urban"))],
  fill = TRUE)
evt <- evt[, lapply(.SD, sum, na.rm = TRUE), by = seg, .SDcols = c("Openings", "Closings")]

cum <- merge(evt, rec[, .(seg, start, end)], by = "seg")
cum[, net_offices := end - start]
cum[, `opened per 100 closed` := round(100 * Openings / Closings, 1)]

cat("\n=== T9. Cumulative flows ===\n")
print(cum[, .(seg, start, end, net_offices, Openings, Closings,
              `opened per 100 closed`)])
cat("\nHEADLINE THE NET OFFICE COUNT, not the ratio. A figure just above 100 is\n")
cat("replacement, and readers reliably mistake it for a growth rate.\n")
fwrite(cum, file.path(BD_DIR, "T9_cumulative_flows.csv"))
