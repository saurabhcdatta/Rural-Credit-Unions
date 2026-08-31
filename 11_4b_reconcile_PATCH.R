###############################################################################
# 11_4b_reconcile_PATCH.R
#
# THE REMAINING ERROR, AND WHY IT IS EXACTLY ONE SITE
#
#   Residual came back Rural +1, Urban -1. Equal and opposite means nothing is
#   lost or invented -- one site is being counted in the wrong segment.
#
#   The cause is that openings and closings were attributed on event-time
#   status, while the LEVELS are attributed on endpoint status.
#
#     A site that OPENED is absent at QA and present at QZ. It joins the end
#     level of whatever segment it is in AT QZ -- that is r_last. But it was
#     counted as an opening in r_first.
#
#     A site that CLOSED is present at QA and absent at QZ. It leaves the start
#     level of the segment it was in AT QA -- that is r_first. But it was
#     counted as a closing in r_last.
#
#   For any site that never crossed the rural line, r_first == r_last and the
#   distinction is invisible. It only bites for sites that BOTH crossed the line
#   AND opened or closed within the window. Here there is exactly one.
#
#   The earlier arrivals/departures term only covered crossers present at both
#   endpoints, so it could not catch this case.
#
# THE FIX
#   Reconcile on level-consistent attribution: openings by r_last, closings by
#   r_first. Keep the event-time attribution for T8 and T9, because "an office
#   opened in a rural county" should mean where it opened -- but report how many
#   sites the two attributions disagree about, so the difference is documented
#   rather than absorbed.
#
# Run after the corrected 11.4 block, in place of its reconciliation section.
###############################################################################

## ---- level-consistent attribution -------------------------------------------

lvl <- bb[qidx %in% c(QA, QZ),
          .(offices = .N),
          by = .(qidx, seg = fifelse(rural_site == 1L, "Rural", "Urban"))]
lvl <- dcast(lvl, seg ~ qidx, value.var = "offices")
setnames(lvl, as.character(c(QA, QZ)), c("start", "end"))

## Openings join the END level, so attribute them to status at the end.
## Closings leave the START level, so attribute them to status at the start.
open_lvl <- life[opened == TRUE,
                 .(open_lvl = .N),
                 by = .(seg = fifelse(r_last == 1L, "Rural", "Urban"))]
close_lvl <- life[closed == TRUE,
                  .(close_lvl = .N),
                  by = .(seg = fifelse(r_first == 1L, "Rural", "Urban"))]

## Crossers present at BOTH endpoints move between level counts without any
## opening or closing event.
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

cat("\n=== T8b. Reconciliation on level-consistent attribution ===\n")
print(rec[, .(seg, start, end, actual,
              openings = open_lvl, closings = close_lvl,
              arrivals, departures, implied, residual)])

## How much do the two attributions disagree? This is the number that was
## silently breaking the identity, and it belongs in the record.
disagree <- life[crossed == TRUE & (opened == TRUE | closed == TRUE)]
cat("\nSites that crossed the rural line AND opened or closed inside the window: ",
    nrow(disagree), "\n", sep = "")
if (nrow(disagree))
  print(disagree[, .(cu_number, site_id,
                     opened, closed,
                     from = fifelse(r_first == 1L, "Rural", "Urban"),
                     to   = fifelse(r_last  == 1L, "Rural", "Urban"))])
cat("These are the only sites for which event-time and level-consistent\n")
cat("attribution differ. Everything else is identical under both.\n")

stopifnot(all(rec$residual == 0L))
cat("\nIdentity closes exactly for both segments.\n")

## ---- cumulative flows, reported on event-time attribution -------------------
## T9 keeps the natural reading -- an office that opened in a rural county is a
## rural opening -- and states how many sites the two bases disagree about.

cum_evt <- net[, .(Openings = sum(Openings), Closings = sum(Closings)), by = seg]
cum <- merge(cum_evt, rec[, .(seg, start, end)], by = "seg")
cum[, net_offices := end - start]
cum[, `opened per 100 closed` := round(100 * Openings / Closings, 1)]

cat("\n=== T9. Cumulative flows ===\n")
print(cum[, .(seg, start, end, net_offices, Openings, Closings,
              `opened per 100 closed`)])
cat("\nOpenings and closings above are on event-time attribution and differ from\n")
cat("the reconciliation table by ", nrow(disagree), " site(s). Report the NET OFFICE\n", sep = "")
cat("count as the headline: a ratio just above 100 is replacement, not growth,\n")
cat("and readers consistently mistake it for a growth rate.\n")
fwrite(cum, file.path(BD_DIR, "T9_cumulative_flows.csv"))
