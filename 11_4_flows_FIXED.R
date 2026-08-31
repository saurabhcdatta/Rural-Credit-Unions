###############################################################################
# 11_4_flows_FIXED.R  --  replacement for block 11.4 of 11_branch_deep_dive.R
#
# WHY THIS REPLACEMENT EXISTS
#   The published replacement rates do not reconcile with the office levels in
#   the same script. T2 says urban offices went 20,129 -> 20,793, a gain of 664.
#   T9 says urban openings 11,101 against closings 11,419, a NET LOSS of 318.
#   Both cannot be true of the same network over the same window.
#
#   Two defects produce it.
#
#   BUG 1 -- the quarter index rolls a year early on Q4.
#     qidx = year * 4 + quarter, so 2013Q4 gives 2013*4+4 = 8056, and
#     8056 %/% 4 = 2014. Every fourth quarter is labelled into the following
#     year. This hits lab_q() on the axes of F1, F2 and F3, and it hits the
#     `year` grouping in the flow table. Fix: index from zero -- (qidx-1) %/% 4
#     for the year and ((qidx-1) %% 4) + 1 for the quarter.
#
#   BUG 2 -- flow[year < max(year)] silently deletes real events.
#     It was meant to drop a partial final year. Because of Bug 1, max(year)
#     is 2026 and that label covers BOTH 2025Q4 and 2026Q1 -- so two quarters
#     of openings and closings vanish from the cumulative totals in T9, while
#     the levels in T2 still include them. The identity cannot close.
#
#   A third issue, smaller but real: openings are segmented by r_first and
#   closings by r_last, while the LEVELS are segmented by rural status in each
#   quarter. For the handful of sites that crossed the rural line the two
#   disagree, so crossings have to enter the identity as their own term rather
#   than being absorbed silently.
#
# WHAT THIS BLOCK GUARANTEES
#   end - start = openings - closings + arrivals - departures, exactly, for
#   each segment. It is asserted, not assumed. If it fails the script stops and
#   prints the residual, because a replacement rate computed off flows that do
#   not reconcile with levels is not a statistic, it is a coincidence.
#
# Paste over block 11.4 (the `life` / `flow` / `net` / `cum` section).
# Requires: bb, QA, QZ, BD_DIR, INK, RUR, URB from earlier in 11_.
###############################################################################

## ---- 11.4  where capacity actually moves (corrected) ------------------------

## Fixed quarter labelling. Use these everywhere in place of the originals.
q_year <- function(q) (q - 1L) %/% 4L
q_qtr  <- function(q) ((q - 1L) %% 4L) + 1L
lab_q  <- function(q) paste0(q_year(q), "Q", q_qtr(q))
cat("\nQuarter labelling check: QA =", lab_q(QA), " QZ =", lab_q(QZ),
    " (both should read 2013Q1 and 2026Q1)\n")
stopifnot(lab_q(QA) == "2013Q1")

life <- bb[, .(first_q = min(qidx), last_q = max(qidx),
               r_first  = rural_site[which.min(qidx)],
               r_last   = rural_site[which.max(qidx)],
               f_first  = fips[which.min(qidx)],
               f_last   = fips[which.max(qidx)],
               hq       = any(main_office, na.rm = TRUE)),
           by = .(cu_number, site_id)]

## A site "opened" if it was not there in the first quarter of the window, and
## "closed" if it was not there in the last. Everything else is continuity.
life[, `:=`(opened  = first_q > QA,
            closed  = last_q  < QZ,
            moved_county = f_first != f_last,
            crossed = r_first != r_last)]

## Presence gaps do not break the endpoint identity -- only first and last
## quarter matter for it -- but they DO mean a site can be absent from a level
## count in the middle of the series without being an opening or a closing.
## Quantify them so nobody has to wonder.
gaps <- bb[, .(spanned = max(qidx) - min(qidx) + 1L, observed = uniqueN(qidx)),
           by = .(cu_number, site_id)][spanned > observed]
cat("Sites with interior gaps in the panel: ", nrow(gaps),
    " (", round(100 * nrow(gaps) / nrow(life), 2), "% of sites)\n", sep = "")

cat("\n=== T6. Site lifecycle, ", lab_q(QA), " to ", lab_q(QZ), " ===\n", sep = "")
print(life[, .(sites = .N, opened = sum(opened), closed = sum(closed),
               moved_county = sum(moved_county), crossed_rural_line = sum(crossed))])

cat("\n=== T7. Sites that crossed the rural line (real relocations) ===\n")
print(life[crossed == TRUE, .N,
           by = .(direction = fifelse(r_first == 1L, "rural -> urban", "urban -> rural"))])
cat("Fixed vintage, so these moved. Compare the magnitude with openings and\n")
cat("closings below -- relocation is a rounding error; the network changes\n")
cat("through what opens and what shuts.\n")

## ---- annual flows, with NO year dropped -------------------------------------
## Every opening and every closing in the window is counted. Dropping a partial
## year is what broke the reconciliation before; if the final year looks thin on
## the chart, say so in the caption rather than deleting the observations.
flow <- rbind(
  life[opened == TRUE,
       .(n = .N, kind = "Openings"),
       by = .(year = q_year(first_q), seg = fifelse(r_first == 1L, "Rural", "Urban"))],
  life[closed == TRUE,
       .(n = .N, kind = "Closings"),
       by = .(year = q_year(last_q),  seg = fifelse(r_last  == 1L, "Rural", "Urban"))])

net <- dcast(flow, year + seg ~ kind, value.var = "n", fill = 0L)
if (!"Openings" %in% names(net)) net[, Openings := 0L]
if (!"Closings" %in% names(net)) net[, Closings := 0L]
net[, net := Openings - Closings]
setorder(net, seg, year)

PARTIAL <- q_year(QZ)                    # the final year is one quarter only
cat("\n=== T8. Openings, closings and net change by year ===\n")
cat("NOTE: ", PARTIAL, " covers ", lab_q(QZ), " only. It is retained, not dropped,\n",
    "so the flows reconcile with the levels. Read it as a partial year.\n", sep = "")
print(dcast(net, year ~ seg, value.var = c("Openings", "Closings", "net")))
fwrite(net, file.path(BD_DIR, "T8_openings_closings.csv"))

## ---- the reconciliation --------------------------------------------------
## end - start = openings - closings + arrivals - departures.
## Arrivals and departures are the sites that crossed the rural line: they leave
## one segment's level count and join the other without opening or closing.

lvl <- bb[qidx %in% c(QA, QZ),
          .(offices = .N),
          by = .(qidx, seg = fifelse(rural_site == 1L, "Rural", "Urban"))]
lvl <- dcast(lvl, seg ~ qidx, value.var = "offices")
setnames(lvl, as.character(c(QA, QZ)), c("start", "end"))

## A crossing site only shifts a level if it was present at BOTH endpoints.
cross <- life[crossed == TRUE & opened == FALSE & closed == FALSE]
mig <- data.table(
  seg      = c("Rural", "Urban"),
  arrivals = c(cross[r_last == 1L, .N], cross[r_last == 0L, .N]),
  departures = c(cross[r_first == 1L, .N], cross[r_first == 0L, .N]))

flows_tot <- net[, .(Openings = sum(Openings), Closings = sum(Closings)), by = seg]
rec <- Reduce(function(a, b) merge(a, b, by = "seg"), list(lvl, flows_tot, mig))
rec[, implied := Openings - Closings + arrivals - departures]
rec[, actual  := end - start]
rec[, residual := actual - implied]

cat("\n=== T8b. Reconciliation: do the flows explain the levels? ===\n")
print(rec[, .(seg, start, end, actual, Openings, Closings,
              arrivals, departures, implied, residual)])

if (any(rec$residual != 0L)) {
  cat("\nRESIDUAL IS NOT ZERO. Do not publish the replacement rates.\n")
  cat("Most likely causes, in order:\n")
  cat("  1. a year is still being dropped from `flow`\n")
  cat("  2. openings/closings segmented on a different basis from the levels\n")
  cat("  3. bb filtered differently between the level and lifecycle steps\n")
  stop("Flow-level identity failed; residual = ",
       paste(rec$seg, rec$residual, collapse = "; "), call. = FALSE)
}
cat("Identity closes exactly for both segments.\n")

## ---- cumulative flows and the replacement rate ------------------------------
## Only computed once the identity above has closed.
cum <- rec[, .(seg, Openings, Closings, net_offices = end - start)]
cum[, `opened per 100 closed` := round(100 * Openings / Closings, 1)]
cat("\n=== T9. Cumulative flows ===\n"); print(cum)
cat("\nRead the last column as: for every 100 offices that shut, this many opened.\n")
cat("A figure just above 100 is replacement, not growth. Report the net office\n")
cat("count alongside it so nobody reads 105 as a growth rate.\n")
fwrite(cum, file.path(BD_DIR, "T9_cumulative_flows.csv"))

F4 <- ggplot(net, aes(year, net, fill = seg)) +
  geom_hline(yintercept = 0, colour = INK, linewidth = .4) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  scale_fill_manual(values = c(Rural = RUR, Urban = URB)) +
  labs(title = "Net change in credit union offices each year",
       subtitle = "Openings minus closings. This, not relocation, is how the network shifts.",
       x = NULL, y = "Net offices",
       caption = paste0("Every year in the window is shown. ", PARTIAL,
                        " covers ", lab_q(QZ), " only and is therefore partial."))
sv(F4, "F4_net_change")
