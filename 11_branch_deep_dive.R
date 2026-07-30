###############################################################################
# 11_branch_deep_dive.R  --  offices and headquarters, 2013-2026
#
# Three questions:
#   A. Has the credit union office network grown or shrunk, and where?
#   B. Has capacity migrated between rural and urban places?
#   C. Does the answer depend on which vintage of the rural definition we use?
#
# THE VINTAGE PROBLEM AND ITS SOLUTION
#   A site's rural flag can change for two reasons: the site moved, or OMB
#   redrew the county. Mixing them makes a trend uninterpretable. So we hold
#   the map still and run the series TWICE -- once under the 2013 codes, once
#   under the 2024 codes. Within each series, every change is real.
#
#   The gap between the two series is the definitional effect, isolated. And it
#   yields a testable prediction: because each vintage classifies a FIXED set of
#   counties, the choice should move the LEVEL of the rural series but not its
#   TREND. Section 11.3 tests that directly. If the indexed trends track, the
#   findings are robust to a choice we cannot avoid making.
#
# SCOPE: OFFICES ONLY, ATMs DELIBERATELY EXCLUDED
#   The branch file has no ATM rows -- SiteTypeName is only "Branch Office" or
#   "Corporate Office", and ATM is a flag on an office. Two reasons that measure
#   is not used here:
#
#     1. It answers the wrong question. Member access to cash runs through
#        SHARED networks, not through a credit union's own machines. A member
#        can have surcharge-free access to tens of thousands of ATMs while their
#        credit union owns none, so office-level ATM ownership does not measure
#        access.
#     2. It sits at a lower evidentiary standard than everything else here.
#        Placing a flag of uncertain provenance beside results built on stable
#        site identifiers and dual-vintage validation invites a reviewer to
#        discount both.
#
#   If ATM access becomes material to the report, do it properly: the separate
#   NCUA ATM Locations table (2012Q4 onward) combined with shared-network
#   participation. That is an analysis, not a column.
#
# Requires: b (5_), uic24 / uic13 (2_), OUT_DIR (1_)
###############################################################################

library(data.table); library(ggplot2)

BD_DIR <- file.path(OUT_DIR, "branch_deepdive"); dir.create(BD_DIR, recursive = TRUE, showWarnings = FALSE)
INK <- "#10263B"; RUR <- "#4E7A51"; URB <- "#8FA3B5"; ACC <- "#C4451C"; BLU <- "#1F6FB2"
theme_set(theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = INK, size = 13),
        plot.subtitle = element_text(colour = "#4A5A6A", size = 10),
        plot.caption = element_text(colour = "#7A8894", size = 8, hjust = 0),
        panel.grid.minor = element_blank(), legend.position = "top",
        legend.title = element_blank(), strip.text = element_text(face = "bold", colour = INK)))
sv <- function(p, nm, w = 9, h = 5) {
  ggsave(file.path(BD_DIR, paste0(nm, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  cat("wrote ", nm, ".png\n", sep = "")
}

## ---- 11.1 sample ------------------------------------------------------------
## Start 2013Q1: the branch table was introduced Sep-2010 and populated over its
## first year, so earlier "openings" are reporting coverage catching up.
## Require BOTH vintages classified, so the two series cover identical sites.
Q0 <- 2013 * 4L + 1L

## Derive the exclusion flags if 5_ did not persist them. Doing this here rather
## than assuming keeps the script independent of how the branch panel was built.
if (!"foreign" %in% names(b))
  b[, foreign := !is.na(country) & nzchar(country) & country != "United States"]
if (!"territory" %in% names(b))
  b[, territory := state %in% c("GU","VI","AS","MP","FM","MH","PW")]

## Column aliases. 5_ names things slightly differently depending on how it
## was written (rural_site_13 vs rural_site_2013, q vs quarter). Add aliases
## rather than rename, so nothing downstream of this script breaks.
if (!"rural_site_2013" %in% names(b)) {
  alt <- setdiff(grep("^rural_site_?(13|2013)$", names(b), value = TRUE), "rural_site")
  if (length(alt)) {
    b[, rural_site_2013 := get(alt[1])]
    message("aliased '", alt[1], "' -> rural_site_2013")
  }
}
if (!"quarter" %in% names(b) && "q" %in% names(b)) b[, quarter := q]

need <- c("qidx", "quarter", "rural_site", "rural_site_2013", "main_office",
          "fips", "cu_number", "site_id")
miss <- setdiff(need, names(b))
if (length(miss))
  stop("branch panel is missing: ", paste(miss, collapse = ", "),
       "\n  columns present: ", paste(names(b), collapse = ", "), call. = FALSE)

bb <- b[!foreign & !territory & qidx >= Q0 &
        !is.na(rural_site) & !is.na(rural_site_2013)]

cat("\n=== sample ===\n")
cat("site-quarters : ", nrow(bb), "\n", sep = "")
cat("distinct sites: ", uniqueN(bb[, .(cu_number, site_id)]), "\n", sep = "")
cat("quarters      : ", uniqueN(bb$qidx), "  (",
    bb[qidx == min(qidx), paste0(year[1], "Q", quarter[1])], " to ",
    bb[qidx == max(qidx), paste0(year[1], "Q", quarter[1])], ")\n", sep = "")
cat("dropped for missing classification under either vintage: ",
    b[!foreign & !territory & qidx >= Q0, .N] - nrow(bb), " site-quarters\n", sep = "")

QA <- bb[, min(qidx)]; QZ <- bb[, max(qidx)]
lab_q <- function(q) paste0(q %/% 4L, "Q", ifelse(q %% 4L == 0L, 4L, q %% 4L))

## ---- 11.2 the network, by office type and rural status ----------------------
netw <- bb[, .(offices = .N,
               hq      = sum(main_office, na.rm = TRUE),
               branch  = sum(!main_office, na.rm = TRUE)),
           by = .(qidx, year, quarter, seg24 = fifelse(rural_site == 1L, "Rural", "Urban"))]
setorder(netw, seg24, qidx)

T1 <- dcast(netw[qidx %in% c(QA, QZ)], seg24 ~ qidx,
            value.var = c("offices", "hq", "branch"))
cat("\n=== T1. Office network, first quarter vs last (2024 vintage) ===\n"); print(T1)

chg <- netw[qidx %in% c(QA, QZ), .(qidx, seg24, offices, hq, branch)]
chg <- dcast(melt(chg, id.vars = c("qidx","seg24")), seg24 + variable ~ qidx)
setnames(chg, as.character(c(QA, QZ)), c("start","end"))
chg[, `:=`(change = end - start, pct = round(100 * (end / start - 1), 1))]
cat("\n=== T2. Change over the period ===\n"); print(chg)
fwrite(chg, file.path(BD_DIR, "T2_network_change.csv"))

F1 <- ggplot(netw, aes(qidx, offices, colour = seg24)) +
  geom_line(linewidth = .8) +
  scale_colour_manual(values = c(Rural = RUR, Urban = URB)) +
  scale_x_continuous(breaks = seq(QA, QZ, by = 8), labels = lab_q) +
  labs(title = "Credit union offices, 2013 to 2026",
       subtitle = "Headquarters and branches combined, classified on the 2024 rural definition held fixed",
       x = NULL, y = "Offices",
       caption = "Source: NCUA Credit Union Branch Information, quarterly. Foreign and territory sites excluded.")
sv(F1, "F1_offices_over_time")

## indexed, so the two segments are comparable despite very different levels
netw[, idx := 100 * offices / offices[qidx == QA], by = seg24]
F2 <- ggplot(netw, aes(qidx, idx, colour = seg24)) +
  geom_hline(yintercept = 100, colour = "#C6CFD8") +
  geom_line(linewidth = .8) +
  scale_colour_manual(values = c(Rural = RUR, Urban = URB)) +
  scale_x_continuous(breaks = seq(QA, QZ, by = 8), labels = lab_q) +
  labs(title = "Office network, indexed to 2013Q1 = 100",
       subtitle = "Whether rural places are losing offices faster than urban ones",
       x = NULL, y = "Index")
sv(F2, "F2_offices_indexed")

## ---- 11.3 DOES THE VINTAGE CHANGE THE ANSWER? ------------------------------
## The central methodological test. Same sites, same quarters, two maps.
v24 <- bb[rural_site      == 1L, .(offices = .N), by = qidx][, vintage := "2024 codes"]
v13 <- bb[rural_site_2013 == 1L, .(offices = .N), by = qidx][, vintage := "2013 codes"]
vv  <- rbind(v24, v13)
vv[, idx := 100 * offices / offices[qidx == QA], by = vintage]

wedge <- dcast(vv, qidx ~ vintage, value.var = "offices")
setnames(wedge, c("qidx", "v2013", "v2024"))
wedge[, `:=`(gap = v2024 - v2013, gap_pct = round(100 * (v2024 / v2013 - 1), 1))]
cat("\n=== T4. Rural office count under each vintage ===\n")
print(wedge[qidx %in% c(QA, (QA + QZ) %/% 2L, QZ)])
cat("\nLevel differs by roughly ", round(mean(wedge$gap_pct), 1),
    "% throughout. The question is whether the TRENDS differ.\n", sep = "")

trend_cmp <- vv[qidx %in% c(QA, QZ), .(qidx, vintage, idx)]
cat("\n=== T5. Indexed change under each vintage ===\n")
print(dcast(trend_cmp, vintage ~ qidx, value.var = "idx"))
cat("If these two end-points are close, the trend is robust to the vintage and\n")
cat("the choice affects only the level -- which is the defensible position.\n")

## ---- 11.3a the headline change under BOTH vintages, both segments ----------
## The table to put in the report. Each row is a self-contained apples-to-apples
## comparison: one map, held fixed, first quarter against last. Reporting the
## RANGE across vintages is more defensible than picking one and hoping the
## question is not asked.
both <- rbindlist(lapply(c("2013 codes", "2024 codes"), function(v) {
  rc <- if (v == "2013 codes") "rural_site_2013" else "rural_site"
  bb[qidx %in% c(QA, QZ), .(offices = .N),
     by = .(qidx, vintage = v, seg = fifelse(get(rc) == 1L, "Rural", "Urban"))]
}))
both <- dcast(both, vintage + seg ~ qidx, value.var = "offices")
setnames(both, as.character(c(QA, QZ)), c("start", "end"))
both[, `:=`(change = end - start, pct = round(100 * (end / start - 1), 1))]
setorder(both, seg, vintage)
cat("\n=== T4a. Office change under BOTH vintages -- the robustness table ===\n")
print(both)

rng <- both[, .(lo = min(pct), hi = max(pct)), by = seg]
cat("\nRural offices grew ", rng[seg == "Rural", lo], "% to ", rng[seg == "Rural", hi],
    "% depending on vintage; urban ", rng[seg == "Urban", lo], "% to ",
    rng[seg == "Urban", hi], "%.\n", sep = "")
cat("Report the RANGE, not a point estimate -- it pre-empts the objection\n")
cat("rather than inviting it.\n")
fwrite(both, file.path(BD_DIR, "T4a_change_both_vintages.csv"))

F3 <- ggplot(vv, aes(qidx, idx, colour = vintage, linetype = vintage)) +
  geom_hline(yintercept = 100, colour = "#C6CFD8") +
  geom_line(linewidth = .8) +
  scale_colour_manual(values = c("2024 codes" = RUR, "2013 codes" = BLU)) +
  scale_x_continuous(breaks = seq(QA, QZ, by = 8), labels = lab_q) +
  labs(title = "Rural office count under two vintages of the same definition",
       subtitle = "Indexed to 2013Q1 = 100. Identical sites, identical quarters, different maps.",
       x = NULL, y = "Index",
       caption = "If the lines track, the vintage choice moves the level but not the trend, and the findings are robust to it.")
sv(F3, "F3_vintage_comparison")
fwrite(wedge, file.path(BD_DIR, "T4_vintage_wedge.csv"))

## ---- 11.4 where capacity actually moves ------------------------------------
## Individual branches almost never relocate across the rural line. Capacity
## migrates through OPENINGS and CLOSINGS, not through moves -- so measure both
## and show which dominates.
life <- bb[, .(first_q = min(qidx), last_q = max(qidx),
               r_first  = rural_site[which.min(qidx)],
               r_last   = rural_site[which.max(qidx)],
               f_first  = fips[which.min(qidx)],
               f_last   = fips[which.max(qidx)],
               hq       = any(main_office, na.rm = TRUE)),
           by = .(cu_number, site_id)]
life[, `:=`(opened = first_q > QA, closed = last_q < QZ,
            moved_county = f_first != f_last,
            crossed = r_first != r_last)]

cat("\n=== T6. Site lifecycle, ", lab_q(QA), " to ", lab_q(QZ), " ===\n", sep = "")
print(life[, .(sites = .N, opened = sum(opened), closed = sum(closed),
               moved_county = sum(moved_county), crossed_rural_line = sum(crossed))])

cat("\n=== T7. Sites that crossed the rural line (real relocations) ===\n")
print(life[crossed == TRUE, .N, by = .(direction = fifelse(r_first == 1L, "rural -> urban", "urban -> rural"))])
cat("Fixed vintage, so these moved. Compare the magnitude with openings and\n")
cat("closings below -- relocation is a rounding error; the network changes\n")
cat("through what opens and what shuts.\n")

flow <- rbind(
  life[opened == TRUE, .(n = .N, kind = "Openings"), by = .(year = first_q %/% 4L, seg = fifelse(r_first == 1L, "Rural", "Urban"))],
  life[closed == TRUE, .(n = .N, kind = "Closings"), by = .(year = last_q  %/% 4L, seg = fifelse(r_last  == 1L, "Rural", "Urban"))])
flow <- flow[year < max(year)]           # drop the partial final year
net  <- dcast(flow, year + seg ~ kind, value.var = "n", fill = 0)
net[, net := Openings - Closings]
cat("\n=== T8. Openings, closings and net change by year ===\n")
print(dcast(net, year ~ seg, value.var = c("Openings","Closings","net")))
fwrite(net, file.path(BD_DIR, "T8_openings_closings.csv"))

F4 <- ggplot(net, aes(year, net, fill = seg)) +
  geom_hline(yintercept = 0, colour = INK, linewidth = .4) +
  geom_col(position = position_dodge(width = .8), width = .7) +
  scale_fill_manual(values = c(Rural = RUR, Urban = URB)) +
  labs(title = "Net change in credit union offices each year",
       subtitle = "Openings minus closings. This, not relocation, is how the network shifts.",
       x = NULL, y = "Net offices")
sv(F4, "F4_net_change")

cum <- net[, .(Openings = sum(Openings), Closings = sum(Closings), net = sum(net)), by = seg]
cum[, `replacement rate (%)` := round(100 * Openings / Closings, 1)]
cat("\n=== T9. Cumulative flows ===\n"); print(cum)

## ---- 11.5 density against a fixed population base ---------------------------
## A static 2020 denominator, so movement here is entirely in the numerator --
## it isolates network change from population change rather than confounding them.
pop_r <- uic24[rural == 1 & as.integer(substr(fips, 1, 2)) <= 56, sum(pop, na.rm = TRUE)]
pop_u <- uic24[rural == 0 & as.integer(substr(fips, 1, 2)) <= 56, sum(pop, na.rm = TRUE)]
dens <- netw[, .(qidx, seg24, offices)]
dens[, per100k := fifelse(seg24 == "Rural", 1e5 * offices / pop_r, 1e5 * offices / pop_u)]

F5 <- ggplot(dens, aes(qidx, per100k, colour = seg24)) +
  geom_line(linewidth = .8) +
  scale_colour_manual(values = c(Rural = RUR, Urban = URB)) +
  scale_x_continuous(breaks = seq(QA, QZ, by = 8), labels = lab_q) +
  labs(title = "Credit union offices per 100,000 residents",
       subtitle = "Population held at the 2020 Census, so all movement here is in the office network",
       x = NULL, y = "Offices per 100,000",
       caption = "A fixed denominator isolates network change. Rural population is in fact falling, so the\ntrue per-resident position is somewhat better than this shows.")
sv(F5, "F5_offices_per_capita")

## ---- 11.6 headquarters versus branches --------------------------------------
## HQ count tracks the number of institutions; branch count tracks reach. They
## can move in opposite directions, and the difference is the consolidation story.
hqbr <- melt(netw[, .(qidx, seg24, hq, branch)], id.vars = c("qidx","seg24"),
             variable.name = "type", value.name = "n")
hqbr[, type := factor(type, c("hq","branch"), c("Headquarters","Branches"))]
hqbr[, idx := 100 * n / n[qidx == QA], by = .(seg24, type)]

F6 <- ggplot(hqbr, aes(qidx, idx, colour = seg24)) +
  geom_hline(yintercept = 100, colour = "#C6CFD8") +
  geom_line(linewidth = .8) + facet_wrap(~ type) +
  scale_colour_manual(values = c(Rural = RUR, Urban = URB)) +
  scale_x_continuous(breaks = seq(QA, QZ, by = 16), labels = lab_q) +
  labs(title = "Headquarters and branches, indexed to 2013Q1 = 100",
       subtitle = "Headquarters count institutions; branches count reach. Divergence is consolidation without withdrawal.",
       x = NULL, y = "Index")
sv(F6, "F6_hq_vs_branches")

cat("\n", strrep("=", 74), "\n  WHAT TO READ\n", strrep("=", 74), "\n", sep = "")
cat("  T2/F2  did rural places lose offices faster than urban ones?\n")
cat("  F3     does the vintage choice change the trend, or only the level?\n")
cat("  T7/T9  relocation vs openings-and-closings: which moves the network?\n")
cat("  F6     if HQs fall but branches hold, institutions consolidated without\n")
cat("         withdrawing from the places they served.\n")
cat(strrep("=", 74), "\nOutputs in ", BD_DIR, "\n", sep = "")
