###############################################################################
# rq2.R  --  RESEARCH QUESTION 2
# "Are rural credit unions disappearing faster than credit unions everywhere
#  else?"
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# WHAT THIS DOES
#   rq2.1  charter counts and index, both segments, vintage held fixed
#   rq2.2  spells with proper left truncation, and the exit hazard on the
#          counting-process form (Surv(start, stop, event)) -- this is the
#          right way to handle institutions already alive when the panel opens
#   rq2.3  the WILLING-ACQUIRER test. A merger needs a buyer. Rural areas have
#          fewer credit unions nearby, so a lower rural exit rate could be
#          mechanical. We condition on how many other credit unions are in the
#          county and the state and see whether the rural coefficient survives.
#          This is the sharpest untested challenge to the survival finding.
#   rq2.4  flows with an identity check: dN = entries - exits + in - out
#   rq2.5  size at first appearance -- the honest de novo denominator
#   rq2.6  where exiting rural charters go (successor rurality)
#   rq2.7  exit composition by code -- UNVERIFIED labels until the dictionary
#          lookup is done; every label prints as such
#   rq2.8  county coverage, first quarter vs last
#   rq2.9  FOR THE SLIDE
#
# REQUIRES: cr from 3_callreport.R.
###############################################################################

suppressPackageStartupMessages({
  library(data.table); library(survival); library(ggplot2)
})

## ---- rq2.0  panel and columns ----------------------------------------------

stopifnot(exists("cr")); if (!is.data.table(cr)) setDT(cr)
OUT <- file.path(if (exists("OUT_DIR")) OUT_DIR else "output", "rq2")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

pick <- function(pats, what, req = TRUE) {
  for (p in pats) { h <- grep(p, names(cr), ignore.case = TRUE, value = TRUE)
    if (length(h)) return(h[order(nchar(h))][1]) }
  if (req) stop("cannot resolve ", what, " in cr"); NA_character_
}
C <- list(
  rural24 = pick(c("^rural_2024$","rural.*24","^rural$"), "rural (2024 vintage)"),
  rural13 = pick(c("^rural_2013$","rural.*13"), "rural (2013 vintage)", FALSE),
  outcome = pick(c("^outcome$"), "outcome code", FALSE),
  reason  = pick(c("^reason$"),  "reason code",  FALSE),
  succ    = pick(c("^join_number_pointer$","pointer","^acquiredcu$"), "successor pointer", FALSE))
cat("Resolved:", paste(names(C), unlist(C), sep = " = ", collapse = " | "), "\n")

QA <- min(cr$qidx); QZ <- max(cr$qidx)
q_lab <- function(q) sprintf("%dQ%d", (q - 1L) %/% 4L, ((q - 1L) %% 4L) + 1L)
cat(sprintf("Panel window %s to %s, %d quarters\n", q_lab(QA), q_lab(QZ), QZ - QA + 1L))

cr[, rural_fix := get(C$rural24)]      # FIXED vintage for the whole series

## ---- rq2.1  charter counts and index ---------------------------------------

cnt <- cr[!is.na(rural_fix), .(cus = uniqueN(cu_number)), by = .(qidx, rural = rural_fix)]
cnt[, index := 100 * cus / cus[qidx == QA], by = rural]
cnt_w <- dcast(cnt, qidx ~ rural, value.var = c("cus", "index"))
setnames(cnt_w, c("qidx","cus_nonrural","cus_rural","index_nonrural","index_rural"))
cat("\n=== rq2.1  Charter count, first and last quarter ===\n")
print(cnt_w[qidx %in% c(QA, QZ)])                                          ## LOOK
cat(sprintf("Index at end (start = 100): rural %.1f, non-rural %.1f\n",
            cnt_w[qidx == QZ, index_rural], cnt_w[qidx == QZ, index_nonrural]))
fwrite(cnt_w, file.path(OUT, "rq2_1_charter_index.csv"))

## Same thing on the 2013 vintage, to show the answer does not depend on it.
if (!is.na(C$rural13)) {
  alt <- cr[!is.na(get(C$rural13)), .(cus = uniqueN(cu_number)), by = .(qidx, rural = get(C$rural13))]
  alt[, index := 100 * cus / cus[qidx == QA], by = rural]
  cat("2013 vintage, index at end: "); print(alt[qidx == QZ, .(rural, index = round(index, 1))])   ## LOOK
}

F1 <- ggplot(cnt, aes(qidx, index, colour = factor(rural, labels = c("Non-rural","Rural")))) +
  geom_hline(yintercept = 100, colour = "grey60", linetype = 2) + geom_line(linewidth = 1) +
  scale_x_continuous(labels = q_lab, breaks = seq(QA, QZ, by = 8)) +
  labs(title = "Number of credit unions, indexed to the first quarter = 100",
       subtitle = "Rural definition held fixed on the 2024 vintage throughout", x = NULL, y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
ggsave(file.path(OUT, "rq2_1_index.png"), F1, width = 9, height = 5, dpi = 200, bg = "white")

## ---- rq2.2  spells and the exit hazard, counting-process form -------------
## One row per CU with first and last quarter observed. exit = 1 if the CU is
## gone before the panel ends. Institutions alive at QA are LEFT-TRUNCATED:
## we start their clock at QA, not at birth. The counting-process Surv handles
## that correctly; Surv(dur, exit) does not.

sp <- cr[!is.na(rural_fix), .(first_q = min(qidx), last_q = max(qidx),
                              rural = rural_fix[which.max(qidx)],
                              st = state_code[which.max(qidx)],
                              fips_last = fips[which.max(qidx)],
                              ln_assets = log(assets_tot[which.min(qidx)]),
                              networth = networth_pct[which.min(qidx)],
                              roa = roa_pct[which.min(qidx)],
                              lid = max(lid), mdi = max(mdi), fed = max(fed)),
       by = cu_number]
sp[, `:=`(exit = as.integer(last_q < QZ), entered = as.integer(first_q > QA))]
sp[, `:=`(t0 = first_q - QA, t1 = last_q - QA + 1L)]     # counting-process clock from QA
sp <- sp[is.finite(ln_assets) & !is.na(networth) & !is.na(roa)]

cat("\n=== rq2.2  Exits over the panel ===\n")
print(sp[, .(credit_unions = .N, exits = sum(exit), exit_rate_pct = round(100 * mean(exit), 1),
             entered_after_start = sum(entered)), by = rural][order(-rural)])   ## LOOK

cx <- list(
  raw  = coxph(Surv(t0, t1, exit) ~ rural, data = sp),
  size = coxph(Surv(t0, t1, exit) ~ rural + ln_assets + I(ln_assets^2), data = sp),
  full = coxph(Surv(t0, t1, exit) ~ rural + ln_assets + I(ln_assets^2) + networth + roa +
                 lid + mdi + fed + strata(st), data = sp))
hz <- rbindlist(lapply(cx, function(m) {
  ct <- summary(m)$coefficients["rural", ]
  data.table(hazard_ratio = ct["exp(coef)"], se = ct["se(coef)"], p = ct["Pr(>|z|)"])
}), idcol = "spec")
hz[, pct_safer := round(100 * (1 - hazard_ratio), 1)]
cat("\n=== rq2.2  Exit hazard, rural vs non-rural ===\n"); print(hz)      ## LOOK
cat("Compare with 8_q1's Surv(dur, exit) version -- the counting-process form should be close.\n")
cat("If it moves a lot, left truncation was doing work and this is the one to report.\n")

## ---- rq2.3  THE WILLING-ACQUIRER TEST ---------------------------------------
## Add the number of OTHER credit unions in the same county and the same state
## at the CU's last observed quarter. If "rural" was standing in for "no buyers
## nearby", the rural coefficient should collapse toward 1 once these are in.

nb <- cr[!is.na(rural_fix), .(n_cty = .N - 1L), by = .(fips, qidx)]
ns <- cr[!is.na(rural_fix), .(n_st  = .N - 1L), by = .(state_code, qidx)]
sp[nb, on = .(fips_last = fips, last_q = qidx), n_cty := i.n_cty]
sp[ns, on = .(st = state_code, last_q = qidx), n_st := i.n_st]
sp[is.na(n_cty), n_cty := 0L]; sp[is.na(n_st), n_st := 0L]
sp[, `:=`(ln_ncty = log1p(n_cty), ln_nst = log1p(n_st), alone_in_county = as.integer(n_cty == 0L))]

cat("\n=== rq2.3  Potential acquirers nearby, by rurality ===\n")
print(sp[, .(median_cus_in_county = as.numeric(median(n_cty)),
             pct_alone_in_county = round(100 * mean(alone_in_county), 1),
             median_cus_in_state = as.numeric(median(n_st))), by = rural][order(-rural)])   ## LOOK

cx_acq <- list(
  full            = cx$full,
  plus_county     = coxph(Surv(t0, t1, exit) ~ rural + ln_assets + I(ln_assets^2) + networth + roa +
                            lid + mdi + fed + ln_ncty + strata(st), data = sp),
  plus_both       = coxph(Surv(t0, t1, exit) ~ rural + ln_assets + I(ln_assets^2) + networth + roa +
                            lid + mdi + fed + ln_ncty + ln_nst + strata(st), data = sp),
  plus_alone_flag = coxph(Surv(t0, t1, exit) ~ rural + ln_assets + I(ln_assets^2) + networth + roa +
                            lid + mdi + fed + alone_in_county + ln_nst + strata(st), data = sp))
acq <- rbindlist(lapply(cx_acq, function(m) {
  ct <- summary(m)$coefficients
  data.table(rural_hr = ct["rural","exp(coef)"], rural_p = ct["rural","Pr(>|z|)"],
             nearby_hr = if ("ln_ncty" %in% rownames(ct)) ct["ln_ncty","exp(coef)"]
                         else if ("alone_in_county" %in% rownames(ct)) ct["alone_in_county","exp(coef)"] else NA_real_)
}), idcol = "spec")
acq[, rural_pct_safer := round(100 * (1 - rural_hr), 1)]
cat("\n=== rq2.3  Does the rural survival advantage survive conditioning on nearby credit unions? ===\n")
print(acq)                                                                 ## LOOK
cat("nearby_hr > 1 means more neighbours -> HIGHER exit risk (more buyers).\n")
cat("If rural_hr moves toward 1 and loses significance, the survival finding is\n")
cat("partly a thin-market artefact and the report must say so. If it holds, the\n")
cat("willing-acquirer objection is answered.\n")
fwrite(acq, file.path(OUT, "rq2_3_willing_acquirer.csv"))

## ---- rq2.4  flows, with the identity asserted -------------------------------
## dN(rural) = entries - exits + arrivals - departures. Arrivals/departures are
## CUs whose HQ county changed rural status by MOVING (vintage is fixed, so
## there is no map-redrawing term).

life <- cr[!is.na(rural_fix), .(first_q = min(qidx), last_q = max(qidx),
                                r_first = rural_fix[which.min(qidx)], r_last = rural_fix[which.max(qidx)]),
           by = cu_number]
life[, `:=`(entered = first_q > QA, exited = last_q < QZ, crossed = r_first != r_last)]
seg <- function(r) fifelse(r == 1L, "Rural", "Non-rural")

lvl <- cr[!is.na(rural_fix) & qidx %in% c(QA, QZ), .(n = uniqueN(cu_number)), by = .(qidx, seg = seg(rural_fix))]
lvl <- dcast(lvl, seg ~ qidx, value.var = "n"); setnames(lvl, as.character(c(QA, QZ)), c("start","end"))

ent <- life[entered & !exited, .(entries = .N), by = .(seg = seg(r_last))]
exi <- life[exited & !entered, .(exits = .N),   by = .(seg = seg(r_first))]
cb  <- life[crossed & !entered & !exited]
mig <- data.table(seg = c("Rural","Non-rural"),
                  arrivals   = c(cb[r_last == 1L, .N],  cb[r_last == 0L, .N]),
                  departures = c(cb[r_first == 1L, .N], cb[r_first == 0L, .N]))
rec <- Reduce(function(a, b) merge(a, b, by = "seg", all = TRUE), list(lvl, ent, exi, mig))
for (v in c("entries","exits","arrivals","departures")) rec[is.na(get(v)), (v) := 0L]
rec[, `:=`(implied = entries - exits + arrivals - departures, actual = end - start)]
rec[, residual := actual - implied]
cat("\n=== rq2.4  Flow identity ===\n"); print(rec)                        ## LOOK
if (any(rec$residual != 0L)) {
  cat("Residual not zero. Most likely: CUs that entered AND exited inside the window\n")
  cat("(they touch neither endpoint) or interior gaps. Count them:\n")
  print(life[entered & exited, .N, by = .(seg = seg(r_first))])
  warning("Flow identity did not close -- check before quoting entries or exits.")
}

## Event counts (everything that happened, including come-and-go charters) and
## rates per 100 at risk, so the two segments are comparable.
evt <- merge(life[entered == TRUE, .(entries_all = .N), by = .(seg = seg(r_first))],
             life[exited == TRUE,  .(exits_all   = .N), by = .(seg = seg(r_last))], by = "seg")
evt <- merge(evt, lvl, by = "seg")
evt[, `:=`(entries_per_100 = round(100 * entries_all / start, 1),
           exits_per_100   = round(100 * exits_all / start, 1),
           replaced_per_100_exits = round(100 * entries_all / exits_all, 1))]
cat("\n=== rq2.4  Event counts and rates ===\n"); print(evt)               ## LOOK
fwrite(evt, file.path(OUT, "rq2_4_flows.csv"))

## Entries by year, both segments
eby <- life[entered == TRUE, .N, by = .(yr = (first_q - 1L) %/% 4L, seg = seg(r_first))]
cat("\n=== rq2.4  Entries by year ===\n"); print(dcast(eby, yr ~ seg, value.var = "N", fill = 0L))   ## LOOK

## ---- rq2.5  size at first appearance -- the de novo denominator -------------
## A genuine de novo arrives tiny. An "entry" that arrives with $80m is a
## conversion or a re-registration. This is what "47 entries" is actually made of.

fa <- cr[!is.na(rural_fix)][life[entered == TRUE], on = .(cu_number, qidx = first_q),
                             .(cu_number, first_q, rural = rural_fix, assets_first = assets_tot, members_first = members)]
cat("\n=== rq2.5  Entrants by assets at first appearance ===\n")
print(fa[, .(entrants = .N,
             median_assets_m = round(as.numeric(median(assets_first)) / 1e6, 1),
             under_2m = sum(assets_first < 2e6), under_5m = sum(assets_first < 5e6),
             under_25m = sum(assets_first < 25e6), over_100m = sum(assets_first >= 100e6)),
         by = seg(rural)])                                                 ## LOOK
cat("Under $5m at first appearance is a defensible 'genuinely new' cut. Report that\n")
cat("count, not the raw entry count, when the sentence is about de novo formation.\n")
fwrite(fa, file.path(OUT, "rq2_5_entrant_size.csv"))

## ---- rq2.6  where exiting rural charters go ---------------------------------
## Follow the successor pointer. A rural charter absorbed by a NON-rural
## acquirer keeps its offices (11_ showed that) but loses local control. That is
## a different sentence from "the credit union closed".

if (!is.na(C$succ)) {
  ## The pointer (join_number_pointer / acquiredcu) refers to the ACQUIRER's
  ## join_number, not its cu_number. Build the successor lookup on join_number.
  ## If the panel has no join_number the fallback tries cu_number and reports
  ## how many it identified, so a near-zero match is visible rather than silent.
  key_col <- if ("join_number" %in% names(cr)) "join_number" else "cu_number"
  cat("Successor lookup keyed on:", key_col, "\n")

  last_row <- cr[!is.na(rural_fix)][life[exited == TRUE], on = .(cu_number, qidx = last_q),
                                     .(cu_number, last_q, rural_exit = rural_fix, succ = get(C$succ))]
  last_row[, succ := as.character(as.integer(as.numeric(succ)))]     # normalise "12345.0" / " 12345"
  last_row[succ %in% c("0", "NA", ""), succ := NA_character_]

  ## Rural status of the acquirer at the quarter the exiting CU last appeared,
  ## falling back to the acquirer's latest status if it was not observed then.
  succ_q <- cr[!is.na(rural_fix), .(key = as.character(as.integer(get(key_col))), qidx, rural_succ = rural_fix)]
  last_row[succ_q, on = .(succ = key, last_q = qidx), rural_succ := i.rural_succ]
  succ_last <- succ_q[, .(rural_latest = rural_succ[which.max(qidx)]), by = key]
  last_row[succ_last, on = .(succ = key), rural_succ := fifelse(is.na(rural_succ), i.rural_latest, rural_succ)]

  cat("\n=== rq2.6  Successor rurality for exiting charters ===\n")
  print(last_row[, .(exits = .N,
                     pointer_present = sum(!is.na(succ)),
                     successor_identified = sum(!is.na(rural_succ)),
                     successor_rural = sum(rural_succ == 1L, na.rm = TRUE),
                     successor_nonrural = sum(rural_succ == 0L, na.rm = TRUE)),
                 by = .(exiting = seg(rural_exit))])                       ## LOOK
  cat("pointer_present < exits means those exits carry no successor -- liquidations,\n")
  cat("or a pointer recorded in a different field. successor_identified < pointer_present\n")
  cat("means the pointer value did not match any ", key_col, " in the panel.\n", sep = "")
  rr <- last_row[rural_exit == 1L & !is.na(rural_succ)]
  cat(sprintf("\nOf rural exits with an identified successor, %.0f%% went to a NON-rural acquirer.\n",
              100 * mean(rr$rural_succ == 0L)))
  fwrite(last_row, file.path(OUT, "rq2_6_successors.csv"))
} else cat("\nrq2.6 skipped: no successor pointer column found.\n")

## ---- rq2.7  exit composition -- UNVERIFIED until the dictionary is checked --

OUTCOME_LOOKUP <- c()   # fill from the NCUA data dictionary, e.g. c(MC = "Merged", LQ = "Liquidated")
lab <- function(x) {
  x <- as.character(x)
  v <- if (length(OUTCOME_LOOKUP)) unname(OUTCOME_LOOKUP[x]) else rep(NA_character_, length(x))
  fifelse(is.na(v), paste0(x, " [UNVERIFIED]"), v)
}

if (!is.na(C$outcome)) {
  ex_row <- cr[!is.na(rural_fix)][life[exited == TRUE], on = .(cu_number, qidx = last_q),
                                   .(cu_number, rural = rural_fix, outcome = get(C$outcome),
                                     reason = if (!is.na(C$reason)) get(C$reason) else NA)]
  comp <- ex_row[, .N, by = .(seg = seg(rural), outcome = lab(outcome))][order(seg, -N)]
  comp[, pct := round(100 * N / sum(N), 1), by = seg]
  cat("\n=== rq2.7  Exit composition by outcome code ===\n"); print(comp)  ## LOOK
  cat("EVERY LABEL ABOVE IS UNVERIFIED. Fill OUTCOME_LOOKUP from the NCUA data\n")
  cat("dictionary before any of this reaches a draft. '220 mergers' and '220\n")
  cat("failures' are different sentences.\n")
  fwrite(comp, file.path(OUT, "rq2_7_exit_composition.csv"))
}

## ---- rq2.8  county coverage by headquarters --------------------------------

cov <- cr[!is.na(rural_fix) & qidx %in% c(QA, QZ) & rural_fix == 1L,
          .(counties_with_hq = uniqueN(fips)), by = qidx]
lost <- setdiff(cr[rural_fix == 1L & qidx == QA, unique(fips)], cr[rural_fix == 1L & qidx == QZ, unique(fips)])
gain <- setdiff(cr[rural_fix == 1L & qidx == QZ, unique(fips)], cr[rural_fix == 1L & qidx == QA, unique(fips)])
cat("\n=== rq2.8  Rural counties with a credit union HEADQUARTERED in them ===\n")
print(cov)                                                                 ## LOOK
cat(sprintf("Lost their last HQ: %d counties | gained a first HQ: %d counties\n", length(lost), length(gain)))
fwrite(data.table(fips = c(lost, gain), change = rep(c("lost last HQ","gained first HQ"), c(length(lost), length(gain)))),
       file.path(OUT, "rq2_8_county_hq_changes.csv"))
cat("Headquarters only. Offices reach roughly twice as far -- see 14_ for that measure.\n")

## ---- rq2.9  FOR THE SLIDE ---------------------------------------------------

cat("\n================ RQ2 -- SLIDE NUMBERS ================\n")
cat(sprintf("Charter index, end (start=100): rural %.1f | non-rural %.1f\n",
            cnt_w[qidx == QZ, index_rural], cnt_w[qidx == QZ, index_nonrural]))
cat(sprintf("Exit rate: rural %.1f%% | non-rural %.1f%%\n",
            100 * sp[rural == 1, mean(exit)], 100 * sp[rural == 0, mean(exit)]))
cat(sprintf("Hazard ratio, full model: %.3f (%.0f%% safer), p = %.2g\n",
            hz[spec == "full", hazard_ratio], hz[spec == "full", pct_safer], hz[spec == "full", p]))
cat(sprintf("... after conditioning on nearby CUs: %.3f (%.0f%% safer), p = %.2g\n",
            acq[spec == "plus_both", rural_hr], acq[spec == "plus_both", rural_pct_safer], acq[spec == "plus_both", rural_p]))
cat(sprintf("Rural entries %d vs exits %d | per 100 at risk: entries %.1f, exits %.1f\n",
            evt[seg == "Rural", entries_all], evt[seg == "Rural", exits_all],
            evt[seg == "Rural", entries_per_100], evt[seg == "Rural", exits_per_100]))
cat(sprintf("Rural entrants under $5m at first appearance: %d of %d\n",
            fa[rural == 1 & assets_first < 5e6, .N], fa[rural == 1, .N]))
cat(sprintf("Rural counties that lost their last HQ: %d | gained a first: %d\n", length(lost), length(gain)))
cat("======================================================\n")
