###############################################################################
# 8_q1.R  -- EXPLORATORY QUESTION 1: is "rural" different from "small"?
#
# DECISION RULE, written down before running:
#   gap largely disappears once size is accounted for
#       -> reorient the report around SCALE; the cost function becomes central
#   gap survives conditioning
#       -> rural-specific barriers become central
#
# E7 in the previous script already answers this without econometrics. What
# follows is confirmation, plus a decomposition for the executive summary.
###############################################################################

library(fixest)

est <- cr[acq_window == 0 & !is.na(g_assets) & !is.na(networth_pct) & !is.na(roa_pct)]
est[, .(cu_quarters = .N, cus = uniqueN(cu_number),
        rural_cus = uniqueN(cu_number[rural == 1]))]      ## LOOK

## ---- 8.1 COMMON SUPPORT -- read this before any regression ------------------
## If rural CUs sit in a size range non-rural CUs barely occupy, the
## regressions below extrapolate rather than compare.
est[, .(cus = uniqueN(cu_number),
        p10 = quantile(assets_tot, .10), p50 = quantile(assets_tot, .50),
        p90 = quantile(assets_tot, .90), max = max(assets_tot)), by = rural]   ## LOOK

rng <- est[rural == 1, range(ln_assets)]
round(100 * est[rural == 0, mean(ln_assets >= rng[1] & ln_assets <= rng[2])], 1)  ## LOOK

## Below ~70%: lead with m5 (common support) rather than m4.

## ---- 8.2 raw vs conditional -------------------------------------------------
## NOTE ON FIXED EFFECTS: rural is defined by HQ county, so county FE would
## absorb it entirely and beta would not be identified. State x quarter is the
## tightest feasible specification under a county-based definition.
CTRL <- "ln_assets + I(ln_assets^2) + age_q + lid + mdi + fed + cecl_flag"

fit <- function(y, rhs, fe, d = est)
  feols(as.formula(sprintf("%s ~ %s | %s", y, rhs, fe)), data = d,
        cluster = ~ cu_number + fips)

## start with one outcome and actually look at the models
m1 <- fit("g_assets", "rural", "qtr")
m2 <- fit("g_assets", "rural + ln_assets + I(ln_assets^2)", "qtr")
m3 <- fit("g_assets", paste("rural", CTRL, sep = " + "), "qtr")
m4 <- fit("g_assets", paste("rural", CTRL, sep = " + "), "st^qtr")
m5 <- fit("g_assets", paste("rural", CTRL, sep = " + "), "st^qtr",
          est[ln_assets >= rng[1] & ln_assets <= rng[2]])
etable(m1, m2, m3, m4, m5, keep = "rural")                ## LOOK

## ---- 8.3 all outcomes, all specifications -----------------------------------
OUT <- c("g_assets", "networth_pct", "roa_pct", "g_members", "g_loans")
SPEC <- list(
  m1_raw   = function(y) fit(y, "rural", "qtr"),
  m2_size  = function(y) fit(y, "rural + ln_assets + I(ln_assets^2)", "qtr"),
  m3_full  = function(y) fit(y, paste("rural", CTRL, sep = " + "), "qtr"),
  m4_state = function(y) fit(y, paste("rural", CTRL, sep = " + "), "st^qtr"),
  m5_supp  = function(y) fit(y, paste("rural", CTRL, sep = " + "), "st^qtr",
                             est[ln_assets >= rng[1] & ln_assets <= rng[2]]))

res <- rbindlist(lapply(names(SPEC), function(s)
  rbindlist(lapply(OUT, function(y) {
    ct <- coeftable(SPEC[[s]](y))["rural", ]
    data.table(spec = s, outcome = y, beta = ct[1], se = ct[2], p = ct[4])
  }))))
dcast(res, outcome ~ spec, value.var = "beta")            ## LOOK

## THE HEADLINE NUMBER: how much of the raw gap survives full conditioning
shrink <- dcast(res, outcome ~ spec, value.var = "beta")
shrink[, pct_surviving := round(100 * m4_state / m1_raw, 1)]
shrink[, .(outcome, m1_raw, m4_state, pct_surviving)]     ## LOOK

## ---- 8.4 does the gap vary BY size? -----------------------------------------
## If the rural effect is concentrated in small CUs, "rural" and "small"
## interact rather than compete -- and the report has to say so. This is the
## most likely real-world answer.
for (y in c("g_assets", "networth_pct", "roa_pct")) {
  mm <- feols(as.formula(sprintf(
        "%s ~ rural * band + age_q + lid + mdi + fed + cecl_flag | st^qtr", y)),
        data = est, cluster = ~ cu_number + fips)
  cat("\n==== ", y, " ====\n")
  print(coeftable(mm)[grep("rural", rownames(coeftable(mm))), ])
}                                                         ## LOOK

## ---- 8.5 Oaxaca-Blinder decomposition ---------------------------------------
## Splits the raw gap into:
##   EXPLAINED   -- rural CUs differ in observable characteristics (mostly size)
##   UNEXPLAINED -- they perform differently GIVEN those characteristics
## The unexplained share is the policy-relevant object and belongs in the
## executive summary.
XV <- c("ln_assets", "I(ln_assets^2)", "age_q", "lid", "mdi", "fed", "cecl_flag")

ob <- function(y) {
  f  <- as.formula(paste(y, "~", paste(XV, collapse = " + ")))
  d0 <- est[rural == 0]; d1 <- est[rural == 1]
  b0 <- coef(lm(f, d0)); b1 <- coef(lm(f, d1))
  X0 <- colMeans(model.matrix(f, d0)); X1 <- colMeans(model.matrix(f, d1))
  gap <- sum(X0 * b0) - sum(X1 * b1)
  data.table(outcome = y, raw_gap = gap,
             explained   = sum((X0 - X1) * b1),
             unexplained = sum(X1 * (b0 - b1)),
             pct_unexplained = round(100 * sum(X1 * (b0 - b1)) / gap, 1))
}
D <- rbindlist(lapply(c("g_assets", "networth_pct", "roa_pct"), ob))
D                                                         ## LOOK

## ---- 8.6 robustness on S2 (service area) ------------------------------------
## Only worth running if S1 and S2 disagreed materially in 6.5. If agreement
## was above ~95%, this is a single appendix row.
est2 <- cr2[acq_window == 0 & !is.na(S2_majority) &
            !is.na(g_assets) & !is.na(networth_pct) & !is.na(roa_pct)]
if (nrow(est2)) {
  r2 <- rbindlist(lapply(c("g_assets", "networth_pct", "roa_pct"), function(y) {
    mm <- feols(as.formula(sprintf("%s ~ S2_majority + %s | st^qtr", y, CTRL)),
                data = est2, cluster = ~ cu_number + fips)
    ct <- coeftable(mm)["S2_majority", ]
    data.table(outcome = y, beta_S2 = ct[1], se = ct[2], p = ct[4])
  }))
  merge(r2, shrink[, .(outcome, beta_S1 = m4_state)], by = "outcome")   ## LOOK
}
## Similar coefficients under both definitions = the finding is not an artifact
## of where charters are domiciled.

## ---- 8.7 survivorship -- the main threat to everything above ----------------
## A third of the 2010 rural cohort is gone. If weak rural CUs exited faster,
## the survivors would look strong for reasons that have nothing to do with
## rurality, and the 8.2-8.5 results would be conditional on survival -- a much
## weaker claim. This is the check that decides whether the finding holds.
##
## Read the rural hazard ratio (exp(coef)):
##   at or below 1  -> rural CUs are NOT selectively exiting; the advantage
##                     cannot be survivorship, and 8.2-8.5 stand as written
##   well above 1   -> restate the performance result as conditional on
##                     survival, and put the hazard ratio next to it

library(survival)

panel_end   <- cr[, max(qidx)]
panel_start <- cr[, min(qidx)]

## one row per credit union; exit = stopped reporting before the panel ends
sv <- cr[, .(first_q   = min(qidx),
             last_q    = max(qidx),
             rural     = rural[which.max(qidx)],
             ln_assets = mean(ln_assets, na.rm = TRUE),
             networth_pct = mean(networth_pct, na.rm = TRUE),
             roa_pct   = mean(roa_pct, na.rm = TRUE),
             lid = max(lid), mdi = max(mdi), fed = max(fed),
             st  = st[which.max(qidx)]), by = cu_number]
sv[, `:=`(exit = as.integer(last_q < panel_end),
          dur  = last_q - first_q + 1L,
          left_censored = as.integer(first_q == panel_start))]

## as.numeric() on the median: median() returns integer for an odd-length
## group and double for an even one, and data.table will not reconcile the two
## across groups.
sv[, .(credit_unions = .N, exits = sum(exit),
       exit_rate_pct = round(100 * mean(exit), 1),
       median_quarters = as.numeric(median(dur))), by = rural]   ## LOOK

## ---- 8.7a raw vs conditional hazard -----------------------------------------
## Raw first, then conditional on size -- same logic as 8.2. Rural CUs are
## smaller and small CUs exit more, so the raw hazard will overstate.
cx1 <- coxph(Surv(dur, exit) ~ rural, data = sv)
cx2 <- coxph(Surv(dur, exit) ~ rural + ln_assets + I(ln_assets^2), data = sv)
cx3 <- coxph(Surv(dur, exit) ~ rural + ln_assets + I(ln_assets^2) +
               networth_pct + roa_pct + lid + mdi + fed + strata(st), data = sv)

hz <- rbindlist(lapply(list(raw = cx1, size = cx2, full = cx3), function(m) {
  ct <- summary(m)$coefficients["rural", ]
  data.table(hazard_ratio = ct["exp(coef)"], se = ct["se(coef)"], p = ct["Pr(>|z|)"])
}), idcol = "spec")
hz[, pct_change := round(100 * (hazard_ratio - 1), 1)]
hz                                                          ## LOOK

## ---- 8.7b does the survivor pool explain the performance gap? ---------------
## Re-run the headline growth spec on BALANCED survivors only -- CUs present
## for the whole panel. If the rural coefficient holds there, selection into
## the survivor pool is not what is generating it.
always <- sv[first_q == panel_start & last_q == panel_end, cu_number]
est_bal <- est[cu_number %in% always]

bal <- rbindlist(lapply(c("g_assets", "networth_pct", "roa_pct"), function(y) {
  mm <- fit(y, paste("rural", CTRL, sep = " + "), "st^qtr", est_bal)
  ct <- coeftable(mm)["rural", ]
  data.table(outcome = y, beta_balanced = ct[1], se = ct[2], p = ct[4])
}))
merge(bal, shrink[, .(outcome, beta_full = m4_state)], by = "outcome")   ## LOOK
cat("\nCoefficients close to the full-sample ones => survivorship is not\n",
    "driving the result. Much smaller => restate 8.2-8.5 as conditional\n",
    "on survival and say so in the report.\n", sep = "")

fwrite(hz,  file.path(OUT_DIR, "q1_survival_hazard.csv"))
fwrite(bal, file.path(OUT_DIR, "q1_balanced_panel_check.csv"))

## ---- 8.8 size-matched pairs -- the executive-legible version ----------------
## Everything above controls for size statistically. This does it literally:
## pair each rural credit union with the non-rural credit union closest to it in
## size, IN THE SAME QUARTER, keep only pairs within 10% on assets, and average
## the difference. No model, no functional form, no coefficients -- just
## like-for-like pairs.
##
## For a non-technical audience this is the one to show. "We matched each rural
## credit union to a non-rural credit union of almost identical size in the same
## quarter, and compared them" needs no explanation of what a control variable is.

PV <- c("g_assets", "networth_pct", "roa_pct")

rr <- est[rural == 1L, c(.(qidx, ln_assets, cu_number, assets_tot), .SD), .SDcols = PV]
nn <- est[rural == 0L, c(.(qidx, ln_assets, nl = ln_assets, n_cu = cu_number), .SD), .SDcols = PV]
setnames(nn, PV, paste0("n_", PV))
setkey(nn, qidx, ln_assets)

## rolling join on size within quarter: nearest non-rural neighbour
mp <- nn[rr, on = .(qidx, ln_assets), roll = "nearest"]
mp[, size_gap := abs(nl - ln_assets)]          # log difference in assets

CAL <- log(1.10)                                # 10% caliper
mpk <- mp[size_gap <= CAL]

cat("\n--- size-matched pairs ---\n")
cat("rural CU-quarters:            ", nrow(mp), "\n", sep = "")
cat("matched within 10% on assets: ", nrow(mpk),
    "  (", round(100 * nrow(mpk) / nrow(mp), 1), "%)\n", sep = "")
cat("median size difference in a matched pair: ",
    round(100 * (exp(mpk[, median(size_gap)]) - 1), 2), "%\n\n", sep = "")

matched <- rbindlist(lapply(PV, function(v) {
  d <- mpk[!is.na(get(v)) & !is.na(get(paste0("n_", v)))]
  diff <- d[[v]] - d[[paste0("n_", v)]]
  tt <- t.test(diff)
  data.table(outcome = v, pairs = length(diff),
             rural_mean = mean(d[[v]]), matched_nonrural_mean = mean(d[[paste0("n_", v)]]),
             difference = mean(diff), ci_lo = tt$conf.int[1], ci_hi = tt$conf.int[2],
             p = tt$p.value)
}))
matched                                                     ## LOOK

cat("\nCompare `difference` here with the regression coefficients in 8.3.\n")
cat("If the two agree, the finding does not depend on any modelling choice --\n")
cat("which is the point worth making to a non-technical audience.\n")

fwrite(matched, file.path(OUT_DIR, "q1_size_matched_pairs.csv"))

## ---- 8.9 read-out -----------------------------------------------------------
cat("\n", strrep("=", 70), "\n  Q1: is 'rural' different from 'small'?\n", strrep("=", 70), "\n", sep = "")
cat("\nShare of raw gap surviving conditioning:\n"); print(shrink[, .(outcome, pct_surviving)])
cat("\nShare of gap unexplained by observables:\n");  print(D[, .(outcome, pct_unexplained)])
cat("\nExit hazard for rural CUs (1.0 = same as non-rural):\n"); print(hz[, .(spec, hazard_ratio, p)])
cat("\n  small + small  -> SCALE story; promote the cost function work\n")
cat("  large          -> RURAL story; promote the Sec. 909(c)(2) barrier work\n")
cat("  differs by outcome, or concentrated in the small bands (8.4)\n")
cat("                 -> report per-outcome; the likely realistic result\n")

fwrite(res,    file.path(OUT_DIR, "q1_conditional_gap.csv"))
fwrite(shrink, file.path(OUT_DIR, "q1_gap_shrinkage.csv"))
fwrite(D,      file.path(OUT_DIR, "q1_oaxaca.csv"))
saveRDS(est,   file.path(OUT_DIR, "q1_estimation_sample.rds"))
