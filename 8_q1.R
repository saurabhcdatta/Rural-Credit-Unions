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

## ---- 8.7 read-out -----------------------------------------------------------
cat("\n", strrep("=", 70), "\n  Q1: is 'rural' different from 'small'?\n", strrep("=", 70), "\n", sep = "")
cat("\nShare of raw gap surviving conditioning:\n"); print(shrink[, .(outcome, pct_surviving)])
cat("\nShare of gap unexplained by observables:\n");  print(D[, .(outcome, pct_unexplained)])
cat("\n  small + small  -> SCALE story; promote the cost function work\n")
cat("  large          -> RURAL story; promote the Sec. 909(c)(2) barrier work\n")
cat("  differs by outcome, or concentrated in the small bands (8.4)\n")
cat("                 -> report per-outcome; the likely realistic result\n")

fwrite(res,    file.path(OUT_DIR, "q1_conditional_gap.csv"))
fwrite(shrink, file.path(OUT_DIR, "q1_gap_shrinkage.csv"))
fwrite(D,      file.path(OUT_DIR, "q1_oaxaca.csv"))
saveRDS(est,   file.path(OUT_DIR, "q1_estimation_sample.rds"))
