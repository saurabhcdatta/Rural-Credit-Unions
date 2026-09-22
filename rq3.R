###############################################################################
# rq3.R  --  RESEARCH QUESTION 3
# "Does the cap on business lending actually restrict rural credit unions?"
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# THE STATUTE (12 U.S.C. 1757a)
#   Aggregate member business loans may not exceed the LESSER of
#     (a) 1.75 x actual net worth, or
#     (b) 1.75 x the minimum net worth required to be well capitalized.
#   Well capitalized is 7% net worth, so (b) = 12.25% of assets. For any credit
#   union at or above 7% net worth -- almost all of them -- (b) binds and the cap
#   is effectively 12.25% of total assets. Below 7%, (a) binds and is tighter.
#   It is 1.75 x NET WORTH, not assets. Get that right in every sentence.
#
#   Exempt: credit unions chartered for or with a history of primarily making
#   MBLs; credit unions serving predominantly low-income members (the LID
#   designation); CDFIs. We observe LID. We do NOT observe the other two.
#
# WHAT THIS DOES
#   rq3.1  construct the cap and the MBL-to-cap ratio, non-exempt only
#   rq3.2  how many are near it, rural vs non-rural, and over time
#   rq3.3  BUNCHING -- if the cap changes behaviour, institutions should pile
#          up just below it. We fit a smooth density excluding a window around
#          the cap and measure the excess mass in the window. This is the test
#          that distinguishes "constraint" from "coincidence".
#   rq3.4  exempt vs non-exempt intensity, like-for-like on size and state.
#          The raw three-times gap could be selection; this narrows it.
#   rq3.5  does being near the cap predict slower subsequent loan growth?
#   rq3.6  FOR THE SLIDE
#
# REQUIRES: cr from 3_callreport.R. Uses lns_mbl_part723 (full coverage) and
# networth_tot if present, else networth_pct x assets.
###############################################################################

suppressPackageStartupMessages({
  library(data.table); library(fixest); library(ggplot2)
})

## ---- rq3.0  panel and columns ----------------------------------------------

stopifnot(exists("cr")); if (!is.data.table(cr)) setDT(cr)
OUT <- file.path(if (exists("OUT_DIR")) OUT_DIR else "output", "rq3")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

MBL_COL <- if ("lns_mbl_part723" %in% names(cr)) "lns_mbl_part723" else
           if ("lns_mbl" %in% names(cr)) "lns_mbl" else stop("no MBL column")
cat("MBL column:", MBL_COL, "\n")
if ("networth_tot" %in% names(cr) && cr[, mean(!is.na(networth_tot))] > .9) {
  cr[, nw_dollars := networth_tot]
  cat("Net worth in dollars from networth_tot\n")
} else {
  cr[, nw_dollars := networth_pct / 100 * assets_tot]
  cat("Net worth in dollars reconstructed as networth_pct x assets\n")
}

## ---- rq3.1  the cap and the ratio ------------------------------------------

m <- cr[!is.na(rural) & !is.na(get(MBL_COL)) & assets_tot > 0]
m[, mbl := get(MBL_COL)]
m[, `:=`(cap_a = 1.75 * nw_dollars,             # 1.75 x actual net worth
         cap_b = 1.75 * 0.07 * assets_tot)]     # 1.75 x well-capitalized minimum = 12.25% of assets
m[, cap := pmin(cap_a, cap_b)]
m[, which_binds := fifelse(cap_a < cap_b, "1.75 x net worth (below 7%)", "12.25% of assets")]
m[, `:=`(ratio = fifelse(cap > 0, mbl / cap, NA_real_),
         mbl_pct_assets = 100 * mbl / assets_tot,
         exempt = lid == 1L,
         active = mbl > 0)]

cat("\n=== rq3.1  Which prong binds, among MBL-active credit unions ===\n")
print(m[active == TRUE, .N, by = which_binds][, pct := round(100 * N / sum(N), 1)][])   ## LOOK
cat("Nearly everyone should be on the 12.25% prong. The 1.75 x net worth prong only\n")
cat("binds for institutions under 7% -- and those have bigger problems than the cap.\n")

## The population that matters: NON-exempt, MBL-active, latest quarter
QZ <- max(m$qidx)
ne <- m[exempt == FALSE & active == TRUE]
cat(sprintf("\nNon-exempt MBL-active CU-quarters: %s | latest quarter: %d CUs (%d rural)\n",
            format(nrow(ne), big.mark = ","), ne[qidx == QZ, .N], ne[qidx == QZ & rural == 1, .N]))

## ---- rq3.2  how many are near the cap ---------------------------------------

near <- ne[, .(cus = .N,
               above_80 = sum(ratio >= .80), above_90 = sum(ratio >= .90), at_cap = sum(ratio >= .98),
               pct_above_90 = round(100 * mean(ratio >= .90), 1),
               median_ratio = round(as.numeric(median(ratio)), 3)),
           by = .(qidx, rural)]
cat("\n=== rq3.2  Latest quarter: distance to the cap, non-exempt MBL-active ===\n")
print(near[qidx == QZ][order(-rural)])                                     ## LOOK
cat("\nShare above 90% of the cap, by year:\n")
print(dcast(near[, .(pct = round(100 * sum(above_90) / sum(cus), 1)), by = .(yr = (qidx - 1L) %/% 4L, rural)],
            yr ~ rural, value.var = "pct"))                                 ## LOOK
fwrite(near, file.path(OUT, "rq3_2_near_cap.csv"))

## Also: the population that does NOT do business lending at all. If most rural
## CUs have no MBL book, the cap is irrelevant to them by construction.
cat("\nShare of credit unions with ANY business lending, latest quarter:\n")
print(m[qidx == QZ, .(cus = .N, pct_active = round(100 * mean(active), 1),
                      pct_exempt = round(100 * mean(exempt), 1)), by = rural][order(-rural)])   ## LOOK

## ---- rq3.3  BUNCHING at the cap ---------------------------------------------
## Bin the ratio in 0.02 steps. Fit a polynomial to bin counts EXCLUDING a
## window just below the cap, predict the counterfactual count inside the
## window, and compare. Excess mass >> 0 with a tight CI means institutions are
## managing to the cap. Standard bunching estimator, pooled over quarters.
## The window is [LO, 1.00]; the polynomial is fit on [0.30, 1.30] minus window.

BIN <- 0.02; LO <- 0.90; HI <- 1.00; FIT_RANGE <- c(0.30, 1.30); DEG <- 5L

bunch <- function(d, label) {
  d <- d[ratio >= FIT_RANGE[1] & ratio <= FIT_RANGE[2]]
  d[, bin := floor(ratio / BIN) * BIN]
  h <- d[, .(n = .N), by = bin][order(bin)]
  h[, in_window := bin >= LO & bin < HI]
  f <- lm(n ~ poly(bin, DEG), data = h[in_window == FALSE])
  h[, cf := pmax(predict(f, newdata = h), 0)]
  obs <- h[in_window == TRUE, sum(n)]; cf <- h[in_window == TRUE, sum(cf)]
  ## bootstrap the excess mass over bins
  set.seed(909)
  bs <- replicate(300, {
    hb <- h[sample(.N, replace = TRUE)]
    fb <- lm(n ~ poly(bin, DEG), data = hb[in_window == FALSE])
    h[in_window == TRUE, sum(n)] - sum(pmax(predict(fb, newdata = h[in_window == TRUE]), 0))
  })
  list(table = h, label = label, observed = obs, counterfactual = cf,
       excess = obs - cf, excess_ratio = obs / cf,
       ci = quantile(bs, c(.025, .975)))
}

b_rural <- bunch(ne[rural == 1], "Rural, non-exempt")
b_nonr  <- bunch(ne[rural == 0], "Non-rural, non-exempt")
b_exmpt <- bunch(m[exempt == TRUE & active == TRUE], "LID-exempt (placebo: no cap applies)")

bres <- rbindlist(lapply(list(b_rural, b_nonr, b_exmpt), function(b)
  data.table(group = b$label, observed_in_window = b$observed,
             counterfactual = round(b$counterfactual, 1),
             excess_mass = round(b$excess, 1), obs_over_cf = round(b$excess_ratio, 2),
             ci_lo = round(b$ci[1], 1), ci_hi = round(b$ci[2], 1))))
cat("\n=== rq3.3  Bunching in the window [0.90, 1.00) of the cap ===\n")
print(bres)                                                                ## LOOK
cat("obs_over_cf > 1 with ci_lo > 0 means excess mass just below the cap: the cap\n")
cat("is shaping behaviour. The LID-exempt row is a PLACEBO -- no cap applies to\n")
cat("them, so there should be no bunching. If it bunches too, the 'cap' is\n")
cat("picking up something else (a supervisory threshold, a rounding habit).\n")
fwrite(bres, file.path(OUT, "rq3_3_bunching.csv"))

bt <- rbindlist(list(b_rural$table[, grp := b_rural$label], b_nonr$table[, grp := b_nonr$label],
                     b_exmpt$table[, grp := b_exmpt$label]))
F1 <- ggplot(bt, aes(bin + BIN/2)) +
  annotate("rect", xmin = LO, xmax = HI, ymin = -Inf, ymax = Inf, fill = "#C8553D", alpha = .12) +
  geom_col(aes(y = n), fill = "#7FB0AE", width = BIN * .9) +
  geom_line(aes(y = cf), colour = "#0E4C55", linewidth = .9) +
  geom_vline(xintercept = 1, linetype = 2) +
  facet_wrap(~ grp, scales = "free_y", ncol = 1) +
  labs(title = "Business lending relative to the statutory cap",
       subtitle = "Bars: CU-quarters per bin. Line: smooth counterfactual fitted outside the shaded window.\nA pile-up in the shaded window that the line does not predict is bunching.",
       x = "Member business loans / cap  (1.0 = at the cap)", y = "CU-quarters") +
  theme_minimal(base_size = 12)
ggsave(file.path(OUT, "rq3_3_bunching.png"), F1, width = 9, height = 9, dpi = 200, bg = "white")

## ---- rq3.4  exempt vs non-exempt, like-for-like -----------------------------
## Raw: exempt lend ~3x more. But LID is not random. Condition on size, state
## and quarter and see how much survives. Then split by rurality.

mm <- m[active == TRUE]
mm[, `:=`(qtr = factor(qidx), st = factor(state_code), ln_assets = log(assets_tot))]
ex <- list(
  raw  = feols(mbl_pct_assets ~ exempt | qtr, data = mm, cluster = ~ cu_number),
  size = feols(mbl_pct_assets ~ exempt + ln_assets + I(ln_assets^2) | qtr, data = mm, cluster = ~ cu_number),
  full = feols(mbl_pct_assets ~ exempt + ln_assets + I(ln_assets^2) + rural + fed + mdi | st^qtr, data = mm, cluster = ~ cu_number),
  rural_only = feols(mbl_pct_assets ~ exempt + ln_assets + I(ln_assets^2) + fed + mdi | st^qtr, data = mm[rural == 1], cluster = ~ cu_number))
exres <- rbindlist(lapply(ex, function(mo) {
  ct <- coeftable(mo)["exemptTRUE", ]
  data.table(exempt_effect_pp = round(ct["Estimate"], 2), se = round(ct["Std. Error"], 2), p = signif(ct["Pr(>|t|)"], 2))
}), idcol = "spec")
cat("\n=== rq3.4  MBL as % of assets: exempt minus non-exempt ===\n"); print(exres)   ## LOOK
cat("Raw medians for reference: "); print(mm[qidx == QZ, .(median_mbl_pct = round(as.numeric(median(mbl_pct_assets)), 2)), by = .(rural, exempt)][order(-rural, exempt)])
fwrite(exres, file.path(OUT, "rq3_4_exempt_gap.csv"))

## ---- rq3.5  does being near the cap slow subsequent lending? ---------------
## For non-exempt CUs: is g_loans over the NEXT four quarters lower for those
## at >= 90% of the cap today? If the cap binds, loan growth should stall.

lead4 <- function(x, idx) x[match(idx + 4L, idx)]
ne[, g_loans_fwd := lead4(g_loans, qidx), by = cu_number]
ne[, `:=`(near90 = as.integer(ratio >= .90), qtr = factor(qidx), st = factor(state_code), ln_assets = log(assets_tot))]
nm <- feols(g_loans_fwd ~ near90 * rural + ln_assets + I(ln_assets^2) + networth_pct + roa_pct | st^qtr,
            data = ne[!is.na(g_loans_fwd)], cluster = ~ cu_number)
cat("\n=== rq3.5  Loan growth over the following year, near-cap vs not ===\n")
print(coeftable(nm)[c("near90", "near90:rural"), ])                        ## LOOK
cat("near90 < 0 : institutions at the cap grow loans more slowly afterwards -- the cap\n")
cat("is binding on them. near90:rural tells you whether that is worse in rural areas.\n")

## ---- rq3.6  FOR THE SLIDE ---------------------------------------------------

nz <- near[qidx == QZ]
cat("\n================ RQ3 -- SLIDE NUMBERS ================\n")
cat(sprintf("Rural CUs with any business lending .......... %.0f%%\n", m[qidx == QZ & rural == 1, 100 * mean(active)]))
cat(sprintf("Rural, non-exempt, above 90%% of cap .......... %d of %d (%.1f%%)\n",
            nz[rural == 1, above_90], nz[rural == 1, cus], nz[rural == 1, pct_above_90]))
cat(sprintf("Non-rural, same ............................... %d of %d (%.1f%%)\n",
            nz[rural == 0, above_90], nz[rural == 0, cus], nz[rural == 0, pct_above_90]))
cat(sprintf("Bunching, rural: observed/counterfactual %.2f (CI on excess %.0f to %.0f)\n",
            bres[1, obs_over_cf], bres[1, ci_lo], bres[1, ci_hi]))
cat(sprintf("Bunching placebo (exempt): %.2f\n", bres[3, obs_over_cf]))
cat(sprintf("Exempt lend more by: raw %+.1f pp -> like-for-like %+.1f pp\n",
            exres[spec == "raw", exempt_effect_pp], exres[spec == "full", exempt_effect_pp]))
cat(sprintf("Near-cap next-year loan growth effect: %+.2f pp (p = %.2g)\n",
            coeftable(nm)["near90","Estimate"], coeftable(nm)["near90","Pr(>|t|)"]))
cat("======================================================\n")
