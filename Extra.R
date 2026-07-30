fl <- cr[, .(last_q = max(qidx), seg = fifelse(rural[which.max(qidx)] == 1L, "Rural", "Urban"),
             out = outcome[which.max(qidx)], rsn = reason[which.max(qidx)]), by = cu_number]
fl <- fl[last_q < cr[, max(qidx)]]
fl[, .N, by = .(seg, out)][order(seg, -N)]
fl[seg == "Rural", .N, by = rsn][order(-N)][1:10]

cap <- cr[qidx > cr[, max(qidx)] - 4 & !is.na(g_assets) & networth_pct > 0]
cap[, `:=`(g_sust = 100 * roa_pct / networth_pct)]
cap[, binding := as.integer(g_assets > g_sust)]
cap[, .(cus = uniqueN(cu_number),
        median_actual = round(median(g_assets, na.rm = TRUE), 2),
        median_sustainable = round(median(g_sust, na.rm = TRUE), 2),
        pct_above_sustainable = round(100 * mean(binding), 1)),
    by = .(seg = fifelse(rural == 1L, "Rural", "Urban"))]