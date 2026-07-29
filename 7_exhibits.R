###############################################################################
# 7_exhibits.R  -- the descriptive story, in the order it has to be argued
#
# Run these BEFORE the econometrics. Each block answers a question a Board
# member or committee staffer would actually ask.
###############################################################################

LATEST <- cr[, max(qidx)]
cur    <- cr[qidx == LATEST]
recent <- cr[qidx > LATEST - 4]
cur[1, .(year, quarter)]                                  ## LOOK -- panel end

EX <- list()

## ---- E1 the rural universe --------------------------------------------------
EX$E1 <- cur[, .(cus = uniqueN(cu_number),
                 members_mn = sum(members, na.rm = TRUE) / 1e6,
                 assets_bn  = sum(assets_tot, na.rm = TRUE) / 1e9,
                 median_assets_mn = median(assets_tot, na.rm = TRUE) / 1e6),
             by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural"))]
EX$E1[, `:=`(pct_cus = round(100 * cus / sum(cus), 1),
             pct_members = round(100 * members_mn / sum(members_mn), 1),
             pct_assets  = round(100 * assets_bn / sum(assets_bn), 1))]
EX$E1                                                     ## LOOK

## ---- E2 the asymmetry (the framing number) ----------------------------------
u <- u24[as.integer(substr(fips, 1, 2)) <= 56]
EX$E2 <- data.table(
  measure = c("Counties", "Population (2020)", "Credit unions", "Members", "Assets"),
  rural = c(u[rural == 1, .N], u[rural == 1, sum(pop, na.rm = TRUE)],
            cur[rural == 1, uniqueN(cu_number)],
            cur[rural == 1, sum(members, na.rm = TRUE)],
            cur[rural == 1, sum(assets_tot, na.rm = TRUE)]),
  total = c(u[, .N], u[, sum(pop, na.rm = TRUE)],
            cur[, uniqueN(cu_number)], cur[, sum(members, na.rm = TRUE)],
            cur[, sum(assets_tot, na.rm = TRUE)]))
EX$E2[, rural_share_pct := round(100 * rural / total, 1)]
EX$E2                                                     ## LOOK

## Half the counties, a small share of the people. Everything about scale
## follows from that gap -- say it aloud in any briefing.

## ---- E3 size distribution ---------------------------------------------------
EX$E3 <- dcast(cur[, .(n = uniqueN(cu_number)), by = .(band, rural)],
               band ~ rural, value.var = "n", fill = 0)
setnames(EX$E3, c("0", "1"), c("nonrural", "rural"))
EX$E3[, `:=`(rural_pct = round(100 * rural / sum(rural), 1),
             nonrural_pct = round(100 * nonrural / sum(nonrural), 1))]
EX$E3                                                     ## LOOK

## ---- E4 trend, indexed ------------------------------------------------------
yr <- cr[quarter == 4 | qidx == LATEST,
         .(cus = uniqueN(cu_number),
           members_mn = sum(members, na.rm = TRUE) / 1e6,
           assets_bn  = sum(assets_tot, na.rm = TRUE) / 1e9),
         by = .(year, segment = fifelse(rural == 1L, "Rural", "Non-rural"))]
setorder(yr, segment, year)
yr[, cu_index := round(100 * cus / first(cus), 1), by = segment]
EX$E4 <- yr
dcast(yr, year ~ segment, value.var = "cu_index")         ## LOOK

## If the two lines track each other, this is system-wide consolidation rather
## than rural decline -- and the report should say so.

## ---- E5 entries and exits ---------------------------------------------------
fl <- cr[, .(first_q = min(qidx), last_q = max(qidx),
             seg = fifelse(rural[which.max(qidx)] == 1L, "Rural", "Non-rural")),
         by = cu_number]
pe <- cr[, max(qidx)]; ps <- cr[, min(qidx)]
ent <- fl[first_q > ps, .(entries = .N), by = .(year = first_q %/% 4L, seg)]
ext <- fl[last_q  < pe, .(exits   = .N), by = .(year = last_q  %/% 4L, seg)]
EX$E5 <- merge(ent, ext, by = c("year", "seg"), all = TRUE)
setnafill(EX$E5, fill = 0, cols = c("entries", "exits"))
EX$E5[, net := entries - exits]
dcast(EX$E5, year ~ seg, value.var = c("entries", "exits"))            ## LOOK

## 'exits' pools mergers, liquidations and conversions -- split with the
## outcome/reason fields before this goes in the report.

## ---- E6 performance, pooled -------------------------------------------------
pv <- intersect(c("g_assets","g_members","g_loans","networth_pct","roa_pct",
                  "netintmrg","dq_rate"), names(cr))
EX$E6 <- rbind(
  recent[, c(.(basis = "median"), lapply(.SD, median, na.rm = TRUE)),
         by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural")), .SDcols = pv],
  recent[, c(.(basis = "asset-weighted mean"),
             lapply(.SD, function(x) weighted.mean(x, assets_tot, na.rm = TRUE))),
         by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural")), .SDcols = pv])
EX$E6                                                     ## LOOK

## ---- E7 performance WITHIN size band (the most important table) -------------
pbs <- recent[, lapply(.SD, median, na.rm = TRUE),
              by = .(band, rural), .SDcols = c("g_assets","networth_pct","roa_pct")]
EX$E7 <- dcast(melt(pbs, id.vars = c("band", "rural")), band + variable ~ rural,
               value.var = "value")
setnames(EX$E7, c("0", "1"), c("nonrural", "rural"))
EX$E7[, gap := round(rural - nonrural, 3)]
setorder(EX$E7, variable, band)
EX$E7                                                     ## LOOK

## Read `gap` across bands within each outcome. Shrinking toward zero = a size
## story. Stable and non-zero = a rural story. This answers Q1 before any
## regression runs.

## ---- E8 geography -----------------------------------------------------------
EX$E8 <- cur[, .(rural_cus = sum(rural == 1L), total_cus = .N,
                 rural_assets_bn = sum(assets_tot[rural == 1L], na.rm = TRUE) / 1e9),
             by = .(state = state_code)][order(-rural_cus)]
EX$E8[, rural_share_pct := round(100 * rural_cus / total_cus, 1)]
head(EX$E8, 15)                                           ## LOOK

## ---- E9 designations --------------------------------------------------------
## LID credit unions are EXEMPT from the statutory MBL cap, so a high rural LID
## rate would mean much of the rural population is already outside it.
## `shortform` is omitted: 100% NA in this build.
EX$E9 <- cur[, .(cus = uniqueN(cu_number),
                 lid_pct = round(100 * mean(lid), 1),
                 mdi_pct = round(100 * mean(mdi), 1),
                 federal_pct = round(100 * mean(fed), 1)),
             by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural"))]
EX$E9                                                     ## LOOK

## ---- E10 MBL against the statutory cap --------------------------------------
## 12 U.S.C. 1757a: the lesser of 1.75x net worth, or 1.75x the 7% well-
## capitalised minimum (= 12.25% of assets). Which prong binds depends on the
## institution's net worth ratio, so the cap is INSTITUTION-SPECIFIC.
## lns_mbl_part723 is used because it is the Part 723 statutory concept and
## covers the whole panel; lns_comm starts in 2018 and is a supervisory
## category, not the cap measure.
m <- cur[!is.na(lns_mbl_part723) & !is.na(networth_tot) & assets_tot > 0]
m[, cap := pmin(1.75 * networth_tot, 0.1225 * assets_tot)]
m[, `:=`(util = fifelse(cap > 0, lns_mbl_part723 / cap, NA_real_),
         exempt = lid == 1L)]

EX$E10 <- m[, .(cus = uniqueN(cu_number),
                pct_with_mbl = round(100 * mean(lns_mbl_part723 > 0), 1),
                pct_over_50 = round(100 * mean(util > 0.50, na.rm = TRUE), 1),
                pct_over_75 = round(100 * mean(util > 0.75, na.rm = TRUE), 1),
                pct_over_90 = round(100 * mean(util > 0.90, na.rm = TRUE), 1)),
            by = .(segment = fifelse(rural == 1L, "Rural", "Non-rural"),
                   status  = fifelse(exempt, "LID - exempt", "subject to cap"))]
setorder(EX$E10, status, segment)
EX$E10                                                    ## LOOK

## Compare exempt vs non-exempt utilisation. If exempt institutions run far
## higher, the cap is changing behaviour -- evidence from a comparison rather
## than an assertion. If few non-exempt rural CUs are near their cap, the cap
## is not what limits rural business lending, and Q3 should say so.

## ---- E11 how much of 'rural' is the vintage ---------------------------------
EX$E11 <- cur[rural_2013 != rural_2024,
              .(cus = uniqueN(cu_number),
                assets_bn = sum(assets_tot, na.rm = TRUE) / 1e9,
                members = sum(members, na.rm = TRUE)),
              by = .(moved = fifelse(rural_2013 == 1L, "lost rural", "gained rural"))]
EX$E11                                                    ## LOOK

## ---- write out --------------------------------------------------------------
ex_dir <- file.path(OUT_DIR, "exhibits"); dir.create(ex_dir, showWarnings = FALSE)
for (n in names(EX)) fwrite(EX[[n]], file.path(ex_dir, paste0(n, ".csv")))
list.files(ex_dir)                                        ## LOOK

cat("\nExhibits written. Next: 8_q1.R\n")
