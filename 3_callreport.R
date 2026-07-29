###############################################################################
# 3_callreport.R  -- load the Stata panel, classify rural, build outcomes
#
# The .dta is ~4.8 GB with ~275 columns. Reading it whole can exceed RAM, so
# only the columns the study uses are read -- roughly 45 of them.
###############################################################################

## ---- 3.1 what is in the file? (header only, instant) ------------------------
hdr <- haven::read_dta(PANEL_FILE, n_max = 0)
ncol(hdr)                                                 ## LOOK -- ~275
round(file.size(PANEL_FILE) / 1e9, 2)                     ## LOOK -- GB

## Confirm the Stata coding of the flags before they become regression controls
for (v in c("cu_type", "limited_inc", "ismdi", "shortform")) {
  if (v %in% names(hdr)) { cat("\n", v, ":\n", sep = ""); print(attr(hdr[[v]], "labels")) }
}                                                         ## LOOK

## ---- 3.2 columns to read ----------------------------------------------------
KEEP <- c("cu_number","join_number","year","quarter","cycle_date",
          "state","state_code","county_code","city","zip_code_char5","smsa",
          "cu_type","limited_inc","ismdi","shortform","foicu_impute","cecl",
          "assets_tot","assets_avg","members","members_pot","fte",
          "lns_tot","dep_tot","networth","networth_tot","pcanetworth","subdebt",
          "lns_mbl","lns_mbl_part723","lns_comm","lns_re_1_tot","lns_auto","lns_cc",
          "roa","netmarg","netintmrg","costfds","yldavgloans",
          "dq_rate","dq_mbl","dq_comm","chg_tot_ratio","netchgoffs",
          "outcome","reason","acquiredcu","acquiredcu_ct","acquiredcu_ytd")
KEEP <- intersect(KEEP, names(hdr))
setdiff(c("cu_number","year","quarter","assets_tot","networth","roa","members",
          "lns_tot","state_code","county_code"), names(hdr))   ## LOOK -- must be empty

## ---- 3.3 read (slow once, then cached) --------------------------------------
cr_cache <- file.path(DATA_DIR, "_cr_panel.rds")
if (file.exists(cr_cache)) {
  cr <- readRDS(cr_cache)
} else {
  cr <- haven::read_dta(PANEL_FILE, col_select = tidyselect::all_of(KEEP))
  cr <- haven::zap_labels(cr); cr <- haven::zap_formats(cr)
  setDT(cr)
  saveRDS(cr, cr_cache)
}
dim(cr)                                                   ## LOOK
cr[1:3]                                                   ## LOOK

## ---- 3.4 period index and FIPS ----------------------------------------------
cr[, qidx := as.integer(year) * 4L + as.integer(quarter)]
setorder(cr, cu_number, qidx)
cr[, .(cus = uniqueN(cu_number), quarters = uniqueN(qidx), rows = .N)]   ## LOOK

cr[, fips := sprintf("%02d%03d", as.integer(state_code), as.integer(county_code))]
cr[, .N, by = .(bad = substr(fips, 3, 5) == "000")]       ## LOOK -- missing county codes

## map historical FIPS forward
cr[FIPS_FIX, on = .(fips = old), fips := i.new]
cr[fips %in% FIPS_FIX$new, .N]                            ## LOOK

## ---- 3.5 attach rural -------------------------------------------------------
cr[u24[, .(fips, r = rural)], on = "fips", rural_2024 := i.r]
cr[u13[, .(fips, r = rural)], on = "fips", rural_2013 := i.r]

## Connecticut: pre-2022 records carry old county FIPS the 2024 UIC lacks.
## Section 2.5 established no CT geography is rural under either vintage.
cr[substr(fips, 1, 2) == "09" & is.na(rural_2024), rural_2024 := 0L]
cr[substr(fips, 1, 2) == "09" & is.na(rural_2013), rural_2013 := 0L]

cr[, rural := rural_2024]
round(100 * mean(!is.na(cr$rural)), 2)                    ## LOOK -- match rate

## why are the rest unmatched?
cr[is.na(rural), .N, by = .(reason = fifelse(substr(fips, 3, 5) == "000", "county code missing",
                            fifelse(as.integer(substr(fips, 1, 2)) > 56, "territory",
                                    "unrecognised FIPS")))]         ## LOOK
cr[is.na(rural), .N, by = fips][order(-N)][1:10]                    ## LOOK

## keep the dropped rows so they can be inspected later
fwrite(cr[is.na(rural), .(cu_number, year, quarter, fips, state, city, assets_tot)],
       file.path(DATA_DIR, "_dropped_unclassified.csv"))
cr <- cr[!is.na(rural)]

## ---- 3.6 sample filters -----------------------------------------------------
cr <- cr[year >= 2010 & assets_tot >= 1e6]

## Mergers: acquirers post mechanical growth spikes. Flag the acquisition
## quarter and the four that follow.
cr[, acq := as.integer(!is.na(acquiredcu_ct) & acquiredcu_ct > 0)]
cr[, acq_window := as.integer(frollsum(acq, 5, align = "right", fill = 0) > 0), by = cu_number]
cr[, .N, by = acq_window]                                 ## LOOK

## Short-form: the field exists but is entirely NA in this build, so the
## short-form sensitivity test cannot be run. Confirm and move on.
table(cr$shortform, useNA = "ifany")                      ## LOOK

## ---- 3.7 units --------------------------------------------------------------
cr[, .(med_networth = median(networth, na.rm = TRUE),
       med_roa      = median(roa,      na.rm = TRUE))]    ## LOOK
## networth ~11 and roa ~0.7 => already percentages. If they came back as
## ~0.11 and ~0.007 they are decimals; set SCALE <- 100.
SCALE <- 1
cr[, `:=`(networth_pct = networth * SCALE, roa_pct = roa * SCALE)]

## ---- 3.8 field coverage by year (the schedule breaks) -----------------------
cov_vars <- intersect(c("lns_mbl","lns_mbl_part723","lns_comm","networth",
                        "pcanetworth","dq_mbl","dq_comm","subdebt"), names(cr))
cr[, lapply(.SD, function(x) round(100 * mean(!is.na(x)))),
   by = year, .SDcols = cov_vars][order(year)]            ## LOOK

## Expect: lns_mbl ends 2017 / lns_comm begins 2018; dq_mbl ends 2018 /
## dq_comm begins 2019; pcanetworth stops after 2021. lns_mbl_part723 and
## networth run the whole panel -- use those for the MBL cap and capital work.

## ---- 3.9 outcomes -----------------------------------------------------------
lagq <- function(x, k, idx) x[match(idx - k, idx)]
cr[, `:=`(assets_l4  = lagq(assets_tot, 4L, qidx),
          members_l4 = lagq(members,    4L, qidx),
          lns_l4     = lagq(lns_tot,    4L, qidx)), by = cu_number]
cr[, `:=`(g_assets  = 100 * (assets_tot / assets_l4  - 1),
          g_members = 100 * (members    / members_l4 - 1),
          g_loans   = 100 * (lns_tot    / lns_l4     - 1))]

wins <- function(x, p = c(.01, .99)) {
  q <- quantile(x, p, na.rm = TRUE); pmin(pmax(x, q[1]), q[2])
}
OUTCOMES <- c("g_assets","networth_pct","roa_pct","g_members","g_loans")
cr[, (OUTCOMES) := lapply(.SD, wins), .SDcols = OUTCOMES]

cr[, `:=`(ln_assets = log(assets_tot),
          lid = as.integer(limited_inc %in% c(1, "1", "Y", TRUE)),
          mdi = as.integer(ismdi       %in% c(1, "1", "Y", TRUE)),
          fed = as.integer(cu_type     %in% c(1, "1")),
          cecl_flag = as.integer(cecl  %in% c(1, "1", "Y", TRUE)),
          qtr = factor(qidx),
          st  = factor(state_code))]

cr[, first_q := min(qidx), by = cu_number]
cr[, age_q := qidx - first_q]

BANDS <- c(0, 10e6, 50e6, 100e6, 500e6, 1e9, Inf)
BLAB  <- c("<$10M","$10-50M","$50-100M","$100-500M","$500M-1B",">$1B")
cr[, band := cut(assets_tot, BANDS, labels = BLAB, right = FALSE)]

cr[, .(cus = uniqueN(cu_number), rows = .N), by = rural]  ## LOOK
cr[qidx == max(qidx), .(cus = uniqueN(cu_number)), by = rural]   ## LOOK -- latest quarter

cat("\nCall Report panel ready as `cr`. Next: 4_branch_download.R\n")
