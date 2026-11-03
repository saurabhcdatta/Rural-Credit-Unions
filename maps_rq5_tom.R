###############################################################################
# maps_rq5_tom.R  --  Map 15 (what counts as rural), Map 16 (who serves rural
#                     America, by membership type) and the TOM breakdown
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# WHAT EACH EXHIBIT SAYS
#   Map 15  The definitions as rings around the same places. Darkest = rural
#           under every definition; middle = rural under the statute but next
#           to a metro; lightest = outside a metro but not rural under the
#           statute (the counties the wide definition adds in RQ5).
#   Map 16  Every rural credit union headquarters, coloured by field-of-
#           membership type (TOM_CODE): community, multiple common bond,
#           single bond (employer), single bond (association), other.
#   Chart   The rural growth and ROA advantage WITHIN each membership type --
#           is the advantage a community-charter story, or does it hold for all?
#
# REQUIRES  maps.R run through MAP 1 in this session (cty_sf, st_sf, cent_dt,
#           TH, CAP, sv, fac, leg, palette constants, K, MAP_DIR), plus cr,
#           county, PANEL_FILE and RURAL_2024 from setup.
#           Packages: dplyr, ggplot2, sf, fixest, haven, data.table.
#
# Sequential blocks. Run each, read what it prints (## LOOK), then move on.
###############################################################################

suppressPackageStartupMessages({
  library(data.table); library(dplyr); library(ggplot2); library(sf); library(fixest)
})
if (!exists("assert_universe")) source(file.path(CODE_DIR, "universe.R"))
assert_universe(cr, "maps_rq5_tom.R")
stopifnot("run maps.R through MAP 1 first" = exists("cty_sf") && exists("cent_dt") && exists("TH"),
          "PANEL_FILE not set -- run the setup script" = exists("PANEL_FILE"),
          "RURAL_2024 not set -- run the setup script" = exists("RURAL_2024"))
QZ <- max(cr$qidx)
if (!exists("METRO_2024")) METRO_2024 <- c(1L, 4L)   # ERS 2024 UIC: 1 large metro, 4 small metro (code 2 is micropolitan)
if (!exists("q_lab")) q_lab <- function(q) sprintf("%dQ%d", (q - 1L) %/% 4L, ((q - 1L) %% 4L) + 1L)

## ---- T1  MAP 15: what counts as rural, as rings -----------------------------------
## Three shades of one hue (light -> dark), so the nesting reads without a key
## and stays clear for colour-blind readers. Metro counties are the background.
RING_COLS <- c("Metro county"                                   = C_NONRURAL,
               "Outside a metro, but not rural under the statute" = "#D9C9A3",
               "Rural under the statute, next to a metro"       = "#B08D57",
               "Rural under every definition (remote)"          = "#5C4326")
K15 <- county %>%
  as_tibble() %>%
  filter(is_state_or_dc == TRUE) %>%
  mutate(ring = case_when(
    rural24 == 1L & adj_class %in% c("remote micropolitan", "remote") ~ "Rural under every definition (remote)",
    rural24 == 1L                                                     ~ "Rural under the statute, next to a metro",
    !uic24 %in% METRO_2024                                            ~ "Outside a metro, but not rural under the statute",
    TRUE                                                              ~ "Metro county"),
    ring = fac(ring, names(RING_COLS))) %>%
  select(fips, ring)
K15 %>% count(ring) %>% print()                                                  ## LOOK -- county counts per ring

## credit unions in each ring at the latest quarter -- the same counts as RQ5
cu_ring <- cr %>%
  as_tibble() %>%
  filter(qidx == QZ) %>%
  select(cu_number, fips, rural) %>%
  left_join(county %>% as_tibble() %>% select(fips, uic24, adj_class), by = "fips")
n_statute  <- sum(cu_ring$rural == 1L)
n_remote   <- sum(cu_ring$rural == 1L & cu_ring$adj_class %in% c("remote micropolitan", "remote"))
n_nonmetro <- sum(!is.na(cu_ring$uic24) & !cu_ring$uic24 %in% METRO_2024)
cat(sprintf("Rural credit unions: statute %d | remote only %d | every non-metro county %d\n",
            n_statute, n_remote, n_nonmetro))                                    ## LOOK -- expect 463 | 347 | 2266

M15 <- merge(cty_sf, K15, by = "fips", all.x = TRUE)
p15 <- ggplot() +
  geom_sf(data = M15, aes(fill = ring), colour = C_CTY, linewidth = 0.05) +
  geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3) +
  scale_fill_manual(values = RING_COLS, breaks = names(RING_COLS)[-1], drop = FALSE, name = NULL, na.value = C_NONRURAL) +
  labs(title = sprintf("What counts as rural: %d credit unions under the statute, %d if only remote counties count,\n%s if every county outside a metro counts",
                       n_statute, n_remote, format(n_nonmetro, big.mark = ",")),
       subtitle = paste0("The statute's rural is the two darker shades. The rural growth and earnings advantage holds in both (RQ5);\n",
                         "it fades only when the lightest shade -- many of them suburban fringe counties -- is added."),
       caption = CAP("Rings from the USDA-ERS 2024 Urban Influence Codes: remote = not adjacent to any metro area. Counts include 7 credit unions in Guam and the US Virgin Islands.")) +
  TH + leg(1)
sv(p15, "map15_what_counts_as_rural")

## ---- T2  TOM_CODE: read it and give every code its meaning -------------------------
## TOM_CODE is not in cr (3_callreport.R reads ~45 columns), so read just it from
## the Stata panel. The .dta has no value labels, so the meanings come from the
## NCUA data dictionary (TOM Codes, November 2026), typed in once here.
TOM_BOOK <- tribble(
  ~tom_code, ~tom_label,                                                   ~family,
   0, "Community (urban or rural)",                                        "Community",
  66, "Community - Rural District",                                        "Community",
  77, "Community - CBSA(+) Adjacent",                                      "Community",
   1, "Associational - religious",                                         "Single bond: association",
   2, "Associational - fraternal",                                         "Single bond: association",
   3, "Associational - other",                                             "Single bond: association",
   4, "Educational",                                                       "Single bond: employer",
   5, "Military",                                                          "Single bond: employer",
   6, "Federal, state, local government",                                  "Single bond: employer",
  10, "Manufacturing - chemicals",                                         "Single bond: employer",
  11, "Manufacturing - petroleum refining",                                "Single bond: employer",
  12, "Manufacturing - primary and fabricated metals",                     "Single bond: employer",
  13, "Manufacturing - machinery",                                         "Single bond: employer",
  14, "Manufacturing - transportation equipment",                          "Single bond: employer",
  15, "Manufacturing - all other",                                         "Single bond: employer",
  20, "Service - finance, insurance, real estate, trade",                  "Single bond: employer",
  21, "Service - health care",                                             "Single bond: employer",
  22, "Service - transportation",                                          "Single bond: employer",
  23, "Service - communications and utilities",                            "Single bond: employer",
  24, "Single common bond - other",                                        "Single bond: employer",
  34, "Multiple group - primarily educational",                            "Multiple common bond",
  35, "Multiple group - primarily military",                               "Multiple common bond",
  36, "Multiple group - primarily federal, state, local government",       "Multiple common bond",
  40, "Multiple group - primarily chemical",                               "Multiple common bond",
  41, "Multiple group - primarily petroleum refining",                     "Multiple common bond",
  42, "Multiple group - primarily primary and fabricated metals",          "Multiple common bond",
  43, "Multiple group - primarily machinery",                              "Multiple common bond",
  44, "Multiple group - primarily transportation equipment",               "Multiple common bond",
  49, "Multiple group - primarily other manufacturing",                    "Multiple common bond",
  50, "Multiple group - primarily finance, insurance, real estate, trade", "Multiple common bond",
  51, "Multiple group - primarily health care",                            "Multiple common bond",
  52, "Multiple group - primarily transportation",                         "Multiple common bond",
  53, "Multiple group - primarily communications and utilities",           "Multiple common bond",
  54, "Multiple common bond - other",                                      "Multiple common bond",
  98, "Multiple group - other",                                            "Multiple common bond",
  60, "Corporate credit unions",                                           "Other",
  99, "Non-Federal Credit Union (state charter; type not reported)",       "State charter (type not reported)")
FAMS <- c("Community", "Multiple common bond", "Single bond: employer", "Single bond: association",
          "State charter (type not reported)", "Other")

hdr <- haven::read_dta(PANEL_FILE, n_max = 0)
tom_col <- grep("^tom", names(hdr), ignore.case = TRUE, value = TRUE)[1]
if (is.na(tom_col)) stop("No TOM column in the panel. Names containing 'tom' or 'memb': ",
                         paste(grep("tom|memb", names(hdr), ignore.case = TRUE, value = TRUE), collapse = ", "))
cat("TOM column:", tom_col, "\n")
tom_raw <- haven::read_dta(PANEL_FILE, col_select = tidyselect::all_of(c("cu_number", "year", "quarter", tom_col)))

## Clean the code. It is stored as TEXT, so "0" and "00", "1" and "01" are the
## same code. A handful of values are not in the dictionary at all (three-digit
## codes such as 010, 201, 701, 959; and 07, 9, 70, 71) -- entry errors. Those
## are set to missing and filled from the SAME credit union's valid code in the
## nearest quarter (earlier first, then later). Anything still missing -> "Other".
tom <- tom_raw %>%
  transmute(cu_number = as.numeric(cu_number),
            qidx = as.integer(year) * 4L + as.integer(quarter),
            raw  = trimws(as.character(haven::zap_labels(.data[[tom_col]])))) %>%
  mutate(as_num   = suppressWarnings(as.integer(raw)),
         valid    = nchar(raw) %in% 1:2 & as_num %in% TOM_BOOK$tom_code,
         tom_code = ifelse(valid, as_num, NA_integer_))
rm(tom_raw)

## how much is invalid, before filling
tom %>% summarise(rows = n(), invalid_rows = sum(!valid & raw != ""), blank_rows = sum(raw == "" | is.na(raw)),
                  pct_invalid = round(100 * mean(!valid), 2)) %>% print()       ## LOOK -- expect well under 1%
tom %>% filter(!valid, raw != "") %>% count(raw, sort = TRUE) %>% print(n = 30)  ## LOOK -- the stray codes

tom <- tom %>%
  arrange(cu_number, qidx) %>%
  group_by(cu_number) %>%
  tidyr::fill(tom_code, .direction = "downup") %>%
  ungroup() %>%
  select(cu_number, qidx, tom_code)
stopifnot("qidx does not match cr -- check how 3_callreport.R builds qidx" = QZ %in% tom$qidx)
code_book <- TOM_BOOK

## what the codes are and how many credit unions carry each, latest quarter
latest_tom <- cr %>% as_tibble() %>% filter(qidx == QZ) %>% select(cu_number, rural) %>%
  left_join(tom %>% filter(qidx == QZ), by = "cu_number") %>%
  left_join(code_book, by = "tom_code") %>%
  mutate(family = fac(ifelse(is.na(family), "Other", family), FAMS))
latest_tom %>% count(tom_code, tom_label, family, rural) %>%
  tidyr::pivot_wider(names_from = rural, values_from = n, names_prefix = "rural_", values_fill = 0) %>%
  arrange(family, tom_code) %>% print(n = Inf, width = Inf)                      ## LOOK -- CHECK THE FAMILY OF EVERY CODE before going on

## check: are state charters (cu_type 2) the ones coded 99? If so, membership type
## is only known for federal charters, and every result below says so.
cr %>% as_tibble() %>% filter(qidx == QZ) %>% select(cu_number, cu_type) %>%
  left_join(latest_tom %>% select(cu_number, family), by = "cu_number") %>%
  count(cu_type, family) %>% print(n = Inf)                                     ## LOOK -- expect type 2 almost all "State charter"

## ---- T3  the breakdown: mix of membership types, rural vs non-rural ------------------
mix <- latest_tom %>%
  count(rural, family) %>%
  group_by(rural) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% ungroup()
print(mix)                                                                       ## LOOK -- does rural lean community?

FAM_COLS <- c("Community" = "#0072B2", "Multiple common bond" = "#E69F00",       # Okabe-Ito: built for colour-blind readers
              "Single bond: employer" = "#009E73", "Single bond: association" = "#CC79A7",
              "State charter (type not reported)" = "#8C8C8C", "Other" = "#333333")
p_mix <- mix %>%
  mutate(segment = ifelse(rural == 1L, "Rural credit unions", "Other credit unions")) %>%
  ggplot(aes(x = pct, y = segment, fill = family)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = FAM_COLS, name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(title = "Membership type: rural vs other credit unions", x = "Share of credit unions", y = NULL,
       caption = paste("Field-of-membership type from TOM_CODE,", q_lab(QZ), ". Preliminary.")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
ggsave(file.path(MAP_DIR, "chart_tom_mix.png"), p_mix, width = 10, height = 3.6, dpi = 300, bg = "white")

## the rural advantage WITHIN each membership type: the RQ1 model run separately
## on each family. Membership type is taken quarter by quarter (charters convert).
CTRL <- "ln_assets + I(ln_assets^2) + age_q + lid + mdi + fed + cecl_flag"
est_t <- cr %>% as_tibble() %>%
  filter(est_sample == TRUE, !is.na(rural)) %>%
  left_join(tom, by = c("cu_number", "qidx")) %>%
  left_join(code_book %>% select(tom_code, family), by = "tom_code") %>%
  mutate(family = ifelse(is.na(family), "Other", family),
         st = factor(state_code), qtr = factor(qidx))
by_fam <- bind_rows(lapply(FAMS, function(f) {
  dd <- est_t %>% filter(family == f)
  n_rural <- n_distinct(dd$cu_number[dd$rural == 1L])
  if (n_rural < 25) return(tibble(family = f, outcome = c("Asset growth", "Return on assets"), beta = NA, lo = NA, hi = NA, rural_cus = n_rural))
  out <- list()
  for (y in c("g_assets", "roa_pct")) {
    m <- feols(as.formula(sprintf("%s ~ rural + %s | st^qtr", y, CTRL)), data = dd, cluster = ~ cu_number + fips)
    ct <- coeftable(m)["rural", ]
    out[[y]] <- tibble(family = f, outcome = ifelse(y == "g_assets", "Asset growth", "Return on assets"),
                       beta = ct[["Estimate"]], lo = ct[["Estimate"]] - 1.96 * ct[["Std. Error"]],
                       hi = ct[["Estimate"]] + 1.96 * ct[["Std. Error"]], rural_cus = n_rural)
  }
  bind_rows(out)
}))
print(by_fam %>% mutate(across(c(beta, lo, hi), ~ round(.x, 3))), n = Inf)       ## LOOK -- is the advantage only in community charters?
write.csv(by_fam, file.path(MAP_DIR, "tom_rural_advantage_by_family.csv"), row.names = FALSE)

p_fam <- by_fam %>% filter(!is.na(beta)) %>%
  mutate(family = factor(family, rev(FAMS))) %>%
  ggplot(aes(x = beta, y = family, colour = family)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.2) +
  geom_point(size = 3) +
  facet_wrap(~ outcome, scales = "free_x") +
  scale_colour_manual(values = FAM_COLS, guide = "none") +
  labs(title = "The rural advantage within each membership type",
       subtitle = "Percentage points, rural vs other credit unions of the same type, size, state and quarter; bars = 95% range",
       x = NULL, y = NULL, caption = "Types with fewer than 25 rural credit unions not shown. Preliminary.") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
ggsave(file.path(MAP_DIR, "chart_tom_rural_advantage.png"), p_fam, width = 10, height = 4.2, dpi = 300, bg = "white")

## ---- T4  MAP 16: who serves rural America, by membership type ------------------------
hq16 <- cr %>% as_tibble() %>%
  filter(qidx == QZ, rural == 1L) %>%
  select(cu_number, fips, assets_tot) %>%
  left_join(latest_tom %>% select(cu_number, family), by = "cu_number") %>%
  inner_join(as_tibble(cent_dt), by = "fips")                                    # territories drop here, as on map 1
n_terr16 <- n_statute - nrow(hq16)
fam_n <- hq16 %>% count(family)
fam_lab <- setNames(sprintf("%s (%d)", fam_n$family, fam_n$n), fam_n$family)
p16 <- ggplot() +
  geom_sf(data = M15, aes(fill = ring), colour = C_CTY, linewidth = 0.05, alpha = 0.30, show.legend = FALSE) +
  geom_sf(data = st_sf, fill = NA, colour = C_STATE, linewidth = 0.3) +
  scale_fill_manual(values = RING_COLS, na.value = C_NONRURAL) +
  geom_point(data = hq16 %>% arrange(desc(assets_tot)), aes(X, Y, colour = family, size = assets_tot / 1e6), alpha = 0.85) +
  scale_colour_manual(values = FAM_COLS, labels = fam_lab, name = "Membership type") +
  scale_size_area(max_size = 8, breaks = c(10, 100, 500), labels = c("$10m", "$100m", "$500m"), name = "Assets") +
  labs(title = sprintf("Who serves rural America: %d rural credit unions by membership type%s",
                       n_statute, if (n_terr16 > 0) sprintf(" (%d shown; %d in Guam and the US Virgin Islands not shown)", nrow(hq16), n_terr16) else ""),
       subtitle = paste0("One dot per headquarters, coloured by field-of-membership type, on the rural rings of map 15.\n",
                         "State-chartered credit unions are not assigned a membership type by NCUA (grey). Counts in the legend are for the dots shown."),
       caption = CAP("Field-of-membership type from TOM_CODE.")) +
  TH + guides(colour = guide_legend(nrow = 2, title.position = "top", override.aes = list(size = 3.5)),
              size = guide_legend(nrow = 1, title.position = "top"))
sv(p16, "map16_rural_cus_by_membership_type")

cat("\nWritten to", MAP_DIR, ": map15_what_counts_as_rural, map16_rural_cus_by_membership_type,",
    "chart_tom_mix, chart_tom_rural_advantage (+ tom_rural_advantage_by_family.csv)\n")
