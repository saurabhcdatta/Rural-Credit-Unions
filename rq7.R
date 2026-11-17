###############################################################################
# rq7.R  --  RESEARCH QUESTION 7   (standalone, 10 Nov 2026)
# "Is anyone starting new rural credit unions?"
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# THE QUESTION IN PLAIN WORDS
#   How many new credit unions have appeared in rural counties since 2010, how
#   does that compare with how many left, and what became of the new ones?
#
# EXPECTATION (handoff v4, written before running)
#   Very few: roughly one genuinely new rural credit union a year against about
#   fifteen leaving; most entrants small; many gone within ten years.
#
# DEFINITIONS (all from the call report panel, nothing else needed)
#   Entrant      a credit union whose FIRST quarter in the panel is after the
#                panel opens. Rural = its headquarters county was rural (2024
#                codes) in that first quarter.
#   New charter  a GENUINE START-UP: an entrant whose year opened (FOICU field
#                year_opened_cu030 in the OCE panel) is no earlier than the year
#                before it first appears. Entrants that opened long before are
#                existing credit unions appearing late (newly federally insured,
#                conversions, re-registrations, filing gaps). Size at entry
#                (< $2m / < $5m) is kept only as a cross-check -- on its own it
#                counted old, small credit unions as new.
#   Exit         last quarter before the panel ends (merger, liquidation or
#                conversion -- outcome codes are not used, they are unverified).
#   Rural rates  per 100 credit unions existing at the start of each year.
#
# WHAT EACH BLOCK DOES
#   7.0  gates and set-up
#   7.1  one row per credit union: first and last quarter, size at entry
#   7.1b does the panel stop at $1m?   7.1c why existing CUs appear late
#   7.2  the entrants: how many, how big, rural vs other     (THE COUNT)
#   7.3  entries and exits by year, and rates per 100         (THE DROUGHT)
#   7.4  what became of the rural entrants                    (SURVIVAL)
#   7.5  the list of rural entrants (for the report appendix / CURE check)
#   7.6  chart, CSVs, slide numbers
#
# REQUIRES  cr in memory (3_callreport.R or rq_load.R) with cu_number, qidx,
#           fips, state_code, rural, assets_tot, members, networth_pct, cu_type;
#           cu_name if available. universe.R in CODE_DIR. OUT_DIR set.
#           Packages: dplyr, tidyr, ggplot2.
#
# Sequential blocks. Run each, read what it prints (## LOOK), then move on.
###############################################################################

suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tidyr); library(ggplot2) })

## ---- 7.0  gates and set-up -----------------------------------------------------------
## Always re-read universe.R (an older copy may be in memory), and make sure cr
## is a data.table, which the universe check expects.
source(file.path(CODE_DIR, "universe.R"))
if (!is.data.table(cr)) setDT(cr)
assert_universe(cr, "rq7.R")
OUT7 <- file.path(OUT_DIR, "rq7"); dir.create(OUT7, recursive = TRUE, showWarnings = FALSE)
q_lab <- function(q) sprintf("%dQ%d", (q - 1L) %/% 4L, ((q - 1L) %% 4L) + 1L)
q_year <- function(q) (q - 1L) %/% 4L
NEW_CUT  <- 2e6      # under $2m at first report = a new charter
NEW_CUT2 <- 5e6      # check
p <- cr %>% as_tibble()
QA <- min(p$qidx); QZ <- max(p$qidx)
cat("Panel:", q_lab(QA), "to", q_lab(QZ), "|", n_distinct(p$cu_number), "credit unions\n")   ## LOOK
nm_col <- intersect(c("cu_name", "CU_NAME", "name"), names(p))[1]
cat("Name column:", ifelse(is.na(nm_col), "none (list will show numbers only)", nm_col), "\n")

## ---- 7.1  one row per credit union -----------------------------------------------------
spell <- p %>%
  arrange(cu_number, qidx) %>%
  group_by(cu_number) %>%
  summarise(name        = if (!is.na(nm_col)) first(.data[[nm_col]]) else NA_character_,
            cu_type     = first(cu_type),
            state       = first(state_code),
            fips_first  = first(fips),
            rural_first = first(rural),
            rural_last  = last(rural),
            first_q     = first(qidx),
            last_q      = last(qidx),
            n_quarters  = n(),
            assets_first  = first(assets_tot),
            members_first = first(members),
            nw_first      = first(networth_pct),
            assets_last   = last(assets_tot),
            members_last  = last(members),
            .groups = "drop") %>%
  mutate(entered   = first_q > QA,
         exited    = last_q < QZ,
         has_gap   = n_quarters < (last_q - first_q + 1L),        # dropped out and came back
         entry_year = q_year(first_q),
         exit_year  = ifelse(exited, q_year(last_q), NA_integer_),
         small_entry  = entered & assets_first < NEW_CUT,
         small_entry5 = entered & assets_first < NEW_CUT2)

## year opened, from the Stata panel (cr does not keep it)
## the field is named year_opened_CU030 in the panel -- match it whatever its case
yo_col <- grep("^year_opened", names(haven::read_dta(PANEL_FILE, n_max = 0)), ignore.case = TRUE, value = TRUE)[1]
stopifnot("no year_opened field in the panel" = !is.na(yo_col))
cat("Year-opened field:", yo_col, "\n")
yo <- haven::read_dta(PANEL_FILE, col_select = tidyselect::all_of(c("cu_number", yo_col)))
yo <- haven::zap_labels(yo) %>%
  transmute(cu_number = as.numeric(cu_number), year_opened = suppressWarnings(as.integer(as.character(.data[[yo_col]])))) %>%
  filter(!is.na(year_opened), year_opened > 1900) %>%
  group_by(cu_number) %>% summarise(year_opened = max(year_opened), .groups = "drop")   # latest reported, if it ever changes
spell <- spell %>% left_join(yo, by = "cu_number") %>%
  mutate(new_charter  = entered & !is.na(year_opened) & year_opened >= entry_year - 1L,   # GENUINE start-up
         entry_kind = case_when(!entered                         ~ "in panel from the start",
                                new_charter                      ~ "Genuine start-up",
                                is.na(year_opened)               ~ "Entrant, year opened unknown",
                                TRUE                             ~ "Existing CU appearing late"))
spell %>% filter(entered) %>% count(entry_kind, small_entry) %>% print()   ## LOOK -- how size and year-opened disagree
cat("Entrants with no year opened:", sum(spell$entered & is.na(spell$year_opened)), "(should be few)\n")
spell %>% summarise(credit_unions = n(), entrants = sum(entered), exits = sum(exited),
                    with_gaps = sum(has_gap)) %>% print()                    ## LOOK

## ---- 7.1b  does the panel stop at $1 million? -------------------------------------------
## Many "existing CU appearing late" entrants first report at $1.00-1.07m. That
## pattern suggests the panel holds only credit unions of $1m or more. If so:
##   - an "entrant" may just be an old credit union growing past $1m;
##   - an "exit" may just be a credit union shrinking below $1m, not closing;
##   - a start-up that took 2+ years to reach $1m is missed by the year-opened rule.
cat("Smallest assets anywhere in the panel: $", format(min(p$assets_tot, na.rm = TRUE), big.mark = ","), "\n")
cat("Rows under $1m:", sum(p$assets_tot < 1e6, na.rm = TRUE), "  (0 = the panel stops at $1m)\n")   ## LOOK

## entrants that first appear just over $1m, and exits last seen just over $1m
spell %>% filter(entered) %>%
  group_by(grp = ifelse(rural_first == 1L, "Rural", "Other")) %>%
  summarise(entrants = n(), first_seen_1.0_to_1.1m = sum(assets_first < 1.1e6), .groups = "drop") %>% print()   ## LOOK
spell %>% filter(exited) %>%
  group_by(grp = ifelse(rural_last == 1L, "Rural", "Other")) %>%
  summarise(exits = n(), last_seen_1.0_to_1.1m = sum(assets_last < 1.1e6),
            came_back_later = sum(has_gap), .groups = "drop") %>% print()                                   ## LOOK

## a wider start-up count that does not depend on when it first reached $1m:
## any credit union in the panel that OPENED in 2010 or later
spell %>% filter(!is.na(year_opened), year_opened >= q_year(QA)) %>%
  group_by(grp = ifelse(rural_first == 1L, "Rural", "Other")) %>%
  summarise(opened_2010_or_later = n(), still_open = sum(!exited),
            first_report_years_after_opening = median(entry_year - year_opened), .groups = "drop") %>% print()   ## LOOK


## ---- 7.1c  why did existing credit unions appear late? -----------------------------------
## Each "existing CU appearing late" gets its most likely reason, tested in this order:
##   1 Charter conversion  another credit union in the same state made its last report in the
##                         three quarters before (or the same quarter), at a similar size (+/-25%)
##                         -- the same institution under a new charter number
##   2 Newly insured       the raw panel shows it as privately insured (type 3) before
##   3 Crossed $1m         first report under $1.1m (grew past the panel's size floor)
##   4 Missing 2010Q1      first report in 2010Q2, the panel's second quarter
##   5 Unexplained         none of the above -- look these up by hand
late <- spell %>% filter(entry_kind == "Existing CU appearing late")
cat("Existing credit unions appearing late:", nrow(late), "\n")              ## LOOK -- 341 (43 rural)

## test 1: a matching exit just before the entry
last_fips <- p %>% group_by(cu_number) %>% summarise(fips_last = last(fips), .groups = "drop")
gone <- spell %>% filter(exited) %>% left_join(last_fips, by = "cu_number") %>%
  select(old_number = cu_number, state, old_last_q = last_q, old_assets = assets_last, fips_last)
conv <- late %>% select(cu_number, state, first_q, assets_first, fips_first) %>%
  inner_join(gone, by = "state", relationship = "many-to-many") %>%
  filter(old_last_q >= first_q - 3L, old_last_q <= first_q,
         abs(old_assets / assets_first - 1) <= 0.25, old_number != cu_number) %>%
  group_by(cu_number) %>%
  slice_min(abs(old_assets / assets_first - 1), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(cu_number, old_number, same_county = fips_last == fips_first)
cat("Matched to an earlier charter number:", nrow(conv), "| same county:", sum(conv$same_county), "\n")   ## LOOK

## test 2: privately insured before (raw panel, all charter types; five columns only)
raw <- haven::read_dta(PANEL_FILE, col_select = tidyselect::all_of(c("cu_number", "year", "quarter", "cu_type", "assets_tot"))) %>%
  haven::zap_labels() %>%
  mutate(cu_number = as.numeric(cu_number),
         qidx = as.integer(year) * 4L + as.integer(quarter),
         cu_type = suppressWarnings(as.integer(as.character(cu_type)))) %>%
  filter(cu_number %in% late$cu_number)
before <- raw %>% inner_join(late %>% select(cu_number, first_q), by = "cu_number") %>%
  filter(qidx < first_q, qidx >= first_q - 8L) %>%                          # the two years before it appears
  group_by(cu_number) %>%
  summarise(rows_before = n(),
            type3_before = any(cu_type == 3L, na.rm = TRUE),
            under_1m_before = any(assets_tot < 1e6, na.rm = TRUE), .groups = "drop")
cat("Late appearers with earlier rows in the raw panel:", nrow(before),
    "| privately insured before:", sum(before$type3_before),
    "| under $1m before:", sum(before$under_1m_before), "\n")               ## LOOK -- if 'under $1m before' > 0, the floor is in our build, not the panel

## put the reasons together
reasons <- late %>% select(cu_number, first_q, assets_first) %>%
  left_join(conv, by = "cu_number") %>%
  left_join(before, by = "cu_number") %>%
  mutate(late_reason = case_when(
           !is.na(old_number)        ~ "1 Charter conversion (new number)",
           type3_before %in% TRUE    ~ "2 Newly federally insured",
           assets_first < 1.1e6      ~ "3 Crossed $1m",
           first_q == QA + 1L        ~ "4 Missing 2010Q1",
           TRUE                      ~ "5 Unexplained")) %>%
  select(cu_number, late_reason, old_number)
spell <- spell %>% select(-any_of(c("late_reason", "old_number"))) %>% left_join(reasons, by = "cu_number")

spell %>% filter(entry_kind == "Existing CU appearing late") %>%
  count(grp = ifelse(rural_first == 1L, "Rural", "Other"), late_reason) %>%
  tidyr::pivot_wider(names_from = grp, values_from = n, values_fill = 0) %>% print()   ## LOOK -- the footnote table
spell %>% filter(entry_kind == "Existing CU appearing late", rural_first == 1L, late_reason == "5 Unexplained") %>%
  select(cu_number, state, fips_first, first_q, year_opened, assets_first) %>%
  mutate(first_quarter = q_lab(first_q), assets_first_m = round(assets_first / 1e6, 2)) %>%
  select(-first_q, -assets_first) %>% print(n = Inf)                       ## LOOK -- rural ones to check by hand
## ---- 7.2  the entrants: how many, how big --------------------------------------------
entr <- spell %>% filter(entered)
entr %>%
  group_by(rural = ifelse(rural_first == 1L, "Rural", "Other")) %>%
  summarise(entrants = n(),
            genuine_start_ups     = sum(new_charter),
            existing_appearing_late = sum(!new_charter),
            check_small_under_2m  = sum(small_entry),
            check_small_under_5m  = sum(small_entry5),
            median_assets_first_m = round(median(assets_first) / 1e6, 2),
            pct_state_chartered   = round(100 * mean(cu_type == 2L), 0),
            .groups = "drop") %>% print()                                    ## LOOK -- the count (earlier work: about 47 rural, 29 new)

## ---- 7.3  entries and exits by year, and rates per 100 --------------------------------
## existing at the start of each year = present in that year's first quarter
yrs <- seq(q_year(QA) + 1L, q_year(QZ) - 1L)          # full years only (latest year is partial)
base <- p %>% filter(qidx %in% (yrs * 4L + 1L)) %>%
  mutate(year = q_year(qidx), grp = ifelse(rural == 1L, "Rural", "Other")) %>%
  count(year, grp, name = "existing")
ent_y <- entr %>% mutate(grp = ifelse(rural_first == 1L, "Rural", "Other")) %>%
  group_by(year = entry_year, grp) %>%
  summarise(entrants = n(), new_charters = sum(new_charter), .groups = "drop")
ext_y <- spell %>% filter(exited) %>% mutate(grp = ifelse(rural_last == 1L, "Rural", "Other")) %>%
  count(year = exit_year, grp, name = "exits")
flows <- base %>% left_join(ent_y, by = c("year", "grp")) %>% left_join(ext_y, by = c("year", "grp")) %>%
  mutate(across(c(entrants, new_charters, exits), ~ replace_na(.x, 0L)),
         new_per_100  = round(100 * new_charters / existing, 2),
         exits_per_100 = round(100 * exits / existing, 2),
         exits_per_new = ifelse(new_charters > 0, round(exits / new_charters, 1), NA))
flows %>% filter(grp == "Rural") %>% select(year, existing, entrants, new_charters, exits, new_per_100, exits_per_100) %>%
  print(n = Inf)                                                             ## LOOK -- rural, year by year
tot <- flows %>% group_by(grp) %>%
  summarise(new_charters = sum(new_charters), entrants = sum(entrants), exits = sum(exits),
            new_per_100_per_year  = round(100 * sum(new_charters) / sum(existing), 2),
            exits_per_100_per_year = round(100 * sum(exits) / sum(existing), 2), .groups = "drop") %>%
  mutate(exits_per_new_charter = round(exits / pmax(new_charters, 1), 0))
print(tot)                                                                   ## LOOK -- the drought, rural vs other
## the recent five years
flows %>% filter(year > max(year) - 5, grp == "Rural") %>% select(year, new_charters, exits) %>% print()   ## LOOK

## ---- 7.4  what became of the rural entrants -------------------------------------------
re <- entr %>% filter(rural_first == 1L) %>%
  mutate(type = entry_kind,
         years_followed = (pmin(last_q, QZ) - first_q) / 4,
         gone_5y  = exited & (last_q - first_q) < 20L,
         followed_5y = (QZ - first_q) >= 20L)
re %>% group_by(type) %>%
  summarise(entrants = n(),
            still_open = sum(!exited),
            pct_still_open = round(100 * mean(!exited), 0),
            gone_within_5y_pct = round(100 * sum(gone_5y & followed_5y) / pmax(sum(followed_5y), 1), 0),
            median_assets_now_m = round(median(assets_last[!exited]) / 1e6, 1),
            median_members_now  = median(members_last[!exited]),
            .groups = "drop") %>% print()                                    ## LOOK -- survival
## growth of the survivors: from first report to now
re %>% filter(!exited, new_charter) %>%
  summarise(survivors = n(),
            median_assets_first_m = round(median(assets_first) / 1e6, 2),
            median_assets_now_m   = round(median(assets_last) / 1e6, 2),
            median_members_first  = median(members_first),
            median_members_now    = median(members_last)) %>% print()        ## LOOK -- did the survivors grow?
## same comparison for non-rural new charters, as a benchmark
entr %>% filter(rural_first == 0L, new_charter) %>%
  summarise(new_charters = n(), pct_still_open = round(100 * mean(!exited), 0)) %>% print()   ## LOOK -- benchmark

## ---- 7.5  the list of rural entrants ---------------------------------------------------
rural_entrants <- re %>%
  transmute(cu_number, name, state, fips_first, cu_type, type,
            first_quarter = q_lab(first_q),
            status = ifelse(exited, paste("gone after", q_lab(last_q)), "still open"),
            year_opened, assets_first_m = round(assets_first / 1e6, 2), members_first,
            assets_now_m = ifelse(exited, NA, round(assets_last / 1e6, 2)),
            rural_now = rural_last, came_and_went = has_gap,
            late_reason, old_number) %>%
  arrange(first_quarter)
print(rural_entrants, n = Inf, width = Inf)                                  ## LOOK -- the whole list fits on a page
cat("Universe check -- charter types on the list:", paste(sort(unique(rural_entrants$cu_type)), collapse = ", "), "(only 1 and 2)\n")

## ---- 7.6  chart, CSVs, slide numbers ---------------------------------------------------
rplot <- flows %>% filter(grp == "Rural") %>%
  select(year, `New rural charters` = new_charters, `Rural credit unions that left` = exits) %>%
  pivot_longer(-year, names_to = "flow", values_to = "n")
p7 <- ggplot(rplot, aes(year, n, fill = flow)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c("New rural charters" = "#0E4C55", "Rural credit unions that left" = "#C8553D"), name = NULL) +
  scale_x_continuous(breaks = yrs) +
  labs(title = "New rural credit unions vs rural credit unions leaving, each year",
       subtitle = "New charter = genuine start-up (opened no earlier than the year before it first reports). Rural = headquarters county rural (12 CFR 1026.35, 2024 codes).",
       x = NULL, y = "Credit unions", caption = sprintf("Federally insured credit unions, %s-%s. Preliminary.", q_lab(QA), q_lab(QZ))) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
ggsave(file.path(OUT7, "rq7_entries_vs_exits.png"), p7, width = 11, height = 5, dpi = 300, bg = "white")

write.csv(spell,          file.path(OUT7, "rq7_spells_all_cus.csv"),  row.names = FALSE)
write.csv(flows,          file.path(OUT7, "rq7_flows_by_year.csv"),   row.names = FALSE)
write.csv(tot,            file.path(OUT7, "rq7_totals.csv"),          row.names = FALSE)
write.csv(rural_entrants, file.path(OUT7, "rq7_rural_entrants.csv"),  row.names = FALSE)

r <- tot %>% filter(grp == "Rural"); o <- tot %>% filter(grp == "Other")
cat("\n=== SLIDE NUMBERS, Q7 ===\n")
cat(sprintf("Rural, %d-%d: %d entrants, of which %d genuine start-ups; %d rural credit unions left.\n",
            min(yrs), max(yrs), r$entrants, r$new_charters, r$exits))
cat(sprintf("Whole panel %s-%s: %d rural entrants, %d genuine start-ups.\n", q_lab(QA), q_lab(QZ), nrow(re), sum(re$new_charter)))
cat(sprintf("About %d rural credit unions left for every new one (other: %d).\n", r$exits_per_new_charter, o$exits_per_new_charter))
cat(sprintf("New charters per 100 existing per year: rural %.2f, other %.2f.\n", r$new_per_100_per_year, o$new_per_100_per_year))
cat(sprintf("Of %d rural new charters, %d still open (%.0f%%).\n",
            sum(re$new_charter), sum(re$new_charter & !re$exited), 100 * mean(!re$exited[re$new_charter])))
