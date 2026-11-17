## 1. the full Stata panel (cr keeps only some of its columns)
hdr <- haven::read_dta(PANEL_FILE, n_max = 0)
grep("open|charter|estab|insur|year_", names(hdr), value = TRUE, ignore.case = TRUE)    ## LOOK
## 2. the FOICU file from any quarterly call-report download, if you have one saved
## (FOICU.txt usually carries YEAR_OPENED)
foicu_file <- list.files(CACHE_DIR, "FOICU", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)[1]
foicu_file
if (!is.na(foicu_file)) names(read.csv(foicu_file, nrows = 1))                         ## LOOK


re <- re %>% mutate(
  start_up = case_when(cu_type == 1L & cu_number >= 24000 & new_charter ~ "Genuine start-up (federal, new charter number)",
                       cu_type == 1L & cu_number <  24000               ~ "Existing CU appearing late (old federal number)",
                       cu_type == 2L & new_charter                      ~ "State charter, small at entry (unconfirmed)",
                       TRUE                                             ~ "Larger entrant (likely conversion)"))
re %>% count(start_up) %>% print()                                                        ## LOOK -- the real count
re %>% group_by(start_up) %>% summarise(n = n(), still_open = sum(!exited), .groups = "drop") %>% print()