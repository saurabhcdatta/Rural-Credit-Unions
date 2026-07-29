###############################################################################
# 2_rural_counties.R  -- the statutory rural classification
#
# Builds two county tables: u24 (primary) and u13 (prior vintage), then
# validates u24 against ERS published counts and measures how much of "rural"
# is an artifact of which vintage you use.
###############################################################################

## ---- 2.1 download the ERS files ---------------------------------------------
u24_file <- file.path(CACHE_DIR, "2024-urban-influence-codes.csv")
u13_file <- file.path(CACHE_DIR, "2013-urban-influence-codes.xls")

if (!file.exists(u24_file))
  download.file("https://www.ers.usda.gov/media/6182/2024-urban-influence-codes.csv",
                u24_file, mode = "wb")
if (!file.exists(u13_file))
  download.file("https://www.ers.usda.gov/media/6183/2013-urban-influence-codes.xls",
                u13_file, mode = "wb")

file.size(u24_file); file.size(u13_file)                  ## LOOK -- ~592KB, ~417KB

## If either download fails behind the proxy: open the URL in a browser, save to
## CACHE_DIR under the exact filenames above, and re-run this block.

## ---- 2.2 read the 2024 file (it is in LONG format) --------------------------
raw24 <- fread(u24_file, colClasses = "character")
names(raw24)                                              ## LOOK -- Attribute / Value
head(raw24, 6)                                            ## LOOK -- 3 rows per county
unique(raw24$Attribute)                                   ## LOOK

setnames(raw24, tolower(gsub("[^A-Za-z0-9]+", "_", names(raw24))))
w24 <- dcast(raw24, fips_uic + state + county_name ~ attribute, value.var = "value")
setnames(w24, tolower(names(w24)))
names(w24)                                                ## LOOK -- now wide

u24 <- w24[, .(fips  = sprintf("%05d", as.integer(fips_uic)),
               state, county_name,
               uic   = as.integer(uic_2024),
               pop   = as.numeric(population_2020))]
u24 <- u24[!is.na(uic)]
u24[, rural := as.integer(uic %in% RURAL_2024)]

nrow(u24); u24[, sum(rural)]                              ## LOOK -- 3233 counties

## ---- 2.3 read the 2013 file (WIDE) ------------------------------------------
raw13 <- setDT(readxl::read_excel(u13_file, col_types = "text"))
names(raw13)                                              ## LOOK
setnames(raw13, tolower(gsub("[^A-Za-z0-9]+", "_", names(raw13))))

u13 <- raw13[, .(fips = sprintf("%05d", as.integer(fips)),
                 uic  = as.integer(uic_2013))]
u13 <- u13[!is.na(uic)]
u13[, rural := as.integer(uic %in% RURAL_2013)]
nrow(u13); u13[, sum(rural)]                              ## LOOK

## ---- 2.4 VALIDATE against ERS published county counts -----------------------
## Every diff must be zero. If not, the FIPS parse is wrong and nothing
## downstream can be trusted.
bench <- data.table(uic = 1:9,
                    expected = c(443, 130, 154, 743, 272, 490, 256, 125, 531))
chk <- merge(bench,
             u24[as.integer(substr(fips, 1, 2)) <= 56, .(observed = .N), by = uic],
             by = "uic", all = TRUE)
chk[, diff := observed - expected]
chk                                                       ## LOOK -- all diff == 0

u24[as.integer(substr(fips, 1, 2)) <= 56 & rural == 1, .N]        ## LOOK -- 1556
u24[as.integer(substr(fips, 1, 2)) <= 56,
    .(rural_pop_pct = round(100 * sum(pop[rural == 1], na.rm = TRUE) /
                                  sum(pop, na.rm = TRUE), 1))]     ## LOOK -- ~7.9%

## THE FRAMING NUMBER: about half the counties, under 8% of the people.
## That asymmetry is why rural credit unions are small.

## ---- 2.5 how much of "rural" depends on the vintage? ------------------------
v <- merge(u13[as.integer(substr(fips, 1, 2)) <= 56, .(fips, r13 = rural)],
           u24[as.integer(substr(fips, 1, 2)) <= 56, .(fips, r24 = rural)],
           by = "fips")
nrow(v)                                                   ## LOOK -- matched counties

shift_tab <- v[, .N, by = .(r13, r24)]
shift_tab[, status := fifelse(r13 == r24, "stable",
                       fifelse(r13 == 1L, "LOST rural", "GAINED rural"))]
shift_tab                                                 ## LOOK -- 109 lost, 60 gained

changed_counties <- v[r13 != r24]
nrow(changed_counties)                                    ## LOOK -- 169

## These changed because OMB redelineated them in 2023, NOT because ERS
## renumbered the codes -- every 2013->2024 consolidation happened within a
## rural-status class. Keep this number; it is a report exhibit.

## Connecticut: is any of it rural? (decides how to handle the 2022 switch
## from counties to planning regions)
u13[substr(fips, 1, 2) == "09", .(n = .N, rural = sum(rural))]   ## LOOK -- 0 of 8
u24[substr(fips, 1, 2) == "09", .(n = .N, rural = sum(rural))]   ## LOOK -- 0 of 9

## Neither vintage has rural CT, so the county/planning-region mismatch cannot
## change any institution's rural flag. Document it; do not build a crosswalk.

cat("\nRural county tables ready: u24 (primary), u13 (prior vintage).\n")
cat("Next: 3_callreport.R\n")
