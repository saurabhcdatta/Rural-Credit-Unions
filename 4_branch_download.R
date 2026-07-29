###############################################################################
# 4_branch_download.R  -- pull the branch table out of 63 NCUA quarterly ZIPs
#
# The branch table starts Sep-2010, so 2010Q3 is the first usable quarter.
# Each archive is tens of MB; only the branch file is kept, so this leaves a
# few hundred MB rather than several GB.
#
# Safe to re-run: quarters already present are skipped.
###############################################################################

## ---- 4.1 the URL table ------------------------------------------------------
## NCUA changed hosting conventions twice and left three one-offs, so this is
## explicit rather than a rule:
##   2016Q1 +    analysis/call-report-data-YYYY-MM.zip   (lowercase)
##   2015Q2-Q4   analysis/Call-Report-Data-YYYY-MM.zip   (title case)
##   <= 2015Q1   data-apps/QCRYYYYMM.zip
##   2013Q2      data-apps/5300Data0613Final.zip
##   2010Q2, Q4  data-apps/QCRYYYYMM.Zip                 (capital Z)

qs <- data.table(qidx = (2010 * 4 + 3):(2026 * 4 + 1))
qs[, `:=`(year = qidx %/% 4L, q = qidx %% 4L)]
qs[q == 0L, `:=`(year = year - 1L, q = 4L)]
qs[, `:=`(mon = q * 3L, tag = sprintf("%dQ%d", year, q))]
nrow(qs); qs[c(1, .N)]                                    ## LOOK -- 63 quarters

one_off <- c("2013-06" = "data-apps/5300Data0613Final.zip",
             "2010-06" = "data-apps/QCR201006.Zip",
             "2010-12" = "data-apps/QCR201012.Zip")

qs[, ym  := sprintf("%04d-%02d", year, mon)]
qs[, url := fifelse(
      ym %in% names(one_off), paste0("https://ncua.gov/files/publications/", one_off[ym]),
   fifelse(year >= 2016,
           sprintf("https://ncua.gov/files/publications/analysis/call-report-data-%s.zip", ym),
   fifelse(year == 2015 & mon >= 6,
           sprintf("https://ncua.gov/files/publications/analysis/Call-Report-Data-%s.zip", ym),
           sprintf("https://ncua.gov/files/publications/data-apps/QCR%04d%02d.zip", year, mon))))]
qs[c(1, 2, 12, 22, 23, 24, .N), .(tag, url)]              ## LOOK -- spot-check the edges

## ---- 4.2 download loop ------------------------------------------------------
## Expect 30-60 minutes. Failures are logged and the loop continues.
BRANCH_RX <- "credit.?union.?branch.?information.*\\.txt$"
qs[, status := NA_character_]

for (i in seq_len(nrow(qs))) {
  tag <- qs$tag[i]
  out <- file.path(BRANCH_DIR, sprintf("branch_%s.txt", tag))
  if (file.exists(out)) { qs$status[i] <- "cached"; cat(tag, " cached\n"); next }

  zf <- tempfile(fileext = ".zip")
  ok <- FALSE
  try({
    download.file(qs$url[i], zf, mode = "wb", quiet = TRUE)
    ## a proxy block page returns HTTP 200 but is not a ZIP -- check for "PK"
    con <- file(zf, "rb"); magic <- readBin(con, "raw", 2); close(con)
    ok <- identical(as.character(magic), c("50", "4b"))
  }, silent = TRUE)

  if (!ok) { qs$status[i] <- "FAILED"; cat(tag, " FAILED\n"); unlink(zf); next }

  inner <- grep(BRANCH_RX, unzip(zf, list = TRUE)$Name, value = TRUE, ignore.case = TRUE)
  if (!length(inner)) { qs$status[i] <- "no branch table"; cat(tag, " no branch table\n")
                        unlink(zf); next }

  td <- tempfile(); dir.create(td)
  unzip(zf, files = inner[1], exdir = td, junkpaths = TRUE)
  file.copy(file.path(td, basename(inner[1])), out, overwrite = TRUE)
  unlink(c(zf, td), recursive = TRUE)

  qs$status[i] <- "downloaded"
  cat(tag, " ok  (", round(file.size(out) / 1024), " KB)\n", sep = "")
  Sys.sleep(1)                                            # be polite to a .gov server
}

qs[, .N, by = status]                                     ## LOOK -- want 63 ok/cached
qs[status == "FAILED", .(tag, url)]                       ## LOOK -- retry or fetch by hand

length(list.files(BRANCH_DIR, "^branch_.*\\.txt$"))       ## LOOK -- 63

cat("\nBranch files downloaded. Next: 5_branch_panel.R\n")
