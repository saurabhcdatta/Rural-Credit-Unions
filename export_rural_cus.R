###############################################################################
# export_rural_cus.R  --  Rural credit unions and their offices, to Excel
#
# Rural Credit Unions Study | ROAD Act Sec. 909 | NCUA OCE
#
# Produces one workbook with four sheets:
#   1. Rural credit unions   -- one row per CU headquartered in a rural county,
#                               with size, designations, and its office counts
#   2. Offices of rural CUs  -- every office those institutions operate,
#                               wherever it is (some are in non-rural counties)
#   3. Offices in rural counties -- every office located in a rural county,
#                               whoever owns it (some owners are non-rural CUs)
#   4. Summary               -- the 2 x 2: owner rurality x office location
#
# openxlsx and writexl are not installed here, so the workbook is written as
# an Excel 2003 XML Spreadsheet (.xml) -- one file, multiple sheets, opens in
# Excel directly; "Save As .xlsx" once open if you want the modern format.
# If openxlsx becomes available the script uses it instead. CSVs are written
# alongside in every case.
#
# REQUIRES: rq_load.R (or maps_legacy_shim.R) -> cr, cu, sites, county, QZ.
#           BRANCH_DIR for the raw latest-quarter file (names and addresses).
###############################################################################

suppressPackageStartupMessages(library(data.table))
stopifnot(exists("cr"), exists("sites"), exists("county"), exists("QZ"))
if (!exists("cu")) cu <- cr[, .(rural_last = rural[which.max(qidx)], fips_last = fips[which.max(qidx)]), by = cu_number]
if (!"in_cr" %in% names(sites)) sites[, in_cr := cu_number %in% unique(cr$cu_number)]
XDIR <- file.path(OUT_DIR, "exports"); dir.create(XDIR, recursive = TRUE, showWarnings = FALSE)

## ---- X1  names and addresses from the raw latest-quarter branch file ----------
files <- sort(list.files(BRANCH_DIR, "^branch_.*\\.txt$", full.names = TRUE))
raw <- fread(files[length(files)], colClasses = "character", fill = TRUE, showProgress = FALSE)
setnames(raw, tolower(gsub("[^A-Za-z0-9]+", "_", names(raw))))
pick <- function(pats) { for (p in pats) { h <- grep(p, names(raw), value = TRUE); if (length(h)) return(h[1]) }; NA_character_ }
C <- list(cu_number = pick("^cu_number$"), cu_name = pick("^cu_name$|^name$"), site_id = pick("^siteid$|^site_id$"),
          site_name = pick("^sitename$|^site_name$"), site_type = pick("^sitetypename$"), main = pick("^mainoffice$"),
          addr = pick("^physicaladdressline1$|^physicaladdress1$"), city = pick("^physicaladdresscity$"),
          st = pick("^physicaladdressstatecode$"), zip = pick("^physicaladdresspostalcode$"),
          cty = pick("^physicaladdresscountyname"))
cat("Raw branch file columns resolved:\n"); print(unlist(C))                                        ## LOOK
addr <- raw[, .(cu_number = as.integer(get(C$cu_number)), site_id = as.integer(get(C$site_id)),
                cu_name = get(C$cu_name), site_name = if (!is.na(C$site_name)) get(C$site_name) else NA_character_,
                address = if (!is.na(C$addr)) get(C$addr) else NA_character_, city = get(C$city), state = get(C$st),
                zip = substr(get(C$zip), 1, 5), county_name_ncua = if (!is.na(C$cty)) get(C$cty) else NA_character_)]
addr <- unique(addr, by = c("cu_number", "site_id"))

## ---- X2  the office table at the latest quarter -------------------------------------
off <- sites[qidx == QZ & in_cr == TRUE & !foreign & !terr,
             .(cu_number, site_id, main_office, fips, rural_county = rural_site)]
off[cu, on = "cu_number", `:=`(owner_rural = i.rural_last, owner_hq_fips = i.fips_last)]
off <- merge(off, addr, by = c("cu_number", "site_id"), all.x = TRUE)
off[county, on = "fips", `:=`(county = i.county_name, county_state = i.state)]
off[, `:=`(office_type = fifelse(main_office, "Headquarters", "Branch"),
           office_county_rural = fifelse(rural_county == 1L, "Rural", "Non-rural"),
           owner_is_rural = fifelse(owner_rural == 1L, "Rural CU", "Non-rural CU"))]
setorder(off, cu_number, -main_office, site_id)

## ---- X3  sheet 1: rural credit unions -----------------------------------------------
z <- cr[qidx == QZ & rural == 1L]
counts <- off[, .(offices_total = .N, offices_in_rural_counties = sum(rural_county == 1L),
                  offices_outside_rural = sum(rural_county == 0L), branches = sum(!main_office)), by = cu_number]
s1 <- z[, .(cu_number, join_number, charter_type = cu_type, state, hq_fips = fips, city,
            assets_millions = round(assets_tot / 1e6, 1), members, low_income_designated = lid, mdi,
            first_observed = q_lab(first_q))]
s1[county, on = .(hq_fips = fips), `:=`(hq_county = i.county_name)]
s1 <- merge(s1, unique(addr[, .(cu_number, cu_name)]), by = "cu_number", all.x = TRUE)
s1 <- merge(s1, counts, by = "cu_number", all.x = TRUE)
for (v in c("offices_total","offices_in_rural_counties","offices_outside_rural","branches")) s1[is.na(get(v)), (v) := 0L]
setcolorder(s1, c("cu_number","cu_name","join_number","charter_type","state","hq_county","hq_fips","city",
                  "assets_millions","members","low_income_designated","mdi","first_observed",
                  "offices_total","branches","offices_in_rural_counties","offices_outside_rural"))
setorder(s1, -assets_millions)
cat("\nRural credit unions:", nrow(s1), "\n")                                                        ## LOOK -- expect ~468

## ---- X4  sheets 2 and 3 --------------------------------------------------------------
cols <- c("cu_number","cu_name","owner_is_rural","site_id","site_name","office_type","address","city","state","zip",
          "county","county_state","fips","office_county_rural")
s2 <- off[owner_rural == 1L, ..cols]                    # every office of a rural CU
s3 <- off[rural_county == 1L, ..cols]                   # every office in a rural county
cat("Offices of rural CUs:", nrow(s2), "| offices in rural counties:", nrow(s3), "\n")            ## LOOK

## ---- X5  sheet 4: the 2 x 2 ------------------------------------------------------------
s4 <- off[, .(offices = .N, credit_unions = uniqueN(cu_number)), by = .(office_located_in = office_county_rural, owned_by = owner_is_rural)]
setorder(s4, office_located_in, owned_by)
s4[, share_of_row := round(100 * offices / sum(offices), 1), by = office_located_in]
print(s4)                                                                                            ## LOOK -- the answer to "who owns the rural offices"
cat(sprintf("Of %d offices in rural counties, %d (%.0f%%) belong to credit unions headquartered outside rural counties.\n",
            s4[office_located_in == "Rural", sum(offices)], s4[office_located_in == "Rural" & owned_by == "Non-rural CU", offices],
            s4[office_located_in == "Rural" & owned_by == "Non-rural CU", share_of_row]))

## ---- X6  write ------------------------------------------------------------------------
SHEETS <- list("Rural credit unions" = s1, "Offices of rural CUs" = s2, "Offices in rural counties" = s3, "Summary" = s4)
for (nm in names(SHEETS)) fwrite(SHEETS[[nm]], file.path(XDIR, paste0(gsub(" ", "_", tolower(nm)), ".csv")))

## Excel 2003 XML Spreadsheet writer -- base R only. Excel opens it as a workbook.
write_xml_workbook <- function(sheets, file) {
  esc <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- gsub("&", "&amp;", x, fixed = TRUE)
                       x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
  con <- file(file, open = "w", encoding = "UTF-8")
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
               '<?mso-application progid="Excel.Sheet"?>',
               '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet" xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet">',
               '<Styles><Style ss:ID="h"><Font ss:Bold="1" ss:Color="#FFFFFF"/><Interior ss:Color="#0E4C55" ss:Pattern="Solid"/></Style>',
               '<Style ss:ID="n"><NumberFormat ss:Format="#,##0.0"/></Style><Style ss:ID="i"><NumberFormat ss:Format="#,##0"/></Style></Styles>'), con)
  for (nm in names(sheets)) {
    d <- as.data.frame(sheets[[nm]]); nmx <- substr(gsub("[\\[\\]\\*/\\\\?:]", " ", nm), 1, 31)
    writeLines(sprintf('<Worksheet ss:Name="%s"><Table>', esc(nmx)), con)
    writeLines(paste0('<Column ss:Width="', pmin(pmax(nchar(names(d)) * 7, 60), 260), '"/>', collapse = ""), con)
    writeLines(paste0('<Row>', paste0('<Cell ss:StyleID="h"><Data ss:Type="String">', esc(names(d)), '</Data></Cell>', collapse = ""), '</Row>'), con)
    isnum <- vapply(d, is.numeric, logical(1)); isint <- vapply(d, function(x) is.integer(x) || (is.numeric(x) && all(x == round(x), na.rm = TRUE)), logical(1))
    for (i in seq_len(nrow(d))) {
      cells <- vapply(seq_along(d), function(j) {
        v <- d[i, j]
        if (isnum[j] && !is.na(v)) sprintf('<Cell ss:StyleID="%s"><Data ss:Type="Number">%s</Data></Cell>', if (isint[j]) "i" else "n", format(v, scientific = FALSE, trim = TRUE))
        else sprintf('<Cell><Data ss:Type="String">%s</Data></Cell>', esc(v))
      }, character(1))
      writeLines(paste0('<Row>', paste0(cells, collapse = ""), '</Row>'), con)
    }
    writeLines('</Table><WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel"><FreezePanes/><SplitHorizontal>1</SplitHorizontal><TopRowBottomPane>1</TopRowBottomPane></WorksheetOptions></Worksheet>', con)
  }
  writeLines('</Workbook>', con); close(con); invisible(file)
}

if (requireNamespace("openxlsx", quietly = TRUE)) {
  f <- file.path(XDIR, "Rural_credit_unions_and_offices.xlsx")
  wb <- openxlsx::createWorkbook()
  for (nm in names(SHEETS)) { openxlsx::addWorksheet(wb, nm); openxlsx::writeData(wb, nm, SHEETS[[nm]], headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#0E4C55", fontColour = "#FFFFFF"))
                              openxlsx::freezePane(wb, nm, firstRow = TRUE); openxlsx::setColWidths(wb, nm, cols = seq_along(SHEETS[[nm]]), widths = "auto") }
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
} else {
  f <- file.path(XDIR, "Rural_credit_unions_and_offices.xml")
  write_xml_workbook(SHEETS, f)
  cat("\nopenxlsx not available -- wrote an Excel 2003 XML workbook instead. Open it in Excel, then Save As .xlsx if needed.\n")
}
cat("Workbook:", f, "\nCSVs alongside in", XDIR, "\n")
