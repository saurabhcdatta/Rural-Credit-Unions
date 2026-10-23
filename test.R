## ---- 12.3c  The list of rural entrants -------------------------------------
## One row per credit union that first appears after the panel opens,
## headquartered in a rural county (2024 vintage) when it first appears.

rural_entrants <- spell[entered == 1L & r24_first == 1L,
                        list(id, first_q, last_q, fips_first, assets_first,
                             still_open = as.integer(exited == 0L))]

## first-quarter details from the panel
first_row <- sp[, .SD[which.min(qidx)], by = id][, list(id, st, members_first = members, nw_first = nw)]
rural_entrants <- merge(rural_entrants, first_row, by = "id", all.x = TRUE)

## name and charter type from cr, if those columns are there
nm_col   <- intersect(c("cu_name", "CU_NAME", "name", "NAME"), names(cr))[1]
type_col <- intersect(c("cu_type", "CU_TYPE", "charter_type"), names(cr))[1]
id_col   <- intersect(c("cu_number", "CU_NUMBER"), names(cr))[1]
if (!is.na(nm_col) || !is.na(type_col)) {
  keep <- c(id_col, na.omit(c(nm_col, type_col)))
  info <- unique(as.data.table(cr)[, ..keep], by = id_col)
  setnames(info, id_col, "id")
  rural_entrants <- merge(rural_entrants, info, by = "id", all.x = TRUE)
}

## the "genuinely new" test: tiny at first appearance
rural_entrants[, `:=`(
  first_quarter = qlab(first_q),
  last_quarter  = ifelse(still_open == 1L, "still open", qlab(last_q)),
  assets_first_m = round(assets_first / 1e6, 2),
  under_2m = as.integer(assets_first < 2e6),
  under_5m = as.integer(assets_first < 5e6)
)]
setorder(rural_entrants, first_q)

cat("Rural entrants listed:", nrow(rural_entrants), "(should be 48)\n")
cat("Under $2m at entry:", sum(rural_entrants$under_2m, na.rm = TRUE),
    "| under $5m:", sum(rural_entrants$under_5m, na.rm = TRUE), "(one of these should be 29)\n")
if (!is.na(type_col)) print(table(rural_entrants[[type_col]], useNA = "ifany"))   ## LOOK -- any type 3 must come out
print(rural_entrants)                                                              ## LOOK

fwrite(rural_entrants, file.path(OUT_DIR, "Q2_rural_entrants_list.csv"))