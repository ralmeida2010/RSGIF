# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# Recompute the decadal integrity table of the PRFD (Table "prfd_annual_kpis")
# from the record-level audit table `audit_table_prfd`.
#
# Columns of the table:
#   Records, Area (ha), Geom (%), Tier A/B/U/C (% of burned area),
#   W.Err (%), Med.Err (%), Idx Count, Idx Area, Idx Area (wU = wB)
#
# Outputs (in out_dir):
#   table_integrity_decadal.csv   decadal table (same columns as the paper)
#   table_integrity_annual.csv    the same indicators by year
#   table_integrity_decadal.tex   LaTeX rows ready to paste in the table
#   records_not_counted.csv       records left out of the table, with the reason
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# -----------------------------------------------------------------------------
# 0. CONFIGURATION  (edit this block only)
# -----------------------------------------------------------------------------
cfg <- list(
  # `audit_table_prfd` may already be in memory; otherwise it is read from this file
  # (.rds, .csv, or .gpkg)
  input_file = file.path(if (exists("output_dir")) output_dir else "output", "audit_table_prfd.rds"),
  out_dir    = file.path(if (exists("output_dir")) output_dir else "output", "integrity_table"),

  # column names in audit_table_prfd (as in the RSGIF pipeline)
  cols = list(
    id        = "Codigo",
    year      = "Ano",
    decade    = "Decada",                          # used only to cross-check the decades
    area      = "AreaTotal",                       # tabular burned area A_obs (ha)
    tier      = "PRFD_Overall_Quality_Tier",
    linked    = "Has_Geometry",                    # TRUE when linked to a PBA perimeter
    area_geom = "G4_Area_Geom_ha",                 # area of the linked perimeter (ha)
    reldiff   = "G4_RelDiff_AreaTotal_Geom_pct",   # relative difference tabular vs perimeter (%)
    absdiff   = "AbsDiff_Geom_ha"                  # |A_obs - A_geom| (ha)
  ),

  # weights of the integrity indices (baseline)
  w = c(A = 1.00, B = 0.65, U = 0.00, C = 0.00),

  # decades used in the paper
  breaks = c(1980, 1990, 2000, 2010, 2020, 2026),
  labels = c("1980--1989", "1990--1999", "2000--2009", "2010--2019", "2020--2025"),

  # area-weighted error (W.Err). The script prints all three for checking:
  #   "weighted_mean" = sum(A_obs * reldiff) / sum(A_obs)
  #   "abs_over_obs"  = 100 * sum(AbsDiff_Geom_ha) / sum(A_obs)      (linked records)
  #   "abs_over_max"  = 100 * sum(AbsDiff_Geom_ha) / sum(max(A_obs, A_geom))
  werr_method = "weighted_mean"
)

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. READ AND STANDARDIZE
# -----------------------------------------------------------------------------
read_audit <- function(cfg) {
  if (exists("audit_table_prfd", envir = globalenv())) {
    d <- get("audit_table_prfd", envir = globalenv())
  } else {
    f <- cfg$input_file
    d <- switch(tolower(tools::file_ext(f)),
      rds  = readRDS(f),
      csv  = fread(f, encoding = "UTF-8"),
      gpkg = { if (!requireNamespace("sf", quietly = TRUE)) stop("sf needed for gpkg")
               sf::st_drop_geometry(sf::st_read(f, quiet = TRUE)) },
      stop("unsupported input format: ", f))
  }
  d <- as.data.table(d)
  if (inherits(d, "sf")) d <- as.data.table(sf::st_drop_geometry(d))
  cc <- cfg$cols
  miss <- setdiff(unlist(cc[c("id", "area", "tier")]), names(d))
  if (length(miss)) stop("columns not found in audit_table_prfd: ", paste(miss, collapse = ", "))

  out <- data.table(id = d[[cc$id]], A_obs = as.numeric(d[[cc$area]]),
                    year = as.integer(d[[cc$year]]))
  out[, decade_src := if (!is.na(cc$decade) && cc$decade %in% names(d)) as.character(d[[cc$decade]]) else NA_character_]

  # tier: accepts "A", "Tier A", "Tier A (Gold)", "Gold", "Silver", "Unverifiable", "Caution"
  t <- toupper(trimws(as.character(d[[cc$tier]])))
  tt <- substr(sub("^TIER[ _~-]*", "", t), 1, 1)
  tt[grepl("GOLD", t)] <- "A"; tt[grepl("SILVER", t)] <- "B"
  tt[grepl("UNVERIF", t)] <- "U"; tt[grepl("CAUTION", t)] <- "C"
  out[, tier := tt]
  out[!tier %in% c("A", "B", "U", "C"), tier := NA_character_]

  getnum <- function(col) if (!is.na(col) && col %in% names(d)) as.numeric(d[[col]]) else NA_real_
  out[, `:=`(A_geom = getnum(cc$area_geom), reldiff = getnum(cc$reldiff), absdiff = getnum(cc$absdiff))]
  g <- d[[cc$linked]]
  out[, G := if (is.logical(g)) g else toupper(as.character(g)) %in% c("1", "TRUE", "T", "YES", "Y")]
  out[is.na(G), G := FALSE]
  out
}

# -----------------------------------------------------------------------------
# 2. INDICATORS FOR ONE GROUP (decade or year)
# -----------------------------------------------------------------------------
indicators <- function(x, w, werr_method) {
  A  <- x$A_obs; A[is.na(A)] <- 0
  tot_A <- sum(A)
  tier_area <- sapply(c("A", "B", "U", "C"), function(k) sum(A[x$tier %in% k]))
  tier_n    <- sapply(c("A", "B", "U", "C"), function(k) sum(x$tier %in% k))

  lk <- x[G == TRUE & !is.na(A_obs)]
  # relative difference: the pipeline column if present, otherwise recomputed (Appendix C2)
  e <- if (any(!is.na(lk$reldiff))) lk$reldiff
       else with(lk, 100 * abs(A_obs - A_geom) / pmax(A_obs, A_geom))
  ok <- is.finite(e)   # excludes NA and Inf (records with AreaTotal = 0)
  ad <- if (any(!is.na(lk$absdiff))) lk$absdiff else abs(lk$A_obs - lk$A_geom)
  okd <- !is.na(ad) & !is.na(lk$A_geom)
  werr_all <- c(
    weighted_mean = if (any(ok)) sum(lk$A_obs[ok] * e[ok]) / sum(lk$A_obs[ok]) else NA_real_,
    abs_over_obs  = if (any(okd)) 100 * sum(ad[okd]) / sum(lk$A_obs[okd]) else NA_real_,
    abs_over_max  = if (any(okd)) 100 * sum(ad[okd]) / sum(pmax(lk$A_obs[okd], lk$A_geom[okd])) else NA_real_)
  werr <- werr_all[[werr_method]]
  e <- e[ok]

  wB_as_U <- w; wB_as_U["U"] <- w["B"]
  list(
    Records   = nrow(x),
    Area_ha   = tot_A,
    Geom_pct  = 100 * sum(A[x$G]) / tot_A,
    TierA_pct = 100 * tier_area[["A"]] / tot_A,
    TierB_pct = 100 * tier_area[["B"]] / tot_A,
    TierU_pct = 100 * tier_area[["U"]] / tot_A,
    TierC_pct = 100 * tier_area[["C"]] / tot_A,
    WErr_pct  = werr,
    WErr_weighted_mean = werr_all[["weighted_mean"]],
    WErr_abs_over_obs  = werr_all[["abs_over_obs"]],
    WErr_abs_over_max  = werr_all[["abs_over_max"]],
    MedErr_pct = if (length(e)) median(e) else NA_real_,
    Idx_Count = 100 * sum(w[names(tier_n)] * tier_n) / nrow(x),
    Idx_Area  = 100 * sum(w[names(tier_area)] * tier_area) / tot_A,
    Idx_Area_wU_eq_wB = 100 * sum(wB_as_U[names(tier_area)] * tier_area) / tot_A
  )
}

# -----------------------------------------------------------------------------
# 3. RUN
# -----------------------------------------------------------------------------
build_tables <- function(cfg) {
  d <- read_audit(cfg)
  d[, decade := cut(year, cfg$breaks, labels = cfg$labels, right = FALSE)]
  if (any(!is.na(d$decade_src))) {
    x <- d[!is.na(decade), .N, by = .(decade, decade_src)][order(decade)]
    message("Decades from `Ano` vs column `Decada`:"); print(x)
  }

  # records left out of the table, with the reason (explains any gap with the total)
  d[, reason := fifelse(is.na(year), "no valid year (Ano)",
                fifelse(is.na(decade), "year outside 1980-2025",
                fifelse(is.na(tier), "no tier assigned",
                fifelse(is.na(A_obs), "no burned area", NA_character_))))]
  excl <- d[!is.na(reason)]
  fwrite(excl[, .(id, year, tier, A_obs, reason)], file.path(cfg$out_dir, "records_not_counted.csv"))

  message(sprintf("audit_table_prfd: %d records", nrow(d)))
  if (nrow(excl)) print(excl[, .N, by = reason])

  # NOTE: records without a tier are kept in Records / Area (they are in the database)
  # but are not in any tier; records without a valid year or decade cannot be placed.
  dd <- d[!is.na(decade)]
  dec <- dd[, indicators(.SD, cfg$w, cfg$werr_method), by = decade][order(decade)]
  ann <- dd[, indicators(.SD, cfg$w, cfg$werr_method), by = year][order(year)]
  tot <- dd[, indicators(.SD, cfg$w, cfg$werr_method)][, decade := "1980--2025"]
  dec_all <- rbind(dec, tot, fill = TRUE)

  fwrite(dec_all, file.path(cfg$out_dir, "table_integrity_decadal.csv"))
  fwrite(ann,     file.path(cfg$out_dir, "table_integrity_annual.csv"))

  # LaTeX rows in the format of the paper
  f2 <- function(x) formatC(x, format = "f", digits = 2)
  fi <- function(x) formatC(x, format = "d", big.mark = ",")
  tex <- dec[, sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                       decade, fi(Records), fi(round(Area_ha)), f2(Geom_pct),
                       f2(TierA_pct), f2(TierB_pct), f2(TierU_pct), f2(TierC_pct),
                       f2(WErr_pct), f2(MedErr_pct), f2(Idx_Count), f2(Idx_Area),
                       f2(Idx_Area_wU_eq_wB))]
  writeLines(tex, file.path(cfg$out_dir, "table_integrity_decadal.tex"))

  # consistency checks
  message(sprintf("Records in table: %s (database: %s; difference: %s)",
                  fi(sum(dec$Records)), fi(nrow(d)), fi(nrow(d) - sum(dec$Records))))
  chk <- dec[, abs(TierA_pct + TierB_pct + TierU_pct + TierC_pct - 100)]
  if (any(chk > 0.01, na.rm = TRUE))
    message("Warning: tier shares do not add up to 100% in some decades (records without tier).")
  print(dec_all, digits = 4)
  invisible(list(decadal = dec_all, annual = ann, excluded = excl))
}

res <- build_tables(cfg)
