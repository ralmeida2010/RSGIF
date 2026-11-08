# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 03 - former 3-FireDHAAManagment.R
# =============================================================================
# 3-FireDHAAManagment.R  --  Location checks and correction of end time and
#                            burned area (Step 2 of the PRFD pipeline)
#   Merges the former scripts 3-FireDHAAManagment.R, 3_1-DHFimAnalysis.R,
#   3_2-ZeroAreaAnalysis.R and the location checks of
#   4_2-Verificacao_de_Incendios_fora_de_Portugal.R
# =============================================================================
#
# PURPOSE
#   Starting from the consolidated table `fogos` (output of script 2), this
#   script (A) checks and harmonizes the ignition coordinates and the parish
#   code (INE), and (B) corrects impossible or missing end times (DHFim) and
#   missing or zero burned areas (AreaTotal) with a sequence of explicit rules.
#   The optional block C reproduces the model comparisons that justified the
#   rules.
#   The MICE imputation is NOT done here: it runs afterwards in script 4_1.
#
# INPUT
#   fogos                      consolidated records in memory (script 2)
#   data/CAOP2012/*.shp        official parish boundaries (CAOP 2012)
#   AdministrativeRelations    INE -> district/municipality/parish names (script 2)
#
# A. LOCATION CHECKS  (former 4_2)
#   A1. Count missing coordinates in each reference system.
#   A2. Convert Datum 73 / Hayford-Gauss coordinates (EPSG:20790) to PT-TM06
#       (EPSG:3763).
#   A3. Flag ignition points outside mainland Portugal (union of CAOP parishes,
#       with an optional tolerance band along the coast and border).
#   A4. (Optional, off by default) Re-geocode points outside Portugal from the
#       place name with OpenStreetMap/Nominatim; a new position is accepted only
#       if it falls inside the reported municipality.
#   A5. Assign INE from the parish polygon containing the point (all records or
#       only those without INE), refresh the names from AdministrativeRelations,
#       and recompute Lat_4326 / Lon_4326.
#   Records without coordinates are KEPT (they are toponym-located later).
#
# B. END TIME AND BURNED AREA RULES  (former 3)
#   Reference sample: closed records with AreaTotal > 0 and DHFim > DHInicio.
#   Tukey fences (Q1 - 1.5 IQR, Q3 + 1.5 IQR) of duration and of area per hour
#   are computed on this sample. For each closed record, the rules below are
#   applied in order; every rule applied is kept in DHFim_Rules ("1;6", ...):
#     1  DHFim missing                         -> DHFim = DHInicio
#     2  DHFim <= DHInicio, earlier year        -> year of DHFim = year of DHInicio
#     3  DHFim <= DHInicio, same year, earlier month -> month corrected
#     4  DHFim still before DHInicio            -> add whole days
#     5  AreaTotal missing or <= 0, duration > 0 -> AreaTotal = Coef x duration
#        (Coef: regression through the origin of area on duration, fitted on
#        records inside the fences, + 0.001)
#     6  duration = 0 and AreaTotal > 0         -> duration predicted from area
#     7  duration = 0 and AreaTotal = 0         -> AreaTotal = 0.0001 ha and
#                                                  duration predicted
#     8  area per hour AND duration both outside the fences (anomaly), only for
#        fires < rule8_max_ha (10 ha)           -> duration predicted from area
#     9  duration > 800 h                       -> duration predicted from area
#                                                  at municipality level
#   Rules 6-8 use a linear model duration ~ area fitted per parish (INE) with
#   more than 10 reference records; rule 9 the same per municipality
#   (floor(INE / 100)). A prediction is used only if it is positive.
#   Output columns: DHFimEstimado, AreaTotalEstimada, TipoEstimado (last rule,
#   as before), DHFim_Rules (all rules), HaHoraEstimado, Flag_DHFim_Estimated,
#   Flag_DHFim_Anomaly. Notes are appended to Observacoes for script 4_1:
#     "DHFim estimated (rule k)"  rules 1, 6, 7  -> duration re-imputed by MICE
#     "DHFim anomaly (rule k)"    rules 8, 9     -> re-imputed by MICE if < 10 ha
#     "DHFim date corrected (rule k)" rules 2-4 and
#     "AreaTotal from duration (rule 5)", which do not trigger a re-imputation.
#   The result is the table `fogosClosed` (closed records only).
#
# C. (Optional) MODEL DIAGNOSTICS  (former 3_1 and 3_2)
#   C1. End time: linear, mean, and median models of duration; MAE and RMSE
#       of the duration in hours.
#   C2. Area: linear (through the origin), quadratic, and exponential models of
#       area on duration; MAE, RMSE, R2.
#
# REVIEW NOTES (changes marked "# REVIEW:" in the code)
#   - Former 4_2 deleted every record without coordinates (fogos was replaced by
#     the filtered table): records without coordinates are now kept.
#   - INE from CAOP (Dicofre, text "010101") was mixed with numeric INE
#     (10101) by ifelse(): INE is now always numeric.
#   - st_join(..., st_intersects) could duplicate records lying on a parish
#     boundary: one polygon per record is now kept.
#   - The final "points outside" count used as.logical() on a sparse list and
#     failed: replaced by lengths().
#   - Parish/municipality names are no longer copied from the CAOP attributes
#     (different spelling) but refreshed from AdministrativeRelations.
#   - Rules 6-9 refitted a linear model for every record; models are now fitted
#     once per parish / municipality (same coefficients, same predictions).
#   - Only ClassificacaoRegisto == "Closed" was kept: the 1980-2000 records, whose
#     ClassificacaoRegisto is empty, were all dropped (fogosClosed and every later
#     table started in 2001). Records with an empty classification are now
#     treated as closed, and all records are kept (keep_all_records).
#   - Records with both DHFim and AreaTotal missing, or with a rank-deficient
#     parish model, stopped the old function with an error; they are handled.
#   - TipoEstimado kept only the last rule: all rules are now kept.
#   - Nothing was written in Observacoes, although script 4_1 searches it for
#     "anomaly" and "estimated": these notes are now written (block B6).
#   - The replacement of Inf values is restricted to numeric columns (it used to
#     convert dates and factors to numbers).
#   - The geocoding (A4) is off by default: it is not reproducible, it is slow
#     (1 request per second), and it is not described in the manuscript.
#   - Former 3_1 computed MAPE on absolute timestamps and grouped records by the
#     observed area per hour, which is derived from the duration being
#     predicted; the diagnostics now use durations and do not use the target.
#   - The exploratory comparisons with SEVIRI, MODIS, and EFFIS at the end of
#     the former 4_2 were removed: they need the final table and belong to the
#     validation scripts.
# =============================================================================

# Guard against dplyr verbs masked by other packages (e.g. MASS::select, raster::select, plyr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename
mutate <- dplyr::mutate; summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count

suppressPackageStartupMessages({
  library(dplyr); library(lubridate); library(sf)
})

# -----------------------------------------------------------------------------
# 0. CONFIGURATION
# -----------------------------------------------------------------------------
if (!exists("data_dir"))   data_dir   <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "data")
if (!exists("output_dir")) output_dir <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "output")
caop_path <- list.files(file.path(data_dir, "CAOP2012"), pattern = "\\.shp$", full.names = TRUE)[1]

run_location_checks   <- TRUE
datum73_mode          <- "overwrite"   # "overwrite" (as before) or "fill" (only where x_3763 is missing)
outside_tolerance_m   <- 0             # e.g. 500 to ignore points just offshore or across the border
run_geocoding         <- FALSE         # A4: OpenStreetMap re-geocoding of points outside Portugal
update_ine_from_point <- "all"         # "all" (as before) or "missing" (only records without INE)

run_time_area_rules   <- TRUE
# REVIEW: records treated as closed. The 1980-2000 records extracted from SQL
# Server have ClassificacaoRegisto empty (NA); with "Closed" only, all of them
# were dropped and fogosClosed kept 2001 onwards only.
closed_values  <- c("Closed")
closed_if_na   <- TRUE        # records with ClassificacaoRegisto = NA are closed
keep_all_records <- TRUE      # TRUE: records that are not closed are kept (without rules)
is_closed <- function(x) (tolower(trimws(x)) %in% tolower(closed_values)) | (closed_if_na & is.na(x))   # "Closed" = "closed"
# Notes written in Observacoes for script 4_1, which re-imputes with MICE the
# durations of records marked "estimated" (all) or "anomaly" (fires < 10 ha)
rules_estimated <- c(1, 6, 7)   # end time missing or zero duration -> "DHFim estimated"
rules_anomaly   <- c(8, 9)      # duration outside the fences or above 800 h -> "DHFim anomaly"
# REVIEW: the Tukey fences of rule 8 are computed on ALL fires (mostly small ones), so almost every
# large fire has both its area per hour and its duration outside the fences and was flagged as an
# "anomaly" (93% of the fires >= 100 ha), with its end time replaced by a prediction from the area.
# Rule 8 is now applied only to fires smaller than rule8_max_ha (the same 10 ha used by script 4 to
# re-impute anomalies); larger fires keep their reported end time. Rule 9 (> 800 h) is kept for all sizes.
# Use rule8_max_ha <- Inf to recover the previous behaviour.
rule8_max_ha    <- 10
run_model_diagnostics <- FALSE         # block C

# -----------------------------------------------------------------------------
# A. LOCATION CHECKS
# -----------------------------------------------------------------------------
if (run_location_checks) {
  
  # A1. Missing coordinates --------------------------------------------
  print(fogos %>% summarise(
    Records        = n(),
    NA_x20790      = sum(is.na(x_20790)),
    NA_x3763       = sum(is.na(x_3763)),
    NA_Lat4326     = sum(is.na(Lat_4326)),
    Pct_NA_x20790  = 100 * mean(is.na(x_20790)),
    Pct_NA_x3763   = 100 * mean(is.na(x_3763)),
    Pct_NA_Lat4326 = 100 * mean(is.na(Lat_4326))))
  
  # A2. Datum 73 (EPSG:20790) -> PT-TM06 (EPSG:3763) ---------------------
  sel <- !is.na(fogos$x_20790) & !is.na(fogos$y_20790)
  if (datum73_mode == "fill") sel <- sel & (is.na(fogos$x_3763) | is.na(fogos$y_3763))
  if (any(sel)) {
    xy <- st_coordinates(st_transform(
      st_as_sf(fogos[sel, c("x_20790", "y_20790")],
               coords = c("x_20790", "y_20790"), crs = 20790), 3763))
    # REVIEW: update in place (no join by Codigo)
    fogos$x_3763[sel] <- xy[, 1]
    fogos$y_3763[sel] <- xy[, 2]
    message("Datum 73 -> PT-TM06: ", sum(sel), " records converted (mode: ", datum73_mode, ").")
  }
  
  # A3. Points outside mainland Portugal ---------------------------------
  caop <- st_transform(st_read(caop_path, quiet = TRUE), 3763)
  pt_outline <- st_union(caop)
  if (outside_tolerance_m > 0) pt_outline <- st_buffer(pt_outline, outside_tolerance_m)
  
  has_xy <- !is.na(fogos$x_3763) & !is.na(fogos$y_3763)
  pts    <- st_as_sf(fogos[has_xy, c("Codigo", "x_3763", "y_3763")],
                     coords = c("x_3763", "y_3763"), crs = 3763)
  fogos$Outside_PT <- NA
  fogos$Outside_PT[has_xy] <- lengths(st_intersects(pts, pt_outline)) == 0
  message("Points outside mainland Portugal: ", sum(fogos$Outside_PT, na.rm = TRUE),
          " of ", sum(has_xy), " records with coordinates.")
  
  # A4. (Optional) re-geocoding of points outside Portugal ----------------
  if (run_geocoding && any(fogos$Outside_PT, na.rm = TRUE)) {
    library(tidygeocoder)
    tg <- fogos %>%
      filter(Outside_PT %in% TRUE) %>%
      transmute(Codigo, Concelho, address = paste(Local, Concelho, "Portugal", sep = ", ")) %>%
      geocode(address = address, method = "osm", lat = lat_new, long = lon_new)
    tg <- tg %>% filter(!is.na(lat_new))
    if (nrow(tg)) {
      g  <- st_transform(st_as_sf(tg, coords = c("lon_new", "lat_new"), crs = 4326), 3763)
      # REVIEW: accept the new point only if it lies in the reported municipality
      mun <- caop %>% group_by(Municipio) %>% summarise(.groups = "drop")
      hit <- st_join(g, mun, join = st_within)
      ok  <- !is.na(hit$Municipio) & toupper(hit$Municipio) == toupper(hit$Concelho)
      xy  <- st_coordinates(g)
      idx <- match(tg$Codigo[ok], fogos$Codigo)
      fogos$x_3763[idx] <- xy[ok, 1]
      fogos$y_3763[idx] <- xy[ok, 2]
      fogos$Outside_PT[idx] <- FALSE
      fogos$Observacoes[idx] <- paste0(coalesce(fogos$Observacoes[idx], ""),
                                       ";location re-geocoded (OSM)")
      message("Re-geocoded and accepted: ", sum(ok), " of ", nrow(tg), " records.")
    }
  }
  
  # A5. INE from the containing parish; names; WGS84 coordinates ---------
  has_xy <- !is.na(fogos$x_3763) & !is.na(fogos$y_3763)
  pts    <- st_as_sf(fogos[has_xy, c("Codigo", "x_3763", "y_3763")],
                     coords = c("x_3763", "y_3763"), crs = 3763)
  # REVIEW: first intersecting parish only -> one parish per record (no duplicates on boundaries)
  w   <- st_intersects(pts, caop)
  pos <- vapply(w, function(k) if (length(k)) k[1] else NA_integer_, integer(1))
  ine_point <- as.numeric(as.character(caop$Dicofre[pos]))   # REVIEW: numeric INE
  
  fogos$INE <- as.numeric(fogos$INE)
  upd <- which(has_xy)[!is.na(ine_point)]
  new <- ine_point[!is.na(ine_point)]
  if (update_ine_from_point == "missing") {
    keep <- is.na(fogos$INE[upd]); upd <- upd[keep]; new <- new[keep]
  }
  changed <- is.na(fogos$INE[upd]) | fogos$INE[upd] != new
  fogos$Observacoes[upd[changed]] <- paste0(coalesce(fogos$Observacoes[upd[changed]], ""),
                                            ";INE from point location")
  fogos$INE[upd] <- new
  message("INE assigned from the point location: ", length(upd),
          " records (", sum(changed), " changed).")
  
  # REVIEW: names from the official table, not from the CAOP attributes
  if (exists("AdministrativeRelations")) {
    ar <- AdministrativeRelations %>% mutate(INE = as.numeric(INE)) %>% distinct(INE, .keep_all = TRUE)
    m  <- match(fogos$INE, ar$INE)
    for (v in c("Distrito", "Concelho", "Freguesia"))
      if (v %in% names(ar)) fogos[[v]] <- coalesce(ar[[v]][m], fogos[[v]])
  }
  
  # WGS84 coordinates consistent with x_3763 / y_3763
  xy84 <- st_coordinates(st_transform(pts, 4326))
  fogos$Lon_4326[has_xy] <- xy84[, 1]
  fogos$Lat_4326[has_xy] <- xy84[, 2]
  
  n_out <- sum(fogos$Outside_PT, na.rm = TRUE)
  if (n_out == 0) message("✅ All points with coordinates lie inside mainland Portugal.") else
    message("⚠️ ", n_out, " points remain outside mainland Portugal (flag Outside_PT).")
  rm(caop, pt_outline, pts, w, pos, ine_point, upd, new, changed, has_xy, sel)
}

# -----------------------------------------------------------------------------
# B. END TIME AND BURNED AREA RULES
# -----------------------------------------------------------------------------
if (run_time_area_rules) {
  
  # B1. Reference sample and Tukey fences --------------------------------
  rdbase <- fogos %>%
    filter(is_closed(ClassificacaoRegisto), AreaTotal > 0, !is.na(DHFim), DHFim > DHInicio) %>%
    transmute(Codigo, INE = as.numeric(INE), DHInicio, DHFim, AreaTotal,
              Diff   = as.numeric(difftime(DHFim, DHInicio, units = "hours")),
              HaHora = AreaTotal / Diff)
  
  fence <- function(x) { q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  c(lo = unname(q[1] - 1.5 * diff(q)), hi = unname(q[2] + 1.5 * diff(q))) }
  fD <- fence(rdbase$Diff); fH <- fence(rdbase$HaHora); fA <- fence(rdbase$AreaTotal)
  Q1Diff <- fD["lo"]; Q3Diff <- fD["hi"]; Q1HaHora <- fH["lo"]; Q3HaHora <- fH["hi"]
  
  rdbaseArea <- rdbase %>%
    filter(Diff > fD["lo"], Diff < fD["hi"], AreaTotal > fA["lo"], AreaTotal < fA["hi"],
           HaHora > fH["lo"], HaHora < fH["hi"])
  
  # Rule 5: area = Coef x duration (regression through the origin + 0.001)
  Coef <- unname(coef(lm(AreaTotal ~ 0 + Diff, data = rdbaseArea))[1]) + 0.001
  
  # B2. Duration models, fitted ONCE per parish and per municipality ------
  # REVIEW: identical to fitting lm(Diff ~ AreaTotal) inside the loop, but fast.
  # With a constant AreaTotal the slope is NA and lm/predict return the
  # intercept; the same is done here.
  fit_groups <- function(d, key) {
    g <- split(d[, c("Diff", "AreaTotal")], key)
    g <- g[vapply(g, nrow, integer(1)) > 10]          # more than 10 reference records
    if (!length(g)) return(tibble(.key = numeric(0), a = numeric(0), b = numeric(0)))
    cf <- lapply(g, function(x) coef(lm(Diff ~ AreaTotal, data = x)))
    tibble(.key = as.numeric(names(g)),
           a = vapply(cf, `[`, numeric(1), 1), b = vapply(cf, `[`, numeric(1), 2))
  }
  m_ine <- fit_groups(rdbase, rdbase$INE)
  m_mun <- fit_groups(rdbase, floor(rdbase$INE / 100))
  a_ine <- setNames(m_ine$a, m_ine$.key); b_ine <- setNames(m_ine$b, m_ine$.key)
  a_mun <- setNames(m_mun$a, m_mun$.key); b_mun <- setNames(m_mun$b, m_mun$.key)
  
  predict_diff <- function(area, key, a, b) {
    k <- as.character(key)
    if (is.na(key) || !k %in% names(a)) return(NA_real_)
    unname(a[k] + ifelse(is.na(b[k]), 0, b[k]) * area)
  }
  
  # B3. Rules applied to one record --------------------------------------
  adjust_DHFim <- function(IN_DHInicio, IN_DHFim, IN_AreaTotal, IN_INE) {
    rules <- integer(0)
    if (is.na(IN_DHInicio)) return(list(DHFim = IN_DHFim, AreaTotal = IN_AreaTotal, rules = "start missing"))
    
    # Rule 1: missing end time
    if (is.na(IN_DHFim)) { IN_DHFim <- IN_DHInicio; rules <- c(rules, 1L) }
    
    # Rule 2: end before start with an earlier year
    if (IN_DHFim <= IN_DHInicio && year(IN_DHFim) < year(IN_DHInicio)) {
      year(IN_DHFim) <- year(IN_DHInicio); rules <- c(rules, 2L) }
    
    # Rule 3: same year, earlier month
    if (IN_DHFim <= IN_DHInicio && year(IN_DHFim) == year(IN_DHInicio) &&
        month(IN_DHFim) < month(IN_DHInicio)) {
      month(IN_DHFim) <- month(IN_DHInicio); rules <- c(rules, 3L) }
    
    # Rule 4: still before the start -> add whole days
    if (IN_DHFim < IN_DHInicio) {
      while (IN_DHFim < IN_DHInicio) IN_DHFim <- IN_DHFim + days(1)
      rules <- c(rules, 4L) }
    
    dur <- function() as.numeric(difftime(IN_DHFim, IN_DHInicio, units = "hours"))
    set_dur <- function(h) if (isTRUE(h > 0)) { IN_DHFim <<- IN_DHInicio + as.difftime(h, units = "hours"); TRUE } else FALSE
    
    # Rule 5: missing or zero area, positive duration
    if ((is.na(IN_AreaTotal) || IN_AreaTotal <= 0) && dur() > 0) {
      IN_AreaTotal <- Coef * dur(); rules <- c(rules, 5L) }
    
    # Rule 6: zero duration, positive area  (REVIEW: isTRUE() guards against NA area)
    if (dur() == 0 && isTRUE(IN_AreaTotal > 0)) {
      if (set_dur(predict_diff(IN_AreaTotal, IN_INE, a_ine, b_ine))) rules <- c(rules, 6L) }
    
    # Rule 7: zero duration, zero area
    if (dur() == 0 && isTRUE(IN_AreaTotal == 0)) {
      IN_AreaTotal <- 0.0001
      if (set_dur(predict_diff(IN_AreaTotal, IN_INE, a_ine, b_ine))) rules <- c(rules, 7L) }
    
    # Rule 8: area per hour AND duration outside the fences (anomaly)
    d <- dur(); hh <- IN_AreaTotal / d
    if (isTRUE(IN_AreaTotal < rule8_max_ha && (hh < Q1HaHora || hh > Q3HaHora) && (d < Q1Diff || d > Q3Diff))) {
      if (set_dur(predict_diff(IN_AreaTotal, IN_INE, a_ine, b_ine))) rules <- c(rules, 8L) }
    
    # Rule 9: duration above 800 h -> municipality model
    if (dur() > 800) {
      if (set_dur(predict_diff(IN_AreaTotal, floor(IN_INE / 100), a_mun, b_mun))) rules <- c(rules, 9L) }
    
    list(DHFim = IN_DHFim, AreaTotal = IN_AreaTotal,
         rules = if (length(rules)) paste(rules, collapse = ";") else "0")
  }
  
  # B4. Apply to all closed records -------------------------------------
  print(table(ClassificacaoRegisto = fogos$ClassificacaoRegisto, useNA = "ifany"))
  message("Records treated as closed: ", sum(is_closed(fogos$ClassificacaoRegisto)), " of ", nrow(fogos))
  rd <- fogos %>%
    filter(is_closed(ClassificacaoRegisto)) %>%
    select(Codigo, INE, DHInicio, DHFim, AreaTotal) %>%
    mutate(INE = as.numeric(INE))
  
  out <- mapply(adjust_DHFim, rd$DHInicio, rd$DHFim, rd$AreaTotal, rd$INE, SIMPLIFY = FALSE)
  rd$DHFimEstimado     <- as.POSIXct(vapply(out, function(z) as.numeric(z$DHFim), numeric(1)),
                                     origin = "1970-01-01", tz = "UTC")
  rd$AreaTotalEstimada <- vapply(out, function(z) as.numeric(z$AreaTotal), numeric(1))
  rd$DHFim_Rules       <- vapply(out, `[[`, character(1), "rules")
  # Last rule only, as in the previous version (used by later scripts)
  rd$TipoEstimado      <- suppressWarnings(as.integer(sub(".*;", "", rd$DHFim_Rules)))
  
  print(sort(table(rd$DHFim_Rules), decreasing = TRUE))
  
  # B5. Join to the fire table -------------------------------------------
  fogosClosed <- fogos %>%
    select(-any_of(c("DHFimEstimado", "AreaTotalEstimada", "AreaTotalEstimado", "TipoEstimado", "DHFim_Rules"))) %>%
    { if (keep_all_records) left_join(., rd %>% select(Codigo, DHFimEstimado, AreaTotalEstimada, TipoEstimado, DHFim_Rules), by = "Codigo")
      else inner_join(., rd %>% select(Codigo, DHFimEstimado, AreaTotalEstimada, TipoEstimado, DHFim_Rules), by = "Codigo") } %>%
    mutate(DHFim_Rules = coalesce(DHFim_Rules, "0")) %>%
    mutate(
      HaHoraEstimado = AreaTotalEstimada / as.numeric(difftime(DHFimEstimado, DHInicio, units = "hours")),
      Distrito = replace(Distrito, Distrito == "Viana Do Castelo", "Viana do Castelo"),
      Observacoes = coalesce(Observacoes, "")
    ) %>%
    # REVIEW: Inf replaced only in numeric columns (dates and factors untouched)
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), 0.0000001, .))) %>%
    select(-any_of(c("...1", "...2")))
  
  # B6. Notes in Observacoes ("estimated" / "anomaly") ---------------------
  # REVIEW: these words are read by script 4_1 (L51-52). Rule 5 (area) and
  # rules 2-4 (date corrections) use other wording, so that they do not
  # trigger a new imputation of the duration.
  has_rule <- function(r) vapply(strsplit(fogosClosed$DHFim_Rules, ";"),
                                 function(k) any(k %in% as.character(r)), logical(1))
  rr <- function(r) vapply(strsplit(fogosClosed$DHFim_Rules, ";"),
                           function(k) paste(intersect(k, as.character(r)), collapse = ";"), character(1))
  
  f_est <- has_rule(rules_estimated); f_ano <- has_rule(rules_anomaly)
  f_dat <- has_rule(2:4);             f_are <- has_rule(5)
  obs <- fogosClosed$Observacoes
  obs <- ifelse(f_est, paste0(obs, ";DHFim estimated (rule ", rr(rules_estimated), ")"), obs)
  obs <- ifelse(f_ano, paste0(obs, ";DHFim anomaly (rule ",   rr(rules_anomaly),   ")"), obs)
  obs <- ifelse(f_dat, paste0(obs, ";DHFim date corrected (rule ", rr(2:4), ")"), obs)
  obs <- ifelse(f_are, paste0(obs, ";AreaTotal from duration (rule 5)"), obs)
  fogosClosed$Observacoes <- sub("^;", "", obs)
  fogosClosed$Flag_DHFim_Estimated <- f_est
  fogosClosed$Flag_DHFim_Anomaly   <- f_ano
  
  message("Notes written: estimated = ", sum(f_est), ", anomaly = ", sum(f_ano),
          " (of which < 10 ha: ", sum(f_ano & fogosClosed$AreaTotal < 10, na.rm = TRUE),
          "; fires >= 100 ha flagged: ", sum(f_ano & fogosClosed$AreaTotal >= 100, na.rm = TRUE), " of ",
          sum(fogosClosed$AreaTotal >= 100, na.rm = TRUE),
          "), date corrected = ", sum(f_dat), ", area from duration = ", sum(f_are))
  rm(has_rule, rr, f_est, f_ano, f_dat, f_are, obs)
  
  if ("Tipo" %in% names(fogosClosed)) fogosClosed <- rename(fogosClosed, TipoIncendio = Tipo)
  
  message("fogosClosed: ", nrow(fogosClosed), " records (of ", nrow(fogos), "); ",
          sum(fogosClosed$Ano < 2001, na.rm = TRUE), " before 2001.")
  rm(out, rd, rdbaseArea, fence, fD, fH, fA, fit_groups, m_ine, m_mun)
}

# -----------------------------------------------------------------------------
# C. (OPTIONAL) MODEL DIAGNOSTICS  (former 3_1 and 3_2)
# -----------------------------------------------------------------------------
if (run_model_diagnostics && exists("rdbase")) {
  
  metrics <- function(obs, pred) c(MAE = mean(abs(obs - pred), na.rm = TRUE),
                                   RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)))
  
  # C1. Duration (hours) from area: per-parish linear model vs mean and
  #     median area-per-hour of the parish.
  # REVIEW: errors on durations (not timestamps); groups do not use the
  # observed area per hour, which is computed from the target.
  d1 <- rdbase %>% filter(Diff > Q1Diff, Diff < Q3Diff) %>%
    group_by(INE) %>% filter(n() > 10) %>%
    mutate(pred_lin    = predict(lm(Diff ~ AreaTotal)),
           pred_mean   = AreaTotal / mean(HaHora),
           pred_median = AreaTotal / median(HaHora)) %>% ungroup()
  print(rbind(Linear = metrics(d1$Diff, d1$pred_lin),
              Mean   = metrics(d1$Diff, d1$pred_mean),
              Median = metrics(d1$Diff, d1$pred_median)))
  
  # C2. Area from duration on the sample inside the fences (rule 5)
  rdbaseArea_diag <- rdbase %>%
    filter(Diff > Q1Diff, Diff < Q3Diff, HaHora > Q1HaHora, HaHora < Q3HaHora)
  quad <- lm(AreaTotal ~ 0 + poly(Diff, 2), data = rdbaseArea_diag)
  expm <- nls(AreaTotal ~ a * exp(b * Diff), data = rdbaseArea_diag, start = list(a = 1, b = 0.01))
  r2 <- function(obs, pred) 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
  p  <- list(Linear = Coef * rdbaseArea_diag$Diff,
             Quadratic = predict(quad), Exponential = predict(expm))
  print(t(sapply(p, function(x) c(metrics(rdbaseArea_diag$AreaTotal, x),
                                  R2 = r2(rdbaseArea_diag$AreaTotal, x)))))
}