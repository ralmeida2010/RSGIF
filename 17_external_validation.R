# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 17 - PRFD External Validation (EFFIS & FIRMS)
# =============================================================================
# Validação externa do PRFD utilizando exclusivamente:
#   (1) EFFIS Rapid Damage Assessment (Polígonos de área ardida)
#   (2) NASA FIRMS (Incêndios ativos / Hotspots MODIS e VIIRS)
#
# REVIEW (correções desta versão)
#   - EFFIS: a data (FIREDATE) é lida por vários formatos (texto, Date, época numérica); o
#     script PÁRA com um diagnóstico se nenhuma data for lida ou se o filtro temporal ficar
#     vazio (antes escrevia uma tabela só com o cabeçalho). Usa st_filter (não recorta os
#     polígonos, por isso as áreas não mudam).
#   - EFFIS: compara também a duração (dias entre FIREDATE e FINALDATE) com a do PRFD nos eventos ligados
#     (teste independente das durações dos fogos grandes, que no I.3 são muitas vezes modeladas).
#   - EFFIS: duas regras de correspondência: "melhor área" (otimista: escolhe o candidato
#     cuja área mais se parece com a do EFFIS) e "mais próximo" (neutra: menor distância ao
#     polígono e depois menor diferença de data). A comparação anual usa só polígonos EFFIS
#     >= effis_min_ha, como os fogos do PRFD.
#   - FIRMS: o script já não depende de um objeto firms_pt_sf pré-existente: lê os .csv do
#     FIRMS em cfg$firms_local_dir (ou usa firms_pt_sf se existir na sessão).
#   - Limites a ter em conta na leitura: a deteção de fogos ativos é "um hotspot a menos de
#     1,5 km (MODIS) / 0,75 km (VIIRS) do ponto de ignição durante [início - 1 h, fim + 2 h]";
#     subestima os fogos grandes, que se afastam desse ponto. A "passagem" é inferida pelas
#     horas de qualquer hotspot do ficheiro (não pelas órbitas), logo é um limite inferior.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(sf); library(httr2); library(ggplot2)
})

# -----------------------------------------------------------------------------
# 0. CONFIGURAÇÃO GLOBAL (Caminhos absolutos)
# -----------------------------------------------------------------------------
if (!exists("base_path")) base_path <- if (exists("project_dir")) project_dir else Sys.getenv("RSGIF_DIR", getwd())

cfg <- list(
  out_dir   = file.path(base_path, "validation_out"),
  cache_dir = file.path(base_path, "validation_cache"),
  tz_local  = "Europe/Lisbon",
  crs_work  = 3763,                       # PT-TM06 (metres)
  
  # --- PRFD ---
  prfd_file  = file.path(base_path, "output", "fogos.csv"),   # written by 13_final_export_reports.R
  prfd_cols  = list(id = "Codigo", start = "DHInicio", end = "DHFim",
                    x = "x_3763", y = "y_3763", ba = "AreaTotal", r95 = NA),
  
  # --- EFFIS ---
  effis_file        = file.path(base_path, "effis_layer/modis.ba.poly.shp"), 
  effis_cols        = list(date = "FIREDATE", country = "COUNTRY", area = "AREA_HA"),
  effis_final_col   = "FINALDATE",      # opcional: fim do mapeamento (última deteção)
  effis_country     = "PT",
  effis_days_before = 3,                  
  effis_days_after  = 1,
  effis_buffer_m    = 1000,               
  effis_min_ha      = 30,
  prfd_last_year    = 2025,               # último ano completo do PRFD (para a comparação anual)
  
  # --- FIRMS ---
  firms_local_dir   = file.path(base_path, "FIRMS"),
  af_radius_m       = c(MODIS = 1500, VIIRS = 750), 
  af_hours_before   = 1,                    
  af_hours_after    = 2                     
)

dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cfg$cache_dir, showWarnings = FALSE, recursive = TRUE)

# Acumulador de sumários
summary_rows <- list()
add_summary <- function(test, metric, value, note = "") {
  summary_rows[[length(summary_rows) + 1]] <<-
    data.table(test = test, metric = metric, value = value, note = note)
}

size_class <- function(ba) cut(ba, c(-Inf, 1, 10, 30, 100, 500, 1000, Inf), right = FALSE,
                               labels = c("<1", "1-10", "10-30", "30-100", "100-500", "500-1000", ">=1000"))

dur_class <- function(h) cut(h, c(-Inf, 1, 3, 6, 12, 24, Inf), right = FALSE,
                             labels = c("<1 h", "1-3 h", "3-6 h", "6-12 h", "12-24 h", ">=24 h"))

annual_stats <- function(a, b) {            
  ok <- is.finite(a) & is.finite(b) & b > 0
  list(n_years = sum(ok),
       r       = if (sum(ok) > 2) cor(a[ok], b[ok]) else NA_real_,
       mape    = mean(abs(a[ok] - b[ok]) / b[ok]) * 100,
       diff_total_pct = (sum(a[ok]) / sum(b[ok]) - 1) * 100)
}

binom_ci <- function(k, n) {               
  if (n == 0) return(c(NA, NA))
  round(100 * binom.test(k, n)$conf.int[1:2], 1)
}

rate_table <- function(dt, by, hit) {      
  out <- dt[, .(n = .N, detected = sum(get(hit))), by = by]
  out[, pct := round(100 * detected / n, 1)]
  out[, c("ci_low", "ci_high") := as.list(binom_ci(detected, n)), by = seq_len(nrow(out))]
  setorderv(out, by)
  out[]
}

# =============================================================================
# 1. CARREGAR PRFD
# =============================================================================
message(">>> A carregar PRFD...")
cc <- cfg$prfd_cols
if (exists("fogos")) {
  d <- as.data.table(fogos)
} else {
  d <- fread(cfg$prfd_file, encoding = "UTF-8", fill = Inf,
             colClasses = list(character = c(cc$start, cc$end)))
}

d[, X_num := suppressWarnings(as.numeric(get(cc$x)))]
d[, Y_num := suppressWarnings(as.numeric(get(cc$y)))]
d_valid <- d[!is.na(X_num) & !is.na(Y_num) & X_num != 0 & Y_num != 0]

parse_t <- function(v) {
  # Se já for data/hora ou Date, aproveitamos diretamente sem converter para texto
  if (inherits(v, c("POSIXct", "POSIXt", "Date"))) {
    return(as.POSIXct(v, tz = cfg$tz_local))
  }
  
  # Caso contrário, se for texto, tratamos da conversão de forma segura
  v_char <- as.character(v)
  res <- suppressWarnings(as.POSIXct(v_char, tz = cfg$tz_local, tryFormats = c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y"
  )))
  
  na_idx <- which(is.na(res) & !is.na(v_char) & v_char != "" & v_char != "NA")
  if (length(na_idx) > 0) {
    for (i in na_idx) {
      val_parsed <- suppressWarnings(tryCatch(
        as.POSIXct(v_char[i], tz = cfg$tz_local),
        error = function(e) as.POSIXct(NA, tz = cfg$tz_local)
      ))
      if (!is.na(val_parsed)) res[i] <- val_parsed
    }
  }
  res
}

prfd <- data.table(
  id  = d_valid[[cc$id]], X = d_valid$X_num, Y = d_valid$Y_num,
  ba  = as.numeric(d_valid[[cc$ba]]),
  t0  = parse_t(d_valid[[cc$start]]), t1  = parse_t(d_valid[[cc$end]])
)
prfd[, `:=`(year = as.integer(format(t0, "%Y")), date0 = as.Date(t0, tz = cfg$tz_local))]
prfd[, dur_h := as.numeric(difftime(t1, t0, units = "hours"))]
prfd[, size := size_class(ba)]
prfd <- prfd[!is.na(t0)]
message(sprintf("PRFD carregado: %d registos válidos.", nrow(prfd)))

# =============================================================================
# 2. EFFIS (Processamento Linear e Estruturas Globais)
# =============================================================================
parse_effis_date <- function(v) {
  if (inherits(v, "Date"))  return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) {                                   # época em ms, em s, ou dias
    m <- median(v, na.rm = TRUE)
    return(if (m > 1e11) as.Date(as.POSIXct(v / 1000, origin = "1970-01-01", tz = "UTC"))
           else if (m > 1e8) as.Date(as.POSIXct(v, origin = "1970-01-01", tz = "UTC"))
           else as.Date(v, origin = "1970-01-01"))
  }
  x   <- trimws(as.character(v)); x10 <- substr(x, 1, 10)
  out <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  for (fmt in c("%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y", "%Y.%m.%d", "%d.%m.%Y")) {
    todo <- is.na(out) & !is.na(x10) & nzchar(x10)
    if (!any(todo)) break
    out[todo] <- as.Date(x10[todo], format = fmt)
  }
  todo <- is.na(out) & grepl("^[0-9]{8}$", x)
  if (any(todo)) out[todo] <- as.Date(x[todo], format = "%Y%m%d")
  out
}

effis_clean_table <- NULL
if (file.exists(cfg$effis_file)) {
  message(">>> EFFIS: A carregar shapefile e a converter CRS (EPSG:3763)...")
  effiss_raw <- st_read(cfg$effis_file, quiet = TRUE)
  ec <- cfg$effis_cols
  message("    Campos do shapefile: ", paste(names(effiss_raw), collapse = ", "))
  message("    ", nrow(effiss_raw), " polígonos no ficheiro")
  falta <- setdiff(unlist(ec), names(effiss_raw))
  if (length(falta)) stop("Campos EFFIS em falta: ", paste(falta, collapse = ", "), ". Ajuste cfg$effis_cols.")
  
  pais <- as.character(effiss_raw[[ec$country]])
  message("    Países (amostra): ", paste(head(sort(unique(pais)), 10), collapse = ", "))
  effiss_raw <- effiss_raw[pais == cfg$effis_country, ]
  message("    ", nrow(effiss_raw), " polígonos com ", ec$country, " = ", cfg$effis_country)
  if (nrow(effiss_raw) == 0) stop("Nenhum polígono EFFIS com ", ec$country, " = ", cfg$effis_country)
  
  effiss_raw <- st_make_valid(st_transform(effiss_raw, cfg$crs_work))
  
  # Datas: lidas por vários formatos; para com diagnóstico se não houver nenhuma válida
  d_eff <- parse_effis_date(effiss_raw[[ec$date]])
  message(sprintf("    Datas EFFIS lidas: %d de %d válidas; intervalo %s a %s", sum(!is.na(d_eff)), length(d_eff),
                  format(min(d_eff, na.rm = TRUE)), format(max(d_eff, na.rm = TRUE))))
  if (all(is.na(d_eff)))
    stop("Não consegui ler nenhuma data do campo ", ec$date, ". Amostra: ",
         paste(head(as.character(effiss_raw[[ec$date]]), 5), collapse = " | "))
  # Filtro temporal feito sobre VETORES (não sobre colunas do objeto sf): o ano sai do texto da data
  yr_eff  <- as.integer(substr(as.character(d_eff), 1, 4))
  fin_eff <- if (cfg$effis_final_col %in% names(effiss_raw)) parse_effis_date(effiss_raw[[cfg$effis_final_col]]) else
    as.Date(rep(NA_real_, length(d_eff)), origin = "1970-01-01")
  ha_eff  <- as.numeric(effiss_raw[[ec$area]])
  keep    <- which(!is.na(d_eff) & !is.na(yr_eff) & yr_eff >= 1980 & yr_eff <= 2030)
  message(sprintf("    Anos lidos (amostra): %s | válidos: %d de %d", paste(head(unique(yr_eff[!is.na(yr_eff)]), 6), collapse = ", "),
                  length(keep), length(d_eff)))
  if (!length(keep)) stop("O filtro temporal eliminou todos os polígonos EFFIS. Amostra de datas: ",
                          paste(head(as.character(d_eff), 5), collapse = " | "), " | anos: ",
                          paste(head(yr_eff, 5), collapse = " | "))
  effiss_raw <- effiss_raw[keep, ]
  effiss_raw$effis_date  <- d_eff[keep]
  effiss_raw$effis_final <- fin_eff[keep]
  effiss_raw$effis_ha    <- ha_eff[keep]
  effiss_raw$year        <- yr_eff[keep]
  message(sprintf(">>> EFFIS [Confirmação Temporal]: %d polígonos válidos (anos %s a %s).", nrow(effiss_raw),
                  min(yr_eff[keep]), max(yr_eff[keep])))
  
  # Pré-seleção geográfica SEM recortar (as áreas mantêm-se)
  bb <- st_bbox(c(xmin = min(prfd$X) - 5000, ymin = min(prfd$Y) - 5000,
                  xmax = max(prfd$X) + 5000, ymax = max(prfd$Y) + 5000), crs = st_crs(cfg$crs_work))
  effis_pt_sf <- st_filter(effiss_raw, st_as_sfc(bb))
  effis_pt_sf$effis_id <- seq_len(nrow(effis_pt_sf))
  message(sprintf(">>> EFFIS [Confirmação Geométrica]: %d polígonos na área do PRFD.", nrow(effis_pt_sf)))
  if (nrow(effis_pt_sf) == 0) stop("Nenhum polígono EFFIS dentro da área do PRFD (verifique o CRS).")
  
  effis_clean_table <- as.data.table(st_drop_geometry(effis_pt_sf[, c("effis_id", "effis_date", "effis_final", "year", "effis_ha")]))
  setorder(effis_clean_table, year, effis_id)
  fwrite(effis_clean_table, file.path(cfg$out_dir, "effis_processed_pt_table.csv"))
  message(">>> Tabela intermédia gravada: validation_out/effis_processed_pt_table.csv")
  message(sprintf("    EFFIS: mediana %.1f ha; P90 %.0f ha; máximo %.0f ha", median(effis_clean_table$effis_ha, na.rm = TRUE),
                  quantile(effis_clean_table$effis_ha, 0.9, na.rm = TRUE), max(effis_clean_table$effis_ha, na.rm = TRUE)))
  
  
  # Comparação anual da área ardida: independente da correspondência evento a evento
  anos_c <- sort(unique(effis_clean_table$year)); anos_c <- anos_c[anos_c <= cfg$prfd_last_year]
  ann_p <- prfd[year %in% anos_c, .(prfd_all_ha = sum(ba, na.rm = TRUE), prfd_big_ha = sum(ba[ba >= cfg$effis_min_ha], na.rm = TRUE)), by = year]
  ann_e <- effis_clean_table[year %in% anos_c, .(effis_all_ha = sum(effis_ha, na.rm = TRUE),
                                                 effis_big_ha = sum(effis_ha[effis_ha >= cfg$effis_min_ha], na.rm = TRUE)), by = year]
  ann0 <- merge(ann_p, ann_e, by = "year")
  ann0[, `:=`(ratio_all = round(prfd_all_ha / effis_all_ha, 3), ratio_big = round(prfd_big_ha / effis_big_ha, 3))]
  setorder(ann0, year)
  fwrite(ann0, file.path(cfg$out_dir, "effis_annual.csv"))
  s_all <- annual_stats(ann0$prfd_all_ha, ann0$effis_all_ha)
  s_big <- annual_stats(ann0$prfd_big_ha, ann0$effis_big_ha)
  yr_txt <- sprintf("%d-%d", min(ann0$year), max(ann0$year))
  add_summary("EFFIS", "Annual burned area, all fires: Pearson r", round(s_all$r, 3), yr_txt)
  add_summary("EFFIS", "Annual burned area, all fires: MAPE vs EFFIS (%)", round(s_all$mape, 1), yr_txt)
  add_summary("EFFIS", "Annual burned area, all fires: PRFD total vs EFFIS total (%)", round(s_all$diff_total_pct, 1), yr_txt)
  add_summary("EFFIS", sprintf("Annual burned area, fires >= %d ha: Pearson r", cfg$effis_min_ha), round(s_big$r, 3), yr_txt)
  add_summary("EFFIS", sprintf("Annual burned area, fires >= %d ha: MAPE vs EFFIS (%%)", cfg$effis_min_ha), round(s_big$mape, 1), yr_txt)
  message(sprintf("    Área anual PRFD vs EFFIS (%s): r = %.3f; MAPE = %.1f%%; total PRFD/EFFIS = %+.1f%%", yr_txt, s_all$r, s_all$mape, s_all$diff_total_pct))
  
  yrs <- range(effis_pt_sf$year, na.rm = TRUE)
  pp_effis <- prfd[year >= yrs[1] & year <= yrs[2]]
  if (nrow(pp_effis) > 0) {
    pts <- st_as_sf(pp_effis, coords = c("X", "Y"), crs = cfg$crs_work, remove = FALSE)
    eb  <- st_buffer(effis_pt_sf[, c("effis_id", "effis_date", "effis_ha")], cfg$effis_buffer_m)
    jj  <- st_join(pts, eb, join = st_intersects, left = FALSE)
    j   <- as.data.table(st_drop_geometry(jj))
    message(sprintf("    Pares ponto-polígono (buffer de %d m): %d", cfg$effis_buffer_m, nrow(j)))
    if (nrow(j) > 0) {
      pol <- st_geometry(effis_pt_sf)[match(j$effis_id, effis_pt_sf$effis_id)]
      j[, dist_m := as.numeric(st_distance(st_geometry(jj), pol, by_element = TRUE))]
      j[, dday   := as.numeric(date0 - effis_date)]
      j <- j[dday >= -cfg$effis_days_before & dday <= cfg$effis_days_after]
      message(sprintf("    Pares após a janela temporal (-%d/+%d dias): %d", cfg$effis_days_before, cfg$effis_days_after, nrow(j)))
    }
    if (nrow(j) > 0) {
      j[, dlog := abs(log1p(ba) - log1p(effis_ha))]
      best_area <- j[order(dlog)][, .SD[1], by = effis_id]                 # otimista: escolhe pela área
      best_near <- j[order(dist_m, abs(dday), dlog)][, .SD[1], by = effis_id]   # neutra: espaço e tempo
      
      ev <- copy(effis_clean_table)
      ev <- merge(ev, best_near[, .(effis_id, prfd_id = id, prfd_ba = ba, dist_m, dday)], by = "effis_id", all.x = TRUE)
      ev <- merge(ev, best_area[, .(effis_id, prfd_id_area = id, prfd_ba_area = ba)], by = "effis_id", all.x = TRUE)
      ev <- merge(ev, prfd[, .(prfd_id = id, prfd_dur_h = dur_h)], by = "prfd_id", all.x = TRUE)
      ev[, `:=`(effis_dur_d = as.numeric(effis_final - effis_date) + 1, prfd_dur_d = prfd_dur_h / 24)]
      ev[, matched := !is.na(prfd_id)]
      ev[matched == TRUE, rel_diff_pct      := 100 * (prfd_ba      - effis_ha) / effis_ha]
      ev[matched == TRUE, rel_diff_pct_area := 100 * (prfd_ba_area - effis_ha) / effis_ha]
      ev[, effis_class := cut(effis_ha, c(-Inf, 30, 100, 500, 1000, Inf), right = FALSE,
                              labels = c("<30", "30-100", "100-500", "500-1000", ">=1000"))]
      fwrite(ev, file.path(cfg$out_dir, "effis_event_match.csv"))
      
      k <- sum(ev$matched); n <- nrow(ev)
      add_summary("EFFIS", "EFFIS polygons with a PRFD record (%)", round(100 * k / n, 1), sprintf("%d of %d", k, n))
      ev30 <- ev[effis_ha >= cfg$effis_min_ha]
      if (nrow(ev30)) add_summary("EFFIS", sprintf("EFFIS polygons >= %d ha with a PRFD record (%%)", cfg$effis_min_ha),
                                  round(100 * mean(ev30$matched), 1), sprintf("%d of %d", sum(ev30$matched), nrow(ev30)))
      add_summary("EFFIS", "Median relative area difference, PRFD vs EFFIS (%), nearest match", round(median(ev$rel_diff_pct, na.rm = TRUE), 1), "matched events")
      add_summary("EFFIS", "Median relative area difference (%), best-area match (optimistic)", round(median(ev$rel_diff_pct_area, na.rm = TRUE), 1), "matched events")
      add_summary("EFFIS", "Matched with |difference| <= 20% (%), nearest match", round(100 * mean(abs(ev[matched == TRUE]$rel_diff_pct) <= 20), 1), "")
      valid_cor <- ev[!is.na(prfd_ba) & !is.na(effis_ha)]
      corr_val <- if (nrow(valid_cor) > 2) cor(valid_cor$prfd_ba, valid_cor$effis_ha, method = "spearman", use = "complete.obs") else NA_real_
      add_summary("EFFIS", "Spearman r of event areas (nearest match)", round(corr_val, 3), "")
      dd <- ev[matched == TRUE & is.finite(effis_dur_d) & is.finite(prfd_dur_d) & prfd_dur_d >= 0]
      if (nrow(dd) > 2) {
        # EFFIS: dias entre a primeira e a última deteção por satélite (limite inferior da duração real)
        add_summary("EFFIS", "Fire duration: Spearman r, PRFD vs EFFIS (days)", round(cor(dd$prfd_dur_d, dd$effis_dur_d, method = "spearman"), 3), sprintf("n = %d", nrow(dd)))
        add_summary("EFFIS", "Fire duration: median PRFD / EFFIS ratio", round(median(dd$prfd_dur_d / dd$effis_dur_d), 2), "EFFIS = first to last detection date")
      }
      fwrite(ev[, .(n = .N, matched_pct = round(100 * mean(matched), 1),
                    median_rel_diff_pct = round(median(rel_diff_pct, na.rm = TRUE), 1)), by = effis_class][order(effis_class)],
             file.path(cfg$out_dir, "effis_match_by_class.csv"))
      
      big <- pp_effis[ba >= cfg$effis_min_ha]
      if (nrow(big) > 0) {
        big[, effis_hit := id %in% best_near$id]
        det <- rate_table(big, c("size"), "effis_hit")
        fwrite(det[!is.na(size)], file.path(cfg$out_dir, "effis_detection_by_size.csv"))
        add_summary("EFFIS", sprintf("PRFD fires >= %d ha matched by EFFIS (%%)", cfg$effis_min_ha), round(100 * mean(big$effis_hit), 1), "")
      }
    } else message(">>> EFFIS: nenhum par ponto-polígono dentro da janela temporal: sem correspondência.")
  }
} else {
  message(">>> Ficheiro EFFIS não encontrado - ignorado: ", cfg$effis_file)
}

# =============================================================================
# 3. FIRMS (Processamento Limpo, Extração de Ano e Cruzamento)
# =============================================================================
message(">>> A processar colunas temporais e sensores do FIRMS...")

# Carregar os hotspots do FIRMS: usa firms_pt_sf se existir na sessão; senão lê os .csv de
# cfg$firms_local_dir (arquivos MODIS e VIIRS descarregados do NASA FIRMS)
load_firms_local <- function(dir) {
  files <- list.files(dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("Sem ficheiros .csv do FIRMS em ", dir)
  out <- lapply(files, function(f) {
    x <- tryCatch(fread(f, showProgress = FALSE), error = function(e) NULL)
    if (is.null(x) || !all(c("latitude", "longitude", "acq_date", "acq_time") %in% names(x))) return(NULL)
    if (!"satellite"  %in% names(x)) x[, satellite  := NA_character_]
    if (!"instrument" %in% names(x)) x[, instrument := if (grepl("MODIS|_M-C|M-C61", basename(f), ignore.case = TRUE)) "MODIS" else "VIIRS"]
    if ("type" %in% names(x)) x <- x[is.na(type) | type == 0]            # só presumíveis incêndios de vegetação
    x[, .(latitude = as.numeric(latitude), longitude = as.numeric(longitude), acq_date = as.character(acq_date),
          acq_time = suppressWarnings(as.integer(acq_time)), satellite = as.character(satellite),
          instrument = as.character(instrument))]
  })
  x <- rbindlist(out[!vapply(out, is.null, logical(1))])
  if (!nrow(x)) stop("Nenhum ficheiro do FIRMS com as colunas latitude, longitude, acq_date, acq_time em ", dir)
  x <- unique(x, by = c("latitude", "longitude", "acq_date", "acq_time", "satellite"))       # NRT + arquivo
  x <- x[longitude > -10 & longitude < -6 & latitude > 36.8 & latitude < 42.3]               # Portugal continental
  pts <- st_transform(st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326), cfg$crs_work)
  xy  <- st_coordinates(pts)
  x[, `:=`(X = xy[, 1], Y = xy[, 2], firm_id = .I)]
  message(sprintf("    FIRMS: %d ficheiros, %d hotspots em Portugal continental", length(files), nrow(x)))
  x[]
}

firms_dt <- if (exists("firms_pt_sf")) {
  z <- as.data.table(st_drop_geometry(firms_pt_sf))
  if (!all(c("X", "Y") %in% names(z))) { xy <- st_coordinates(st_transform(firms_pt_sf, cfg$crs_work)); z[, `:=`(X = xy[, 1], Y = xy[, 2])] }
  if (!"firm_id" %in% names(z)) z[, firm_id := .I]
  z
} else load_firms_local(cfg$firms_local_dir)

# 2. Criar a coluna de tempo 't' UTC
date_str <- as.character(firms_dt$acq_date)
time_str <- sprintf("%04d", as.integer(firms_dt$acq_time))
firms_dt[, t := as.POSIXct(paste(date_str, time_str), format = "%Y-%m-%d %H%M", tz = "UTC")]

# 3. Ano civil
firms_dt[, year := as.integer(substr(as.character(t), 1, 4))]

# 4. Classificação operacional de sensores (MODIS vs. VIIRS)
firms_dt[, sensor := ifelse(
  grepl("MODIS", instrument, ignore.case = TRUE) | satellite %in% c("Terra", "Aqua", "T", "A"),
  "MODIS", "VIIRS")]

# 5. Filtrar mantendo apenas registos com ano válido (2000 a 2030)
firms_dt <- firms_dt[!is.na(t) & year >= 2000 & year <= 2030, .(firm_id, sensor, satellite, t, year, X, Y)]

message(sprintf(">>> FIRMS [Pronto]: %d registos válidos em memória para cruzamento.", nrow(firms_dt)))

# 6. Cruzamento Espacial e Temporal com o PRFD
if (nrow(firms_dt) > 0) {
  f_prfd <- prfd[!is.na(t1) & dur_h >= 0 & year >= 2000,
                 .(id, X, Y, ba, size, dur_h,
                   w0 = as.POSIXct(t0, tz = "UTC") - cfg$af_hours_before * 3600,
                   w1 = as.POSIXct(t1, tz = "UTC") + cfg$af_hours_after * 3600)]
  
  cell <- 5000
  firms_dt[, `:=`(cx = floor(X / cell), cy = floor(Y / cell))]
  f_prfd[, `:=`(cx0 = floor(X / cell), cy0 = floor(Y / cell))]
  
  res_af <- list()
  for (s in c("MODIS", "VIIRS")) {
    hs <- firms_dt[sensor == s]
    if (!nrow(hs)) next
    first <- min(hs$t)
    fs <- f_prfd[w1 >= first]
    rad <- cfg$af_radius_m[[s]]
    
    nb <- CJ(dx = -1:1, dy = -1:1)
    fe <- fs[rep(seq_len(.N), each = nrow(nb)), .(id, X, Y, w0, w1, cx0, cy0)]
    fe[, `:=`(cx = cx0 + rep(nb$dx, nrow(fs)), cy = cy0 + rep(nb$dy, nrow(fs)))]
    
    j_af <- hs[fe, on = .(cx, cy, t >= w0, t <= w1), nomatch = 0L,
               .(id, d = sqrt((x.X - i.X)^2 + (x.Y - i.Y)^2))]
    
    hit_ids <- unique(j_af[d <= rad, id])
    
    ot <- data.table(t = sort(unique(hs$t)))
    ov <- ot[fs, on = .(t >= w0, t <= w1), .(over = !is.na(x.t[1])), by = .EACHI]
    
    fs[, `:=`(sensor = s, detected = id %in% hit_ids, overpass = ov$over)]
    res_af[[s]] <- fs[, .(id, sensor, size, dur_h, detected, overpass)]
  }
  
  if (length(res_af) > 0) {
    m_af <- rbindlist(res_af)
    m_af[, dur := dur_class(dur_h)]
    
    bysize <- rate_table(m_af, c("sensor", "size"), "detected")
    ov_sz  <- m_af[, .(pct_with_overpass = round(100 * mean(overpass), 1),
                       pct_detected_given_overpass = round(100 * sum(detected) / max(1, sum(overpass)), 1)),
                   by = .(sensor, size)]
    bysize <- merge(bysize, ov_sz, by = c("sensor", "size"))
    fwrite(bysize[!is.na(size)], file.path(cfg$out_dir, "af_detection_by_size.csv"))
    
    bydur <- rate_table(m_af, c("sensor", "dur"), "detected")
    fwrite(bydur[!is.na(dur)], file.path(cfg$out_dir, "af_detection_by_duration.csv"))
    
    for (s in unique(m_af$sensor)) {
      ms <- m_af[sensor == s]
      add_summary(paste("Active fires", s), "All fires detected (%)", round(100 * mean(ms$detected), 1), sprintf("n = %d", nrow(ms)))
      add_summary(paste("Active fires", s), "Fires >= 100 ha detected (%)", round(100 * mean(ms[size %in% c("100-500", "500-1000", ">=1000"), detected]), 1), "")
    }
    message(">>> FIRMS: Cruzamento espacial e temporal efetuado com sucesso!")
  }
}

# =============================================================================
# 4. GRAVAR SUMÁRIO FINAL
# =============================================================================
smry <- rbindlist(summary_rows)
fwrite(smry, file.path(cfg$out_dir, "validation_summary.csv"))
print(smry)
message(">>> Validação externa concluída com sucesso!")