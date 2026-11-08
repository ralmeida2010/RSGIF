# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 08 - former 27-ID_BA_Fogos_Association.R
# =============================================================================
# 27-ID_BA_Fogos_Association.R  --  Reconciliation of PBA perimeters with
#                                   PRFD records (Sect. 2.2, Tables 5-6)
# =============================================================================
# SUMMARY (English)
#   1. Prepares the PBA polygons (BurnedAreasClosed): valid geometry, area,
#      interior point; polygons are processed from the largest to the smallest.
#   2. Adds two satellite time windows per polygon:
#        PAFA  min / max day of year of the Landsat monthly atlas inside the
#              polygon (Neves et al., 2023);
#        FRP   first / last SEVIRI hotspot within 4 km of the polygon.
#   3. Checks duplicated codes (SGIF and manual Cod_SGIF of the polygons).
#   4. Test on 1000 polygons with a manual Cod_SGIF (seed 123).
#   5. Production run, per year, for every polygon, with exclusive use of each
#      fire (a fire is linked to one polygon at most per year):
#        BLIND whole year, radii 2-14 km (steps of 3 km);
#        PAFA  if the BLIND score > 0.6: PAFA window +/- 5 days, radii 1-7 km
#              (steps of 2 km); a PAFA candidate replaces the BLIND one;
#        FRP   if the score is still > 0.8: FRP window, radii 0.5-5 km (steps of
#              1.5 km); an FRP candidate replaces the previous one.
#      There is no final rejection threshold: the best candidate found is kept,
#      whatever its score (scores > 0.6 are flagged as low-confidence later).
#   6. Accuracy against the polygons with a manual Cod_SGIF, by method and by
#      fire-size class (Tables 5 and 6).
#   7. Join to fogos: manual code when it exists, otherwise the estimated one;
#      columns BACodAssociacao, BACodEstimado, BAMetodo, BAScore,
#      BAMetodoManual and the perimeter geometry (geom); one polygon per fire.
#   8. Export output/fogos.gpkg.
#
# REVIEW NOTES (changes marked "# REVIEW:")
#   - The search function executar_busca_exclusiva() is NOT defined in this
#     script nor in any other script of the pipeline. Without it the linkage
#     cannot be reproduced. The script now stops with a clear message if the
#     function is missing; script 63 contains a reconstruction (Eq. 1). The
#     original function must be added to the public code (Code availability).
#   - fogos was reduced to its first 102 columns by POSITION (select(1:102)):
#     any change in the number or order of columns silently removed variables or
#     kept old ones. The previous linkage columns are now removed by NAME.
#   - A fire linked to several polygons kept only one of them (slice(1)), so the
#     perimeter area of fires mapped as several polygons was underestimated.
#     merge_multiple_polygons = TRUE joins them (default FALSE, as before).
#   - BACodAssociacao is always equal to Codigo after the join (it is the join
#     key); it is kept for compatibility, but it carries no information.
#   - Paths from project_dir.
# =============================================================================

# ==============================================================================
# SCRIPT 1: PREPARAÇÃO, GEOMETRIA E CARREGAMENTO DE DATAS (PAFA/FRP)
# ==============================================================================
# Guard against dplyr verbs masked by other packages (e.g. MASS::select, raster::select, plyr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename
mutate <- dplyr::mutate; summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count

library(sf)
library(dplyr)
library(terra)
library(lubridate)
library(purrr)      # REVIEW: map_dfr() is used in 2.2 (FRP), before purrr was loaded

# --- CONFIGURAÇÃO DE CAMINHOS ---
if (!exists("project_dir")) project_dir <- Sys.getenv("RSGIF_DIR", getwd())
if (!exists("raster_path")) raster_path <- file.path(project_dir, "raster")
merge_multiple_polygons <- FALSE   # REVIEW: TRUE joins all polygons linked to the same fire
if (!exists("data_dir")) data_dir <- file.path(project_dir, "data")

# REVIEW: inputs loaded from disk when they are not in memory
# PBA polygons (BurnedAreasClosed) and FRP15 are downloaded by 02_ingest_csv.R
first_existing <- function(...) { p <- c(...); p[file.exists(p)][1] }
if (!exists("BurnedAreasClosed")) {
  f <- first_existing(file.path(data_dir, "processed", "BurnedAreasClosed.gpkg"),
                      file.path(project_dir, "output", "BurnedAreasClosed.gpkg"))
  if (is.na(f)) stop("BurnedAreasClosed.gpkg not found in data/processed or output")
  BurnedAreasClosed <- st_read(f, quiet = TRUE); message("BurnedAreasClosed read from ", f)
}
if (!exists("FRP15")) {
  f <- first_existing(file.path(data_dir, "processed", "FRP15.csv"), file.path(data_dir, "FRP15.csv"))
  if (!is.na(f)) { FRP15 <- read.csv(f); message("FRP15 read from ", f) }
}
# Fires with coordinates, used as candidates of the linkage
if (!exists("fogos_data")) {
  fogos_data <- (if (inherits(fogos, "sf")) st_drop_geometry(fogos) else fogos)
  fogos_data <- fogos_data[!is.na(fogos_data$x_3763) & !is.na(fogos_data$y_3763), ]
  message("fogos_data: ", nrow(fogos_data), " fires with coordinates")
}


# 1. Carregar e Validar Geometrias
BA_sf <- BurnedAreasClosed %>%
  st_as_sf(geometry_column = "geom_bin") %>%
  st_set_crs(3763) %>%
  st_make_valid() %>%
  mutate(
    AreaHaPoly = as.numeric(st_area(.)) / 10000,
    cent = st_point_on_surface(st_geometry(.))
  ) %>%
  arrange(desc(AreaHaPoly))

# 2. Extração de Coordenadas e Inicialização de Colunas de Satélite
coords_ba <- st_coordinates(BA_sf$cent)
BA_sf$CX <- coords_ba[,1]
BA_sf$CY <- coords_ba[,2]
BA_sf$temp_id <- 1:nrow(BA_sf)

# Criar colunas vazias para receber os dados
BA_sf$d_pafa_min <- as.POSIXct(NA)
BA_sf$d_pafa_max <- as.POSIXct(NA)
BA_sf$d_frp_min  <- as.POSIXct(NA)
BA_sf$d_frp_max  <- as.POSIXct(NA)
# ------------------------------------------------------------------------------
# EXTRAÇÃO DE DATAS PAFA (RASTER DOY) PARA BA_sf
# ------------------------------------------------------------------------------

# 1. Verificar o conteúdo da diretoria
cat("Caminho configurado:", raster_path, "\n")
if (!startsWith(normalizePath(raster_path, winslash = "/", mustWork = FALSE),
                normalizePath(project_dir, winslash = "/", mustWork = FALSE)))
  warning("raster_path is outside the project (", project_dir, "): source 00_Config_paths.R first", call. = FALSE)

if(!dir.exists(raster_path)) {
  stop("ERRO: A diretoria não existe! Verifique o caminho.")
}

# REVIEW: the PAFA atlas can stay inside the ZIP (Portugal_Monthly-Fire-Atlas.zip). For each
# year ONLY that year's *_DOY.tif is extracted to raster_path/_extraidos, used, and deleted
# before the next year, so at most one raster is on disk. Loose *_DOY.tif files placed by
# the user in raster_path are used directly and never deleted. ZIPs per year inside the
# main ZIP are also handled (extracted, read, and deleted the same way).
pafa_zip     <- if (exists("pafa_zip")) pafa_zip else "Portugal_Monthly-Fire-Atlas.zip"
pafa_dir_ext <- file.path(raster_path, "_extraidos")
dir.create(pafa_dir_ext, recursive = TRUE, showWarnings = FALSE)
is_doy  <- function(x) grepl("_DOY\\.tif$", x, ignore.case = TRUE) & !grepl("before|mean", basename(x), ignore.case = TRUE)
year_of <- function(x) suppressWarnings(as.integer(sub("^([0-9]{4}).*", "\\1", basename(x))))

# index: year -> loose file, or entry of the ZIP (or of a ZIP inside the ZIP)
loose <- list.files(raster_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
loose <- loose[is_doy(loose) & !grepl("/_extraidos/", gsub("\\\\", "/", loose))]
pafa_index <- data.frame(ano = year_of(loose), tipo = rep("tif", length(loose)),
                         zip = rep(NA_character_, length(loose)), inner_zip = rep(NA_character_, length(loose)),
                         entry = loose, stringsAsFactors = FALSE)
zpath <- file.path(raster_path, pafa_zip)
if (file.exists(zpath)) {
  zl <- utils::unzip(zpath, list = TRUE)$Name
  d  <- zl[is_doy(zl)]
  if (length(d)) pafa_index <- rbind(pafa_index, data.frame(ano = year_of(d), tipo = "zip", zip = zpath,
                                                            inner_zip = NA_character_, entry = d, stringsAsFactors = FALSE))
  iz <- zl[grepl("\\.zip$", zl, ignore.case = TRUE)]
  iz_year <- suppressWarnings(as.integer(regmatches(basename(iz), regexpr("(19|20)[0-9]{2}", basename(iz)))))
  if (length(iz) && length(iz_year) == length(iz))
    pafa_index <- rbind(pafa_index, data.frame(ano = iz_year, tipo = "inner", zip = zpath,
                                               inner_zip = iz, entry = NA_character_, stringsAsFactors = FALSE))
}
pafa_index <- pafa_index[!is.na(pafa_index$ano), ]
pafa_index <- pafa_index[order(pafa_index$ano, match(pafa_index$tipo, c("tif", "zip", "inner"))), ]
pafa_index <- pafa_index[!duplicated(pafa_index$ano), ]
cat("PAFA rasters available for", nrow(pafa_index), "years (",
    paste(range(pafa_index$ano), collapse = "-"), ") | loose:", sum(pafa_index$tipo == "tif"),
    "| inside the ZIP:", sum(pafa_index$tipo != "tif"), "\n")

# raster of one year: returns list(path, temp_files); extracts only that year
pafa_raster_year <- function(a) {
  k <- which(pafa_index$ano == a)
  if (!length(k)) return(NULL)
  row <- pafa_index[k[1], ]
  if (row$tipo == "tif") return(list(path = row$entry, temp = character(0)))
  if (row$tipo == "zip") {
    utils::unzip(row$zip, files = row$entry, exdir = pafa_dir_ext, junkpaths = TRUE)
    f <- file.path(pafa_dir_ext, basename(row$entry))
    return(if (file.exists(f)) list(path = f, temp = f) else NULL)
  }
  # ZIP of the year inside the main ZIP
  utils::unzip(row$zip, files = row$inner_zip, exdir = pafa_dir_ext, junkpaths = TRUE)
  zi <- file.path(pafa_dir_ext, basename(row$inner_zip))
  e  <- utils::unzip(zi, list = TRUE)$Name; e <- e[is_doy(e)]
  if (!length(e)) { unlink(zi); return(NULL) }
  utils::unzip(zi, files = e[1], exdir = pafa_dir_ext, junkpaths = TRUE)
  f <- file.path(pafa_dir_ext, basename(e[1]))
  list(path = f, temp = c(f, zi))
}
free_pafa <- function(tmp) {                      # delete the temporary files of one year
  if (!length(tmp)) return(invisible())
  invisible(gc())
  for (f in tmp) for (k in 1:5) { if (!file.exists(f) || isTRUE(suppressWarnings(file.remove(f)))) break; Sys.sleep(1) }
  terra::tmpFiles(remove = TRUE)
}
ficheiros_na_pasta <- paste0(pafa_index$ano, " (", pafa_index$tipo, ")")
cat("Total de ficheiros .tif encontrados:", length(ficheiros_na_pasta), "\n")

if(length(ficheiros_na_pasta) > 0) {
  cat("Exemplos de ficheiros encontrados:\n")
  print(head(ficheiros_na_pasta, 5))
} else {
  cat("⚠️ Nenhum ficheiro .tif foi encontrado nesta pasta.\n")
}




# 1. Preparar colunas
BA_sf$d_pafa_min <- as.POSIXct(NA)
BA_sf$d_pafa_max <- as.POSIXct(NA)

cat(">>> Iniciando Processamento PAFA (Rasters Landsat L5)...\n")
cat(rep("-", 60), "\n")

anos_unicos <- sort(unique(BA_sf$Ano))

for(a in anos_unicos) {
  # Procura o ficheiro exato (ex: 1984_fire-allPT_L5_DOY.tif)
  # REVIEW: only this year's raster is extracted (and deleted at the end of the year)
  pr <- pafa_raster_year(a)
  file_raster <- if (is.null(pr)) character(0) else pr$path
  
  idx_ano <- which(BA_sf$Ano == a)
  n_pols <- length(idx_ano)
  
  if(length(file_raster) > 0) {
    cat(paste0("📅 Ano: ", a, " | Ficheiro: ", basename(file_raster[1]), "\n"))
    
    r <- rast(file_raster[1])
    pol_v <- vect(BA_sf[idx_ano,])
    
    # Sincronização de CRS
    if(crs(r) != crs(pol_v)) {
      pol_v <- project(pol_v, crs(r))
    }
    
    # Extração zonal com filtragem interna
    res <- terra::extract(r, pol_v, fun = function(x) {
      val <- x[x > 0 & x <= 366 & !is.na(x)]
      if(length(val) == 0) return(c(min = NA, max = NA))
      return(c(min = min(val), max = max(val)))
    })
    
    # Conversão para data real
    data_base <- as.POSIXct(paste0(a, "-01-01"), tz = "UTC")
    
    d_min_raw <- round(res[,2])
    d_max_raw <- round(res[,3])
    
    BA_sf$d_pafa_min[idx_ano] <- data_base + days(ifelse(is.na(d_min_raw), 0, d_min_raw - 1))
    BA_sf$d_pafa_max[idx_ano] <- data_base + days(ifelse(is.na(d_max_raw), 0, d_max_raw - 1))
    
    # Limpar NAs (o ifelse acima põe 1 de Jan se for NA, aqui corrigimos para NA real)
    BA_sf$d_pafa_min[idx_ano][is.na(d_min_raw)] <- NA
    BA_sf$d_pafa_max[idx_ano][is.na(d_max_raw)] <- NA
    
    # Log de Sucesso do Ano
    sucesso_ano <- sum(!is.na(BA_sf$d_pafa_min[idx_ano]))
    cat(paste0("   └─ Polígonos: ", n_pols, " | Extraídos com sucesso: ", sucesso_ano, "\n"))
    rm(r, pol_v, res); free_pafa(pr$temp)          # REVIEW: free memory and disk
    if (length(pr$temp)) cat("      (temporary raster deleted)\n")
    
  } else {
    if(a >= 1984) { # Landsat 5 só existe a partir de 1984
      cat(paste0("⚠️  Ano: ", a, " | Raster não encontrado na pasta.\n"))
    }
  }
}

cat(rep("-", 60), "\n")
cat(paste0("✅ Concluído! Total de registos com PAFA: ", sum(!is.na(BA_sf$d_pafa_min)), " / ", nrow(BA_sf), "\n"))
# ------------------------------------------------------------------------------
# 2.2 EXTRAÇÃO DE DATAS FRP (HOTSPOTS) - OTIMIZAÇÃO POR BOUNDING BOX
# ------------------------------------------------------------------------------
if(exists("FRP15")) {
  cat(">>> Cruzando com pontos quentes (FRP) via Bounding Box... \n")
  
  # 1. Preparar os pontos FRP e extrair coordenadas puras para filtro rápido
  frp_sf_total <- FRP15 %>% 
    mutate(dh = as.POSIXct(paste(ano, mes, dia, hora, minuto, sep="-"), 
                           format="%Y-%m-%d-%H-%M")) %>%
    filter(!is.na(dh)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(3763)
  
  # Adicionar colunas de coordenadas X e Y explícitas para filtro rápido
  coords_frp <- st_coordinates(frp_sf_total)
  frp_sf_total$X_pts <- coords_frp[,1]
  frp_sf_total$Y_pts <- coords_frp[,2]
  
  # 2. Splits Anuais
  ba_list  <- split(BA_sf, BA_sf$Ano)
  frp_list <- split(frp_sf_total, frp_sf_total$ano)
  
  BA_sf <- map_dfr(names(ba_list), function(ano_str) {
    df_ba_ano  <- ba_list[[ano_str]]
    df_frp_ano <- frp_list[[ano_str]]
    
    if (is.null(df_frp_ano) || nrow(df_frp_ano) == 0) return(df_ba_ano)
    
    cat("\n>>> Ano:", ano_str, "| BA:", nrow(df_ba_ano))
    
    flush.console() # Garante que o R imprime imediatamente
    
    pb <- txtProgressBar(min = 0, max = nrow(df_ba_ano), style = 3)
    
    for(i in 1:nrow(df_ba_ano)) {
      
      
      
      setTxtProgressBar(pb, i)
      
      # --- FILTRO POR COORDENADAS (SELECT RÁPIDO) ---
      # Obter a bounding box do polígono atual
      bbox <- st_bbox(df_ba_ano[i, ])
      dist_buffer <- 4000
      
      # Filtrar pontos que estão dentro do retângulo (bbox + buffer)
      vizinhos_candidatos <- df_frp_ano %>%
        filter(X_pts >= (bbox["xmin"] - dist_buffer),
               X_pts <= (bbox["xmax"] + dist_buffer),
               Y_pts >= (bbox["ymin"] - dist_buffer),
               Y_pts <= (bbox["ymax"] + dist_buffer))
      
      if(nrow(vizinhos_candidatos) > 0) {
        # Só agora fazemos o cruzamento geográfico real, mas apenas com os candidatos do retângulo
        indices <- as.vector(st_is_within_distance(st_geometry(df_ba_ano)[i], 
                                                   vizinhos_candidatos, 
                                                   dist = dist_buffer, 
                                                   sparse = FALSE))
        
        vizinhos_reais <- vizinhos_candidatos[indices, ]
        
        if(nrow(vizinhos_reais) > 0) {
          df_ba_ano$d_frp_min[i] <- min(vizinhos_reais$dh, na.rm = TRUE)
          df_ba_ano$d_frp_max[i] <- max(vizinhos_reais$dh, na.rm = TRUE)
        }
      }
    }
    close(pb)
    return(df_ba_ano)
  })
  cat("\n ✅ Cruzamento FRP por Bounding Box concluído!\n")
  cat("   Polígonos com janela FRP:", sum(!is.na(BA_sf$d_frp_min)), "/", nrow(BA_sf), "\n")   # REVIEW
}
#------------------------------------------------------
# ==============================================================================
# VERIFICAÇÃO DE DUPLICADOS E INTEGRIDADE
# ==============================================================================

# Verificar se existem códigos SGIF duplicados na base de referência
lixo <- fogos %>%
  group_by(Ano, Codigo) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

cat(">>> Códigos duplicados na base SGIF:", nrow(lixo), "\n")



# Identificar polígonos de BA que receberam o mesmo código do algoritmo
lixo <- BA_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(Cod_SGIF)) %>%
  group_by(Ano, Cod_SGIF) %>%
  summarise(
    Quantidade_Poligonos = n(),
    IDs_BA = paste(temp_id, collapse = ", "),
    Soma_Area_HA = sum(AreaHaPoly),
    .groups = "drop"
  ) %>%
  filter(Quantidade_Poligonos > 1) %>%
  arrange(desc(Soma_Area_HA))

# Visualizar os casos mais graves (maior área com código duplicado)
print(head(lixo))

# ==============================================================================
# ENSAIO DE PERFORMANCE OTIMIZADO: HIERARQUIA + BOUNDING BOX (PREPARADO)
# ==============================================================================


colnames(fogos_data)

library(dplyr)
library(purrr)
library(lubridate)
library(sf)

library(sf)
library(dplyr)

# 1. Converter para sf usando as colunas corretas (x_3763 e y_3763)
if (!inherits(fogos_data, "sf")) {
  cat(">>> Convertendo fogos_data para objeto espacial (SF) usando colunas 3763...\n")
  fogos_data <- st_as_sf(fogos_data, 
                         coords = c("x_3763", "y_3763"), 
                         crs = 3763, 
                         remove = FALSE)
}

# 2. Criar colunas numéricas para o filtro de Bounding Box rápido (X_cent, Y_cent)
if(!"X_cent" %in% colnames(fogos_data)) {
  coords_f <- st_coordinates(st_geometry(fogos_data))
  fogos_data$X_cent <- coords_f[,1]
  fogos_data$Y_cent <- coords_f[,2]
}

# Adicionamos colunas de coordenadas numéricas para filtro BBox ultra-rápido
if(!"X_cent" %in% colnames(fogos_data)) {
  coords_fogos <- st_coordinates(st_geometry(fogos_data))
  fogos_data$X_cent <- coords_fogos[,1]
  fogos_data$Y_cent <- coords_fogos[,2]
}

# REVIEW: the search function is required (it is not defined in this script)
# REVIEW: if the function is not in the session, the reconstruction (08a) is loaded
if (!exists("executar_busca_exclusiva", mode = "function")) {
  if (!exists("scripts_dir")) scripts_dir <- file.path(project_dir, "R")
  f08a <- file.path(scripts_dir, "08a_pba_search_function.R")
  if (!file.exists(f08a)) stop("executar_busca_exclusiva() not defined and ", f08a, " not found.", call. = FALSE)
  source(f08a)
}

# 2. PREPARAÇÃO DA AMOSTRA
set.seed(123)
ensaio_sample <- BA_sf %>%
  filter(!is.na(Cod_SGIF) & !Cod_SGIF %in% c("0", "", " ")) %>%
  slice_sample(n = 1000)

# Dividir por ano para otimizar a memória
BA_split_test <- split(ensaio_sample, ensaio_sample$Ano)
fogos_split   <- split(fogos_data, fogos_data$Ano)

# 3. EXECUÇÃO COM HIERARQUIA E FILTRO DE CAIXA (BBOX)
ensaio_processado <- map_dfr(names(BA_split_test), function(ano_str) {
  
  cat("\n>>> Testando ano:", ano_str, "| Amostra:", nrow(BA_split_test[[ano_str]]), "registos")
  
  df_ano <- BA_split_test[[ano_str]]
  cand_ano_total <- fogos_split[[ano_str]] 
  codigos_usados_ano <- c()
  
  # Inicializar colunas de resultados
  df_ano$CODIGOSGIF_ESTIMADO <- NA_character_
  df_ano$METODO_ESTIMATIVA    <- NA_character_
  df_ano$SCORE_ESTIMADO       <- NA_real_
  
  if(is.null(cand_ano_total) || nrow(cand_ano_total) == 0) return(df_ano)
  
  pb <- txtProgressBar(min = 0, max = nrow(df_ano), style = 3)
  
  for(i in 1:nrow(df_ano)) {
    setTxtProgressBar(pb, i)
    row_curr <- df_ano[i, ]
    
    # --- OTIMIZAÇÃO: PRÉ-FILTRO POR BOUNDING BOX ---
    # Margem de 15km (limite do método BLIND)
    margem <- 15000 
    bbox <- st_bbox(row_curr)
    
    # Filtro numérico (milissegundos)
    cand_ano <- cand_ano_total %>%
      filter(X_cent >= (bbox["xmin"] - margem),
             X_cent <= (bbox["xmax"] + margem),
             Y_cent >= (bbox["ymin"] - margem),
             Y_cent <= (bbox["ymax"] + margem))
    
    if(nrow(cand_ano) == 0) next
    
    res <- NULL
    
    # --- PASSO 1: BLIND ---
    res <- executar_busca_exclusiva(row_curr, cand_ano, 
                                    as.POSIXct(paste0(ano_str,"-01-01")), 
                                    as.POSIXct(paste0(ano_str,"-12-31")), 
                                    seq(2000, 15000, 3000), "BLIND", codigos_usados_ano)
    
    # --- PASSO 2: PAFA (Só se tiver data PAFA válida e score baixo) ---
    if((is.null(res) || res$sc > 0.6) && !is.na(row_curr$d_pafa_min)) {
      res_pafa <- executar_busca_exclusiva(row_curr, cand_ano, 
                                           row_curr$d_pafa_min, row_curr$d_pafa_max, 
                                           seq(1000, 8000, 2000), "PAFA", codigos_usados_ano)
      if(!is.null(res_pafa)) res <- res_pafa
    }
    
    # --- PASSO 3: FRP (Só se tiver data FRP válida e score baixo) ---
    if((is.null(res) || res$sc > 0.8) && !is.na(row_curr$d_frp_min)) {
      res_frp <- executar_busca_exclusiva(row_curr, cand_ano, 
                                          row_curr$d_frp_min, row_curr$d_frp_max, 
                                          seq(500, 5000, 1500), "FRP", codigos_usados_ano)
      if(!is.null(res_frp)) res <- res_frp
    }
    
    # Gravação do resultado
    if(!is.null(res)) {
      df_ano$CODIGOSGIF_ESTIMADO[i] <- res$cod
      df_ano$METODO_ESTIMATIVA[i]   <- res$met
      df_ano$SCORE_ESTIMADO[i]      <- res$sc
      codigos_usados_ano <- c(codigos_usados_ano, res$cod)
    }
  }
  close(pb)
  return(df_ano)
})



# 3. ANÁLISE DETALHADA POR MÉTODO
analise_metodos <- ensaio_processado %>% st_drop_geometry() %>%   # REVIEW: no geometry in the summary
  mutate(sucesso = ifelse(Cod_SGIF == CODIGOSGIF_ESTIMADO, 1, 0)) %>%
  group_by(METODO_ESTIMATIVA) %>%
  summarise(
    Total = n(),
    Acertos = sum(sucesso, na.rm = TRUE),
    Acuracia = round(mean(sucesso, na.rm = TRUE) * 100, 2),
    Score_Medio = mean(SCORE_ESTIMADO, na.rm = TRUE)
  )

print(analise_metodos)

# ==============================================================================
# PROCESSAMENTO INTEGRAL: BA_sf + HIERARQUIA + BOUNDING BOX
# ==============================================================================

library(dplyr)
library(purrr)
library(lubridate)
library(sf)

# 1. PREPARAÇÃO DA BASE DE REFERÊNCIA (SGIF)
if (!inherits(fogos_data, "sf")) {
  cat(">>> Convertendo fogos_data para objeto espacial (SF)...\n")
  fogos_data <- st_as_sf(fogos_data, 
                         coords = c("x_3763", "y_3763"), 
                         crs = 3763, 
                         remove = FALSE)
}

# Criar colunas numéricas para filtro BBox ultra-rápido (se não existirem)
if(!"X_cent" %in% colnames(fogos_data)) {
  cat(">>> Criando índices espaciais numéricos...\n")
  coords_f <- st_coordinates(st_geometry(fogos_data))
  fogos_data$X_cent <- coords_f[,1]
  fogos_data$Y_cent <- coords_f[,2]
}

# 2. PREPARAÇÃO DA BASE DE ÁREAS ARDIDAS (BA_sf)
# Usamos toda a base BA_sf que tenha Cod_SGIF para validação ou toda a base se o objetivo for produção
cat(">>> Preparando base completa para processamento...\n")
BA_processamento <- BA_sf 

# Dividir por ano para otimizar a gestão de memória e velocidade
BA_split    <- split(BA_processamento, BA_processamento$Ano)
fogos_split <- split(fogos_data, fogos_data$Ano)

# 3. EXECUÇÃO DO MOTOR DE BUSCA (LOOP ANUAL)
resultados_totais <- map_dfr(names(BA_split), function(ano_str) {
  
  cat("\n", rep("=", 40), "\n")
  cat(">>> PROCESSANDO ANO:", ano_str, "| TOTAL:", nrow(BA_split[[ano_str]]), "polígonos\n")
  
  df_ano <- BA_split[[ano_str]]
  cand_ano_total <- fogos_split[[ano_str]] 
  codigos_usados_ano <- c()
  
  # Inicializar colunas de resultados
  df_ano$CODIGOSGIF_ESTIMADO <- NA_character_
  df_ano$METODO_ESTIMATIVA    <- NA_character_
  df_ano$SCORE_ESTIMADO       <- NA_real_
  
  if(is.null(cand_ano_total) || nrow(cand_ano_total) == 0) {
    cat("    ⚠️ Sem fogos registados no SGIF para este ano.\n")
    return(df_ano)
  }
  
  pb <- txtProgressBar(min = 0, max = nrow(df_ano), style = 3)
  
  for(i in 1:nrow(df_ano)) {
    setTxtProgressBar(pb, i)
    row_curr <- df_ano[i, ]
    
    # --- FILTRO BOUNDING BOX (Otimização Espacial) ---
    margem <- 15000 # 15km
    bbox <- st_bbox(row_curr)
    
    cand_vizinhos <- cand_ano_total %>%
      filter(X_cent >= (bbox["xmin"] - margem),
             X_cent <= (bbox["xmax"] + margem),
             Y_cent >= (bbox["ymin"] - margem),
             Y_cent <= (bbox["ymax"] + margem))
    
    if(nrow(cand_vizinhos) == 0) next
    
    res_match <- NULL
    
    # --- HIERARQUIA DE BUSCA ---
    
    # 1. BLIND (Anual)
    res_match <- executar_busca_exclusiva(row_curr, cand_vizinhos, 
                                          as.POSIXct(paste0(ano_str,"-01-01")), 
                                          as.POSIXct(paste0(ano_str,"-12-31")), 
                                          seq(2000, 15000, 3000), "BLIND", codigos_usados_ano)
    
    # 2. PAFA (Se Score > 0.6 e houver data satélite)
    if((is.null(res_match) || res_match$sc > 0.6) && !is.na(row_curr$d_pafa_min)) {
      res_pafa <- executar_busca_exclusiva(row_curr, cand_vizinhos, 
                                           row_curr$d_pafa_min - days(5), 
                                           row_curr$d_pafa_max + days(5), 
                                           seq(1000, 8000, 2000), "PAFA", codigos_usados_ano)
      if(!is.null(res_pafa)) res_match <- res_pafa
    }
    
    # 3. FRP (Se ainda sem match satisfatório)
    if((is.null(res_match) || res_match$sc > 0.8) && !is.na(row_curr$d_frp_min)) {
      res_frp <- executar_busca_exclusiva(row_curr, cand_vizinhos, 
                                          row_curr$d_frp_min, row_curr$d_frp_max, 
                                          seq(500, 5000, 1500), "FRP", codigos_usados_ano)
      if(!is.null(res_frp)) res_match <- res_frp
    }
    
    # Atribuição de resultados
    if(!is.null(res_match)) {
      df_ano$CODIGOSGIF_ESTIMADO[i] <- res_match$cod
      df_ano$METODO_ESTIMATIVA[i]   <- res_match$met
      df_ano$SCORE_ESTIMADO[i]      <- res_match$sc
      codigos_usados_ano <- c(codigos_usados_ano, res_match$cod)
    }
  }
  close(pb)
  return(df_ano)
})

# 4. ANÁLISE DE RESULTADOS FINAIS
analise_final <- resultados_totais %>% st_drop_geometry() %>%   # REVIEW: no geometry in the summary
  mutate(sucesso = ifelse(!is.na(Cod_SGIF) & Cod_SGIF == CODIGOSGIF_ESTIMADO, 1, 0)) %>%
  group_by(METODO_ESTIMATIVA) %>%
  summarise(
    Total_Processado = n(),
    Acertos = sum(sucesso, na.rm = TRUE),
    Acuracia = round(mean(sucesso, na.rm = TRUE) * 100, 2),
    Score_Medio = round(mean(SCORE_ESTIMADO, na.rm = TRUE), 4)
  )

cat("\n\n>>> RELATÓRIO FINAL DE PERFORMANCE <<<\n")
print(analise_final)


# ==============================================================================
# 4. ANÁLISE DE RESULTADOS FINAIS (VALIDAÇÃO APENAS COM COD_SGIF)
# ==============================================================================

analise_final <- resultados_totais %>% st_drop_geometry() %>%   # REVIEW: no geometry in the summary
  # Filtramos para manter apenas polígonos que já tinham Cod_SGIF original
  # Removemos zeros, brancos ou NAs
  filter(!is.na(Cod_SGIF) & !Cod_SGIF %in% c("0", "", " ")) %>%
  
  mutate(
    # Comparação direta entre o original e o estimado pelo algoritmo
    sucesso = ifelse(as.character(Cod_SGIF) == as.character(CODIGOSGIF_ESTIMADO), 1, 0)
  ) %>%
  
  group_by(METODO_ESTIMATIVA) %>%
  
  summarise(
    Total_Validado = n(),
    Acertos        = sum(sucesso, na.rm = TRUE),
    Falhas         = sum(sucesso == 0, na.rm = TRUE),
    Acuracia_pct   = round(mean(sucesso, na.rm = TRUE) * 100, 2),
    Score_Medio    = round(mean(SCORE_ESTIMADO, na.rm = TRUE), 4)
  ) %>%
  # Ordenar para ver qual método foi mais eficaz
  arrange(desc(Acuracia_pct))

cat("\n\n>>> RELATÓRIO DE VALIDAÇÃO (Apenas registos com Cod_SGIF original) <<<\n")
cat(rep("-", 70), "\n")
print(analise_final)
cat(rep("-", 70), "\n")

# Cálculo da Acurácia Global do Experimento
total_geral <- sum(analise_final$Total_Validado)
acertos_geral <- sum(analise_final$Acertos)
cat(paste0("🚀 ACURÁCIA GLOBAL: ", round((acertos_geral / total_geral) * 100, 2), "%\n"))

# ==============================================================================
# REVIEW: BA_Final (all polygons with their link) and CSV outputs
# ==============================================================================
# BA_Final was used below but never created: it is the result of the production run.
BA_Final <- resultados_totais
# keep the polygons (active geometry) as "geom"; drop the interior point (cent), a second
# geometry column that would otherwise be written to the GeoPackage instead of the polygons
geo_pol  <- st_geometry(BA_Final)
BA_Final <- st_drop_geometry(BA_Final)
BA_Final <- BA_Final[, !vapply(BA_Final, function(v) inherits(v, "sfc"), logical(1)), drop = FALSE]
BA_Final <- st_sf(BA_Final, geom = geo_pol)

if (!exists("output_dir")) output_dir <- file.path(project_dir, "output")
pba_dir <- file.path(output_dir, "pba_linkage"); dir.create(pba_dir, recursive = TRUE, showWarnings = FALSE)
csv <- function(x, f) { write.csv(x, file.path(pba_dir, f), row.names = FALSE, na = ""); cat("   saved:", f, "\n") }
manual <- function(x) !is.na(x$Cod_SGIF) & !x$Cod_SGIF %in% c("0", "", " ")
tab_pol <- function(x) {
  x <- st_drop_geometry(x)
  data.frame(temp_id = x$temp_id, Ano = x$Ano, AreaHaPoly = round(x$AreaHaPoly, 4),
             Cod_SGIF = x$Cod_SGIF, CODIGOSGIF_ESTIMADO = x$CODIGOSGIF_ESTIMADO,
             METODO_ESTIMATIVA = x$METODO_ESTIMATIVA, SCORE_ESTIMADO = round(x$SCORE_ESTIMADO, 4),
             correto = ifelse(manual(x), as.character(x$Cod_SGIF) == as.character(x$CODIGOSGIF_ESTIMADO), NA),
             d_pafa_min = x$d_pafa_min, d_pafa_max = x$d_pafa_max,
             d_frp_min = x$d_frp_min, d_frp_max = x$d_frp_max, CX = x$CX, CY = x$CY)
}
cat(">>> Saving the linkage results in", pba_dir, "\n")
csv(tab_pol(ensaio_processado), "ensaio_1000_poligonos.csv")
csv(analise_metodos, "ensaio_1000_por_metodo.csv")
pol <- tab_pol(BA_Final)
csv(pol, "associacao_todos_poligonos.csv")                    # one row per PBA polygon
# The validation tables (Tables 5-7, accuracy by year) are written by
# 09_PBA_linkage_validation.R from BA_Final, so they are not repeated here.
val <- pol[!is.na(pol$correto), ]
resumo <- data.frame(
  item  = c("PBA polygons", "polygons linked", "polygons not linked", "polygons with manual Cod_SGIF",
            "global accuracy (%)", "polygons with PAFA window", "polygons with FRP window",
            "links with score > 0.6 (low confidence)"),
  value = c(nrow(pol), sum(!is.na(pol$CODIGOSGIF_ESTIMADO)), sum(is.na(pol$CODIGOSGIF_ESTIMADO)), nrow(val),
            round(100 * mean(val$correto), 2), sum(!is.na(pol$d_pafa_min)), sum(!is.na(pol$d_frp_min)),
            sum(pol$SCORE_ESTIMADO > 0.6, na.rm = TRUE)))
csv(resumo, "resumo_associacao.csv")
print(resumo)
# REVIEW: list columns (e.g. GDB_GEOMATTR_DATA of the SQL Server geometry) cannot be written to a
# GeoPackage: they are left out of the file (BA_Final in memory and the .rds keep them)
ba_attr <- st_drop_geometry(BA_Final)
lc <- names(ba_attr)[vapply(ba_attr, is.list, logical(1))]; rm(ba_attr)
if (length(lc)) cat("   columns not written to the GeoPackage (list columns):", paste(lc, collapse = ", "), "\n")
st_write(BA_Final[, setdiff(names(BA_Final), lc)], file.path(pba_dir, "BA_Final.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)   # polygons with the link
saveRDS(BA_Final, file.path(output_dir, "checkpoints", "BA_Final.rds"))
cat("   saved: BA_Final.gpkg and checkpoints/BA_Final.rds\n")






# ==============================================================================
# 1. LIMPEZA DINÂMICA (COLUNAS 103 EM DIANTE)
# ==============================================================================
cat(">>> Removendo geometria original e preparando colunas...\n")
# REVIEW: previous linkage columns removed by name (not the first 102 by position)
fogos_tabular <- fogos %>%
  { if (inherits(., "sf")) st_drop_geometry(.) else . } %>%
  dplyr::select(-any_of(c("BACodAssociacao", "BACodEstimado", "BAMetodo", "BAScore",
                          "BAMetodoManual", "geom", "geom_bin", "CODIGOSGIF_ESTIMADO",
                          "METODO_ESTIMATIVA", "SCORE_ESTIMADO", "Cod_SGIF")))

# ==============================================================================
# 2. PREPARAÇÃO DA BA_FINAL
# ==============================================================================
BA_Join <- BA_Final %>%
  mutate(BEST_Codigo = ifelse(!is.na(Cod_SGIF) & !Cod_SGIF %in% c("0", "", " "), 
                              Cod_SGIF, 
                              CODIGOSGIF_ESTIMADO)) %>%
  # Selecionamos os dados originais + Cod_SGIF para criar a coluna Manual depois
  dplyr::select(BEST_Codigo, CODIGOSGIF_ESTIMADO, METODO_ESTIMATIVA, 
                SCORE_ESTIMADO, Cod_SGIF, geom)

# ==============================================================================
# 3. JUNÇÃO E RENOMEAÇÃO FINAL
# ==============================================================================
cat(">>> Realizando a junção e criando novas colunas, incluindo BAMetodoManual...\n")

fogos2 <- fogos_tabular %>%
  left_join(BA_Join, by = c("Codigo" = "BEST_Codigo")) %>%
  mutate(
    BACodAssociacao = Codigo,
    BACodEstimado   = CODIGOSGIF_ESTIMADO,
    BAMetodo        = METODO_ESTIMATIVA,
    BAScore         = SCORE_ESTIMADO,
    
    # Nova coluna baseada na existência de Cod_SGIF válido
    BAMetodoManual = ifelse(!is.na(Cod_SGIF) & !Cod_SGIF %in% c("0", "", " "), 
                            "Manual Introduction", 
                            NA_character_)
  ) %>%
  # Removemos as colunas temporárias vindas da BA (incluindo o Cod_SGIF usado para o teste)
  dplyr::select(-CODIGOSGIF_ESTIMADO, -METODO_ESTIMATIVA, -SCORE_ESTIMADO, -Cod_SGIF) %>%
  st_as_sf()

# ==============================================================================
# LIMPEZA DE DUPLICADOS NA BASE FOGOS
# ==============================================================================
cat(">>> Iniciando limpeza de duplicados no objeto 'fogos'...\n")

fogos <- fogos2 %>%
  group_by(Ano, Codigo) %>%
  arrange(
    # Regra 1: Prioridade para match entre Codigo e Estimado
    desc(Codigo == BACodEstimado), 
    
    # Regra 2: Prioridade ao menor Score numérico
    as.numeric(BAScore) 
  ) %>%
  slice(if (merge_multiple_polygons) seq_len(n()) else 1L) %>%
  ungroup()

# REVIEW: optionally merge the polygons of fires linked to more than one polygon
if (merge_multiple_polygons) {
  n_multi <- fogos %>% st_drop_geometry() %>% count(Ano, Codigo) %>% filter(n > 1) %>% nrow()
  geo <- fogos %>% group_by(Ano, Codigo) %>% summarise(geom = st_union(geom), .groups = "drop")
  fogos <- fogos %>% st_drop_geometry() %>% group_by(Ano, Codigo) %>% slice(1) %>% ungroup() %>%
    left_join(geo, by = c("Ano", "Codigo")) %>% st_as_sf()
  cat(">>> Fires with several polygons merged:", n_multi, "\n")
}

# Verificação das colunas finais
# colnames(fogos) deverá agora incluir de 104 a 108: 
# BACodAssociacao, BACodEstimado, BAMetodo, BAScore, BAMetodoManual
# ==============================================================================
# VERIFICAÇÃO FINAL
# ==============================================================================
duplicados_pos <- fogos %>%
  st_drop_geometry() %>% # st_drop_geometry agiliza a contagem em objetos sf
  group_by(Ano, Codigo) %>%
  tally() %>%
  filter(n > 1)

cat(">>> Limpeza concluída. Registos totais finais:", nrow(fogos), "\n")
cat(">>> Duplicados restantes:", nrow(duplicados_pos), "\n")


# ==============================================================================
# ANÁLISE DETALHADA EM 3 PARTES (COM DISTINÇÃO DE MÉTODO MANUAL)
# ==============================================================================

cat("\n>>> INICIANDO ANÁLISE DETALHADA EM 3 PARTES (COM FILTRO MANUAL) <<<\n")

# ==============================================================================
# ANÁLISE DETALHADA EM 3 PARTES (COM FILTRO MANUAL E ESTIMATED)
# ==============================================================================

cat("\n>>> INICIANDO ANÁLISE DETALHADA COM CATEGORIAS: Manual vs Estimated <<<\n")

# Preparação da base
base_analise <- fogos %>%
  st_drop_geometry() %>%
  mutate(
    # 1. Garantir que o Score é numérico
    BAScore = as.numeric(BAScore),
    
    # 2. Atualizar BAMetodoManual: Se é NA mas tem estimativa, passa a "Estimated"
    BAMetodoManual = case_when(
      !is.na(BAMetodoManual) ~ BAMetodoManual, # Mantém "Manual Introduction"
      is.na(BAMetodoManual) & !is.na(BACodEstimado) ~ "Estimated",
      TRUE ~ "No Association"
    ),
    
    # 3. Identificação dos grupos para análise
    is_match_direto    = (Codigo == BACodAssociacao),
    is_acerto_estimado = (Codigo == BACodEstimado),
    is_divergente      = (BACodAssociacao != BACodEstimado)
  )

# ==============================================================================
# EXEMPLO DE COMO FICA A PARTE 2 (Agora não deve vir vazia)
# ==============================================================================
cat("\n--- PARTE 2: Acurácia nos Registos 'Estimated' ---\n")

parte2 <- base_analise %>%
  # Filtramos onde a associação foi por estimativa (não manual)
  filter(BAMetodoManual == "Estimated") %>%
  group_by(ClasseArea, BAMetodo) %>%
  summarise(
    Total_Restante = n(),
    Acertos_Estimados = sum(is_acerto_estimado, na.rm = TRUE),
    Acuracia_Pct = round(mean(is_acerto_estimado, na.rm = TRUE) * 100, 2),
    Score_Medio = round(mean(BAScore, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(parte2)

# ==============================================================================
# PARTE 1: MATCH DIRETO (Codigo == BACodAssociacao)
# ==============================================================================
cat("\n--- PARTE 1: Matches Diretos por Fonte e Tipo de Introdução ---\n")

parte1 <- base_analise %>%
  filter(is_match_direto) %>%
  # Adicionada a coluna BAMetodoManual ao agrupamento
  group_by( BAMetodo, BAMetodoManual) %>%
  summarise(
    Total = n(),
    
    Acertos_Estimados = sum(is_acerto_estimado, na.rm = TRUE),
    Acuracia_Pct = round(mean(is_acerto_estimado, na.rm = TRUE) * 100, 2),
    Score_Medio = round(mean(BAScore, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(parte1)

# ==============================================================================
# PARTE 2: ACURÁCIA NOS RESTANTES (Exclui Matches Diretos)
# ==============================================================================
cat("\n--- PARTE 2: Acurácia nos Registos Restantes (Match por Estimativa) ---\n")

parte2 <-  base_analise %>%
  filter(is_match_direto) %>%
  # Adicionada a coluna BAMetodoManual ao agrupamento
  group_by( ClasseArea, BAMetodoManual) %>%
  summarise(
    Total = n(),
    
    Acertos_Estimados = sum(is_acerto_estimado, na.rm = TRUE),
    Acuracia_Pct = round(mean(is_acerto_estimado, na.rm = TRUE) * 100, 2),
    Score_Medio = round(mean(BAScore, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(parte2)

# ==============================================================================
# PARTE 3: DIVERGÊNCIAS (BACodAssociacao != BACodEstimado)
# ==============================================================================
cat("\n--- PARTE 3: Divergências (Associação Diferente da Estimativa) ---\n")

parte3 <- base_analise %>%
  filter(is_divergente) %>%
  # Adicionada a coluna BAMetodoManual ao agrupamento
  group_by( BAMetodo, BAMetodoManual) %>%
  summarise(
    Num_Situacoes = n(),
    Score_Medio_Divergente = round(mean(BAScore, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(parte3)

# ==============================================================================
# PARTE 1: MATCH DIRETO (Codigo == BACodAssociacao)
# ==============================================================================
cat("\n--- PARTE 1: Matches Diretos por Fonte e Tipo de Introdução ---\n")

parte4 <- base_analise %>%
  filter(is_match_direto) %>%
  # Adicionada a coluna BAMetodoManual ao agrupamento
  group_by( BAMetodoManual) %>%
  summarise(
    Total = n(),
    
    Acertos_Estimados = sum(is_acerto_estimado, na.rm = TRUE),
    Acuracia_Pct = round(mean(is_acerto_estimado, na.rm = TRUE) * 100, 2),
    Score_Medio = round(mean(BAScore, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(parte4)

# ==============================================================================
# 4. EXPORTAÇÃO PARA GEOPACKAGE
# ==============================================================================

# Note: The library activation block (#0) handled dependencies; 
# ensure lowercase 'lapply' is used for functional calls.

if (!exists("data_dir"))   data_dir   <- file.path(project_dir, "data")     # REVIEW
if (!exists("output_dir")) output_dir <- file.path(project_dir, "output")


file_gpkg <- file.path(output_dir, "fogos.gpkg")

library(sf)
st_write(fogos, 
         dsn = file_gpkg, 
         layer = "fogos",
         delete_dsn = TRUE, 
         quiet = FALSE)