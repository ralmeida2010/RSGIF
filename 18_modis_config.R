# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# I.18_MODIS_config.R – configuração comum do pipeline MODIS para a PRFD
# (Cada script do pipeline começa com source("R/18_modis_config.R"))
#
# ORDEM DE EXECUÇÃO DO PIPELINE:
#   1. 01_descarga.R                descarrega MODIS (GPP, FAPAR/LAI, NDVI/EVI/NDII7)
#   2. 02_gpp_acumulado.R           GPP diário, acumulado desde 1 jan, percentis
#   3. 03_indices_complementares.R  índices de estado diários, anomalias, greenness
#   4. 04_manutencao.R              verificações e limpeza (funções, uso opcional)
#   5. I.19_MODIS_to_fires.R        atribuição dos valores MODIS aos pontos de fogo
#
# Unidades dos rasters em disco (já em unidades físicas):
#   Gpp = kg C/m2/8 dias | Fpar = 0-1 | Lai = m2/m2 | NDVI, EVI, NDII7 = adimensionais
# =============================================================================

suppressPackageStartupMessages({
  library(terra)      # >= 1.7
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# 1. OPÇÕES DO UTILIZADOR
# =============================================================================

# ---- Pastas -------------------------------------------------------------------
if (!exists("project_dir")) project_dir <- Sys.getenv("RSGIF_DIR", getwd())
if (!exists("scripts_dir")) scripts_dir <- file.path(project_dir, "R")
if (!exists("data_dir"))    data_dir    <- file.path(project_dir, "data")
# REVIEW: the MODIS folders were built from a variable called data_dir set to .../dados,
# which overwrote the data_dir of the main pipeline; they now use modis_root.
modis_root  <- file.path(project_dir, "dados")

dir_dados   <- file.path(modis_root, "modis")
dir_limites <- file.path(dir_dados, "limites")    # limite e regiões (gpkg)
dir_modis   <- file.path(dir_dados, "rasters")    # rasters descarregados
dir_deriv   <- file.path(dir_dados, "derivados")  # séries, zonas, percentis
dir_tmp     <- file.path(dir_dados, "tmp_terra")  # temporários do terra

# ---- Nível de agregação das séries ---------------------------------------------
# "Portugal" | "Distrito" | "Municipio" | "Freguesia"
nivel_regiao <- "Portugal"

# ---- Combustíveis (opcional) ----------------------------------------------------
usar_combustivel <- FALSE
f_combustivel    <- file.path(dir_dados, "combustivel.tif")
limiar_combust   <- 0.5      # fração mínima de combustível no pixel de 500 m

# ---- CAOP (só para regiões abaixo de "Portugal" e para o limite) ---------------
f_caop      <- file.path(dir_limites, "CAOP_Continente.gpkg")
if (!file.exists(f_caop)) {
  .shp <- list.files(file.path(data_dir, "CAOP2012"), pattern = "\\.shp$", full.names = TRUE)[1]
  if (!is.na(.shp)) f_caop <- .shp
}
camada_caop <- NULL
campos_caop <- c(Dicofre = "Dicofre", Freguesia = "Freguesia",
                 Municipio = "Municipio", Distrito = "Distrito")

# ---- Período --------------------------------------------------------------------
inicio      <- "2001-01-01"
fim         <- "2025-12-31"
periodo_ref <- 2001:2020

# ---- Gráficos ---------------------------------------------------------------------
rotular_anos <- TRUE

# ---- Controlo de escalas --------------------------------------------------------
limites_fisicos <- c(Gpp = 3, Fpar = 1, Lai = 10, NDVI = 1, EVI = 1, NDII7 = 1)
media_esperada  <- list(Gpp = c(0.005, 0.06), Fpar = c(0.15, 0.8), Lai = c(0.3, 4),
                        NDVI = c(0.2, 0.9), EVI = c(0.1, 0.7), NDII7 = c(-0.3, 0.7))
min_validos     <- 0.5

# =============================================================================
# 2. AMBIENTE
# =============================================================================
for (d in c(dir_limites, dir_modis, dir_deriv, dir_tmp))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

terraOptions(tempdir = dir_tmp, memfrac = 0.6, progress = 0)

limpar_tmp <- function() {
  tmpFiles(remove = TRUE)
  invisible(gc())
}

espaco_usado <- function(pasta = dir_dados) {
  f <- list.files(pasta, recursive = TRUE, full.names = TRUE)
  if (!length(f)) return(invisible(NULL))
  s <- tapply(file.size(f), dirname(f), sum) / 1e9
  print(round(sort(s, decreasing = TRUE), 3))
  message("Total: ", round(sum(s), 2), " GB")
  invisible(s)
}

desatualizado <- function(destino, fontes = character()) {
  fontes <- fontes[file.exists(fontes)]
  !file.exists(destino) ||
    (length(fontes) > 0 && any(file.mtime(fontes) > file.mtime(destino)))
}

doy365 <- function(data) {
  mes <- month(data)
  dia <- day(data)
  yday(make_date(2001, mes, if_else(mes == 2 & dia == 29, 28L, as.integer(dia))))
}

checar_media <- function(var, x) {
  lim <- media_esperada[[var]]
  if (is.null(lim)) return(invisible(NULL))
  m <- median(x, na.rm = TRUE)
  if (is.na(m) || m < lim[1] || m > lim[2])
    warning(var, ": mediana ", signif(m, 3), " fora do intervalo esperado [",
            lim[1], ", ", lim[2], "]. Verifique as escalas via 04_manutencao.R",
            call. = FALSE, immediate. = TRUE)
  invisible(NULL)
}

pct_ref <- function(x, ref) {
  ref <- ref[!is.na(ref)]
  if (length(ref) == 0) return(rep(NA_real_, length(x)))
  100 * ecdf(ref)(x)
}

rotulos_ano <- function(dados, y, grupos = character()) {
  dados |>
    mutate(y_rot = .data[[y]]) |>
    filter(!is.na(y_rot)) |>
    group_by(across(all_of(c(grupos, "ano")))) |>
    slice_max(doy, n = 1, with_ties = FALSE) |>
    ungroup()
}

camada_rotulos <- function(rot, cor = "grey35", tamanho = 2.3, ...) {
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      data = rot, aes(doy, y_rot, label = ano), colour = cor, size = tamanho,
      hjust = 0, direction = "y", xlim = c(367, NA), segment.size = 0.15,
      min.segment.length = 0, max.overlaps = Inf, ...)
  } else {
    geom_text(data = rot, aes(doy, y_rot, label = ano), colour = cor, size = tamanho,
              hjust = 0, nudge_x = 3, check_overlap = TRUE, ...)
  }
}

# =============================================================================
# 3. ESCRITA COMPACTA
# =============================================================================
gdal_opts <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "TILED=YES")

escrever_int <- function(r, f, escala) {
  tmp <- paste0(f, ".parcial.tif")
  writeRaster(r, tmp, datatype = "INT2S", scale = escala, offset = 0,
              NAflag = -32768, gdal = gdal_opts, overwrite = TRUE)
  file.rename(tmp, f)
}

# =============================================================================
# 4. CAOP -> LIMITE DE PORTUGAL CONTINENTAL E REGIÕES
# =============================================================================
ler_caop <- function() {
  if (file.exists(f_caop)) {
    v <- if (is.null(camada_caop)) vect(f_caop) else vect(f_caop, layer = camada_caop)
  } else if (exists("caop", envir = globalenv())) {
    v <- vect(get("caop", envir = globalenv()))
  } else {
    return(NULL)
  }
  for (nm in names(campos_caop)) {
    if (campos_caop[[nm]] %in% names(v) && nm != campos_caop[[nm]])
      names(v)[names(v) == campos_caop[[nm]]] <- nm
  }
  if (!"Dicofre" %in% names(v)) stop("Coluna Dicofre não encontrada; ajuste 'campos_caop'")
  if (!same.crs(v, "EPSG:3763")) v <- project(v, "EPSG:3763")
  dist <- suppressWarnings(as.integer(substr(v$Dicofre, 1, 2)))
  v[!is.na(dist) & dist <= 18, ]
}

f_limite <- file.path(dir_limites, "pt_continente.gpkg")

limite_pt <- function() {
  if (desatualizado(f_limite, f_caop)) {
    v <- ler_caop()
    if (!is.null(v)) {
      message("A dissolver freguesias da CAOP (pode demorar 1-2 min)...")
      pt <- aggregate(v)
    } else {
      message("CAOP não encontrada: a usar GADM")
      pt <- geodata::gadm("PRT", level = 0, path = dir_limites)
      pt <- project(crop(pt, ext(-10, -6, 36.8, 42.3)), "EPSG:3763")
    }
    writeVector(pt, f_limite, overwrite = TRUE)
  }
  vect(f_limite)
}

regioes_pt <- function(nivel = nivel_regiao) {
  if (nivel == "Portugal") return(NULL)
  campo <- if (nivel == "Freguesia") "Dicofre" else nivel
  f_reg <- file.path(dir_limites, paste0("regioes_", nivel, ".gpkg"))
  if (desatualizado(f_reg, f_caop)) {
    v <- ler_caop()
    if (is.null(v)) stop("Regiões requerem a CAOP (ficheiro ou objeto 'caop')")
    reg <- aggregate(v, by = campo)
    writeVector(reg[, campo], f_reg, overwrite = TRUE)
  }
  vect(f_reg)
}

# =============================================================================
# 5. GRELHA COMUM: 500 m, EPSG:3763, 1 = Portugal continental, NA = fora
# =============================================================================
f_grelha <- file.path(dir_deriv, "grelha_pt_500m.tif")

if (desatualizado(f_grelha, c(f_caop, f_limite))) {
  pt <- limite_pt()
  e  <- ext(pt)
  e  <- ext(floor(xmin(e) / 500) * 500 - 1000, ceiling(xmax(e) / 500) * 500 + 1000,
            floor(ymin(e) / 500) * 500 - 1000, ceiling(ymax(e) / 500) * 500 + 1000)
  g  <- rasterize(pt, rast(e, res = 500, crs = "EPSG:3763"), touches = TRUE)
  writeRaster(g, f_grelha, datatype = "INT1U", gdal = gdal_opts, overwrite = TRUE)
}
grelha_pt <- rast(f_grelha)

aoi_ll  <- project(as.polygons(ext(grelha_pt), crs = crs(grelha_pt)), "EPSG:4326")
e_ll    <- ext(aoi_ll)
bbox_ll <- c(xmin(e_ll), ymin(e_ll), xmax(e_ll), ymax(e_ll))

# =============================================================================
# 6. ZONAS
# =============================================================================
f_zonas     <- file.path(dir_deriv, paste0("zonas_", nivel_regiao,
                                           if (usar_combustivel) "_comb", "_500m.tif"))
f_zonas_tab <- sub("\\.tif$", ".csv", f_zonas)

mascara_combustivel <- function() {
  if (!file.exists(f_combustivel))
    stop("Falta ", f_combustivel, " (raster 1 = combustível, 0 = outro).")
  frac <- project(rast(f_combustivel), grelha_pt, method = "average")
  mask(ifel(frac >= limiar_combust, 1, NA), grelha_pt)
}

zonas_pt <- function() {
  fontes <- c(f_grelha, f_caop, if (usar_combustivel) f_combustivel)
  if (desatualizado(f_zonas, fontes) || !file.exists(f_zonas_tab)) {
    if (nivel_regiao == "Portugal") {
      z   <- grelha_pt
      tab <- data.frame(id = 1L, regiao = "Portugal")
    } else {
      reg    <- regioes_pt()
      campo  <- names(reg)[1]
      tab    <- data.frame(id = seq_len(nrow(reg)),
                           regiao = as.character(as.data.frame(reg)[[campo]]))
      reg$id <- tab$id
      z      <- rasterize(reg, grelha_pt, field = "id")
    }
    if (usar_combustivel) z <- mask(z, mascara_combustivel())
    z <- mask(z, grelha_pt)
    names(z) <- "zona"
    writeRaster(z, f_zonas, datatype = "INT4S", gdal = gdal_opts, overwrite = TRUE)
    write.csv(tab, f_zonas_tab, row.names = FALSE)
  }
  rast(f_zonas)
}

# =============================================================================
# 7. FICHEIROS: DATAS E LISTAGEM
# =============================================================================
data_de_nome <- function(x) {
  b <- basename(x)
  m <- regmatches(b, regexec("_(\\d{4})_(\\d{3})\\.tif$", b))
  anos <- vapply(m, function(z) if (length(z) >= 2) z[2] else NA_character_, character(1))
  doys <- vapply(m, function(z) if (length(z) >= 3) z[3] else NA_character_, character(1))
  
  res <- as.Date(rep(NA, length(x)))
  ok <- !is.na(anos) & !is.na(doys)
  if (any(ok)) {
    res[ok] <- as.Date(paste0(anos[ok], "-01-01")) + as.integer(doys[ok]) - 1L
  }
  res
}

listar <- function(produto, variavel) {
  # Ajustado para aceitar o nome exato do ficheiro (ex: Gpp, Fpar, Lai, etc.)
  pat <- if (tolower(produto) == "gpp") "^Gpp_\\d{4}_\\d{3}\\.tif$" else paste0("^", variavel, "_\\d{4}_\\d{3}\\.tif$")
  f <- list.files(file.path(dir_modis, produto), pattern = pat, full.names = TRUE)
  f[order(data_de_nome(f))]
}


# =============================================================================
# 8. SÉRIE ZONAL INCREMENTAL COM MTIME (Cache robusta)
# =============================================================================
serie_zonal <- function(ficheiros, nome, fun = NULL) {
  if (!length(ficheiros)) stop("Sem ficheiros para '", nome, "'")
  f_cache <- file.path(dir_deriv, sprintf("serie_%s_%s_composto.csv", nome, nivel_regiao))
  zonas   <- zonas_pt()
  tab     <- read.csv(f_zonas_tab, stringsAsFactors = FALSE)
  info    <- data.frame(ficheiro = basename(ficheiros),
                        mtime    = round(as.numeric(file.mtime(ficheiros))))
  
  feito <- NULL
  if (file.exists(f_cache)) {
    feito <- read.csv(f_cache, stringsAsFactors = FALSE, colClasses = c(regiao = "character"))
    if (!"mtime" %in% names(feito)) {
      message(basename(f_cache), ": cache antiga (sem mtime) -> recalcular tudo")
      feito <- NULL
    } else {
      feito <- feito |> mutate(data = as.Date(data)) |>
        semi_join(info, by = c("ficheiro", "mtime"))
    }
  }
  idx   <- !info$ficheiro %in% feito$ficheiro
  novos <- ficheiros[idx]
  mt    <- info$mtime[idx]
  if (length(novos)) message(basename(f_cache), ": ", length(novos), " ficheiros a calcular")
  
  guardar <- function(res) {
    out <- bind_rows(feito, bind_rows(res)) |> arrange(regiao, data)
    write.csv(out, f_cache, row.names = FALSE)
    out
  }
  res <- vector("list", length(novos))
  t0  <- Sys.time()
  for (i in seq_along(novos)) {
    r <- rast(novos[i])
    if (!is.null(fun)) r <- fun(r)
    z <- zonal(r, zonas, fun = "mean", na.rm = TRUE)
    res[[i]] <- data.frame(ficheiro = basename(novos[i]),
                           data     = data_de_nome(novos[i]),
                           regiao   = tab$regiao[match(z[[1]], tab$id)],
                           valor    = ifelse(is.nan(z[[2]]), NA_real_, z[[2]]),
                           mtime    = mt[i])
    if (i %% 50 == 0) limpar_tmp()
    if (i %% 100 == 0) {
      message(sprintf("  %d/%d (%.0f s)", i, length(novos),
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      guardar(res[seq_len(i)])
    }
  }
  guardar(res)
}