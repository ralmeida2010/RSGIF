# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# I.19_MODIS_to_fires.R – valores das variáveis MODIS no ponto de início de cada incêndio
# =============================================================================

if (!exists("scripts_dir")) scripts_dir <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "R")
source(file.path(scripts_dir, "18_modis_config.R"))

# -----------------------------------------------------------------------------
# 0. Opções
# -----------------------------------------------------------------------------
col_id   <- "Codigo"        # identificador do incêndio
col_data <- "DHInicio"      # data/hora de início
col_x    <- "x_3763"        # coordenadas X (ETRS89 / PT-TM06, EPSG:3763)
col_y    <- "y_3763"        # coordenadas Y
crs_xy   <- "EPSG:3763"

criterio <- "fim"           # "fim" ou "inicio"

duracao_composto <- c(gpp = 8, fapar = 8, vi = 16)
idade_max        <- c(gpp = 16, fapar = 16, vi = 24)
tentativas       <- 2

juntar_estado_nacional <- TRUE

# -----------------------------------------------------------------------------
# 1. Tabela de entrada (com parsing robusto de datas para evitar erros de formato)
# -----------------------------------------------------------------------------
if (!exists("fogos")) stop("Carregue a tabela 'fogos' na sessão antes de correr este script")
fogos_df <- if (inherits(fogos, "sf")) sf::st_drop_geometry(fogos) else as.data.frame(fogos)
n <- nrow(fogos_df)

num <- function(v) if (is.numeric(v)) v else as.numeric(sub(",", ".", as.character(v), fixed = TRUE))

detectar_coord <- function(df) {
  nm  <- names(df)
  low <- tolower(nm)
  if (!is.null(col_x) && !is.null(col_y)) {
    if (!all(c(col_x, col_y) %in% nm))
      stop("Colunas '", col_x, "' / '", col_y, "' não existem em 'fogos'")
    return(list(x = col_x, y = col_y, crs = crs_xy))
  }
  pick <- function(padrao) { i <- grep(padrao, low); if (length(i)) nm[i[1]] else NA_character_ }
  lon <- pick("^lon(gitude)?(_?[0-9]+)?$")
  lat <- pick("^lat(itude)?(_?[0-9]+)?$")
  if (!is.na(lon) && !is.na(lat)) return(list(x = lon, y = lat, crs = "EPSG:4326"))
  x <- pick("^x(_?3763)?$")
  y <- pick("^y(_?3763)?$")
  if (!is.na(x) && !is.na(y)) return(list(x = x, y = y, crs = "EPSG:3763"))
  stop("Não encontrei colunas de coordenadas: defina col_x, col_y e crs_xy")
}
co <- detectar_coord(fogos_df)
message("Coordenadas: ", co$x, " / ", co$y, " (", co$crs, ")")

x <- num(fogos_df[[col_x]])
y <- num(fogos_df[[col_y]])

# Parsing robusto de datas (suporta hífens, barras, formatos PT e internacionais)
raw_datas <- as.character(fogos_df[[col_data]])
data_inc <- as_date(parse_date_time(
  raw_datas,
  orders = c(
    "Ymd HMS", "Ymd HM", "Ymd", 
    "dmY HMS", "dmY HM", "dmY",
    "dmy HMS", "dmy HM", "dmy",
    "Y/m/d H:M:S", "Y/m/d H:M", "Y/m/d",
    "d/m/Y H:M:S", "d/m/Y H:M", "d/m/Y",
    "d-m-Y H:M:S", "d-m-Y H:M", "d-m-Y"
  ),
  quiet = TRUE
))

if (all(is.na(data_inc))) stop("Não consegui ler as datas de '", col_data, "'. Verifique o formato na tabela.")
d_ref <- data_inc - 1          # dia anterior ao incêndio para consulta MODIS

# -----------------------------------------------------------------------------
# 2. Pixel da grelha comum (500 m) onde cai cada ponto de ignição
# -----------------------------------------------------------------------------
cel   <- rep(NA_integer_, n)
ok_xy <- !is.na(x) & !is.na(y) & !(x == 0 & y == 0)
if (any(ok_xy)) {
  pts <- project(vect(cbind(x[ok_xy], y[ok_xy]), type = "points", crs = co$crs), crs(grelha_pt))
  cel[ok_xy] <- cellFromXY(grelha_pt, crds(pts))
}
gv <- values(grelha_pt, mat = FALSE)
cel[!is.na(cel) & is.na(gv[cel])] <- NA
message(sum(!is.na(cel)), " de ", n, " incêndios caem num pixel de Portugal continental")


# -----------------------------------------------------------------------------
# 3. Catálogo de compostos (ordenado pela data de referência)
# -----------------------------------------------------------------------------
catalogo <- function(produto, variavel) {
  f   <- listar(produto, variavel)
  ini <- data_de_nome(f)
  fim <- as.Date(pmin(as.numeric(ini + duracao_composto[[produto]] - 1),
                      as.numeric(as.Date(paste0(year(ini), "-12-31")))))
  d <- data.frame(ficheiro = f, ini = ini, fim = fim)
  d$ref <- if (criterio == "fim") d$fim else d$ini
  d[order(d$ref, d$ini), ]
}

extrair_var <- function(cat, produto) {
  val  <- rep(NA_real_, n)
  idad <- rep(NA_real_, n)
  j    <- findInterval(as.numeric(d_ref), as.numeric(cat$ref))
  pend <- which(!is.na(cel) & !is.na(d_ref) & j > 0)
  for (t in seq_len(tentativas)) {
    if (!length(pend)) break
    pend <- pend[j[pend] >= 1]
    if (!length(pend)) break
    age  <- as.numeric(data_inc[pend]) - as.numeric(cat$ref[j[pend]])
    pend <- pend[age <= idade_max[[produto]]]
    if (!length(pend)) break
    for (jj in unique(j[pend])) {
      i <- pend[j[pend] == jj]
      v <- values(rast(cat$ficheiro[jj]), mat = FALSE)[cel[i]]
      val[i]  <- v
      idad[i] <- ifelse(is.na(v), NA_real_, as.numeric(data_inc[i]) - as.numeric(cat$ref[jj]))
    }
    pend    <- pend[is.na(val[pend])]
    j[pend] <- j[pend] - 1L
  }
  data.frame(valor = val, idade = idad)
}

modis <- data.frame(row.names = seq_len(n))
variaveis <- list(gpp_8d = c("gpp", "Gpp"), fapar = c("fapar", "Fpar"), lai = c("fapar", "Lai"),
                  ndvi = c("vi", "NDVI"), evi = c("vi", "EVI"), ndii7 = c("vi", "NDII7"))
for (nm in names(variaveis)) {
  produto <- variaveis[[nm]][1]
  message("A extrair ", nm, " ...")
  cat_v <- catalogo(produto, variaveis[[nm]][2])
  if (!nrow(cat_v)) { message("  sem ficheiros"); next }
  r <- extrair_var(cat_v, produto)
  modis[[nm]]               <- r$valor
  modis[[paste0(nm, "_idade")]] <- r$idade
  limpar_tmp()
}

# -----------------------------------------------------------------------------
# 4. GPP acumulado
# -----------------------------------------------------------------------------
message("A acumular GPP ...")
f_npp <- 0.5
f_ms  <- 2.2
cat_g <- catalogo("gpp", "Gpp")
ano_inc <- year(data_inc)
por_ano <- split(seq_len(n), ano_inc)
acum <- rep(0, n)
ncomp <- integer(n)
for (jj in seq_len(nrow(cat_g))) {
  ano_c <- year(cat_g$ini[jj])
  i <- por_ano[[as.character(ano_c)]]
  if (is.null(i)) next
  i <- i[!is.na(cel[i]) & !is.na(d_ref[i]) & cat_g$ref[jj] <= d_ref[i]]
  if (!length(i)) next
  v <- values(rast(cat_g$ficheiro[jj]), mat = FALSE)[cel[i]]
  acum[i]  <- acum[i] + ifelse(is.na(v), 0, v * 1000 * f_npp * f_ms)
  ncomp[i] <- ncomp[i] + 1L
}
modis$gpp_acum_ms <- ifelse(ncomp > 0 & !is.na(cel), acum, NA_real_)
modis$gpp_n_comp  <- ifelse(!is.na(cel), ncomp, NA_integer_)
limpar_tmp()

# -----------------------------------------------------------------------------
# 5. Greenness relativo (RG)
# -----------------------------------------------------------------------------
f_ext <- file.path(dir_deriv, "ndvi_p02_p98.tif")
if (file.exists(f_ext) && "ndvi" %in% names(modis)) {
  ext <- rast(f_ext)
  p02 <- values(ext[[1]], mat = FALSE)[cel]
  p98 <- values(ext[[2]], mat = FALSE)[cel]
  amp <- p98 - p02
  modis$rg <- ifelse(!is.na(amp) & amp >= 0.02,
                     pmin(pmax((modis$ndvi - p02) / amp, 0), 1), NA_real_)
} else {
  message("RG não calculado (falta ndvi_p02_p98.tif; corra o 03 primeiro)")
}

# -----------------------------------------------------------------------------
# 6. Estado nacional na véspera
# -----------------------------------------------------------------------------
f_comp <- file.path(dir_deriv, "serie_diaria_completa.csv")
if (juntar_estado_nacional && nivel_regiao == "Portugal" && file.exists(f_comp)) {
  nac <- read.csv(f_comp) |>
    mutate(data = as.Date(data)) |>
    select(data, any_of(c("percentil", "anomalia_pct")),
           starts_with("z_"), starts_with("pct_acum_")) |>
    rename_with(~ paste0("nac_", .x), -data)
  nac <- nac[match(d_ref, nac$data), -1, drop = FALSE]
  modis <- bind_cols(modis, nac)
}

# -----------------------------------------------------------------------------
# 7. Gravação e Otimização de Memória
# -----------------------------------------------------------------------------
save_fogosII <- FALSE
cols_main  <- intersect(c("gpp_8d", "gpp_acum_ms", "fapar", "lai", "ndvi", "evi", "ndii7", "rg"), names(modis))
cols_annex <- setdiff(names(modis), cols_main)

for (v in cols_main) fogos[[v]] <- modis[[v]]
if (!exists("fogos_anexo")) fogos_anexo <- data.frame(Codigo = fogos_df[[col_id]])
ma <- match(as.character(fogos_anexo$Codigo), as.character(fogos_df[[col_id]]))
for (v in cols_annex) fogos_anexo[[paste0("MODIS_", v)]] <- modis[[v]][ma]

message("Adicionados a fogos: ", paste(cols_main, collapse = ", "), "; a fogos_anexo: ", length(cols_annex), " colunas")

if (save_fogosII) {
  fogosII <- bind_cols(fogos_df, modis)
  saveRDS(fogosII, file.path(dir_deriv, "fogosII.rds"))
}

f_chave <- if (col_id %in% names(fogos_df)) fogos_df[[col_id]] else seq_len(n)
write.csv(bind_cols(setNames(data.frame(f_chave), col_id), modis),
          file.path(dir_deriv, "fogos_modis.csv"), row.names = FALSE)

# Cobertura por ano
cols_cob <- intersect(c("gpp_8d", "gpp_acum_ms", "fapar", "lai", "ndvi", "evi", "ndii7", "rg"), names(modis))
cobertura <- bind_cols(data.frame(ano = ano_inc), modis[cols_cob]) |>
  filter(!is.na(ano)) |>
  group_by(ano) |>
  summarise(fogos = n(), across(all_of(cols_cob), ~ round(100 * mean(!is.na(.x)), 1)),
            .groups = "drop")
message("\nCobertura (% de incêndios com valor) por ano:")
print(as_tibble(cobertura), n = Inf)