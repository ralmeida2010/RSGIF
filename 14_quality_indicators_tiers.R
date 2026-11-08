# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 14 - former Audit_PRFD_v3.R
# =============================================================================
# Audit_PRFD_v3.R  --  Record-level quality indicators, tiers, integrity
#                      indices, and Monte Carlo sensitivity of the PRFD
# =============================================================================
# SUMMARY (English)
#   Builds the audit table (audit_table_prfd): spatial uncertainty radius R95,
#   area agreement with the PBA perimeter or satellite area, temporal
#   imputation level, multi-source score, and low-confidence PBA links; assigns
#   tiers A, B, U, C with a single parameterized function; computes annual and
#   decadal integrity indices; and runs the Monte Carlo sensitivity analysis
#   with standardized regression coefficients (Tables 8-9, Appendix C).
#   v3 (changes in preparar_base_auditoria only):
#     - imputation read from the flags of 4-FireDataImputation.R
#       (Flag_Area_Imputed, Flag_DHFim_Imputed); in v2 records whose original
#       end time or area was missing were classed as "Observed";
#     - imputation is treated as MAR: a single imputed (or model-replaced) value is
#       "Partially Imputed"; two make the record "Heavily Imputed / Anomaly".
#       DHFim anomalies (rules 8-9 of script 3) count as one model-replaced value
#       (parameter anomalia_como = "parcial"; "ignorar" and "pesada" also available);
#     - anomalies read from Flag_DHFim_Anomaly or the exact note
#       "DHFim anomaly" of script 3;
#     - QI_MultiSource counts only the PBA perimeter and SEVIRI FRP hotspots (0-2): the
#       EFFIS / PBSA / MODIS severity areas are measured inside the PBA perimeter and are not
#       independent; QI_Area has no satellite branch (no perimeter = not verified);
#     - estimated area read from AreaTotalEstimada; flags read
#       from the annex table fogos_anexo (argument `anexo`, linked by Codigo).
# =============================================================================
# ==============================================================================
# AUDITORIA E ANÁLISE DE SENSIBILIDADE DO PRFD - VERSÃO 2 (Audit_PRFD_v2.R)
# ------------------------------------------------------------------------------
# Alterações face a Audit_PRFD.R (ver também plano_resposta_editor.md):
#  1. Uma ÚNICA função de classificação (classificar_tiers) usada na linha de
#     base E no Monte Carlo. Na v1 o Monte Carlo usava regras diferentes (p.ex.
#     todo o registo sem geometria era "Discrepante" -> Tier C), pelo que a
#     Tab. 8 não media a sensibilidade das regras da Tab. 7.
#  2. Precisão espacial pelo raio R95 (como no artigo): 0 dentro do polígono,
#     distância ao polígono, modelo toponímico ou 3500 m sem topónimo. Na v1 o
#     Tier A usava a distância ponto-polígono e as classes R95 só entravam no
#     Tier C, e "Classe C (Sem Topónimo)" nunca levava a Tier C.
#  3. Novo "Tier U (Unverifiable)": registos sem geometria, sem satélite e sem
#     qualquer fonte independente, que na v1 caíam em Tier B por omissão.
#     Os índices são calculados com peso w_U (0 = conservador) e com w_U = w_B
#     (equivalente à v1), para mostrar quanto o índice depende desse pressuposto.
#  4. Propagação da qualidade da associação PBA-PRFD (BAMetodo, BAScore):
#     associações pelo método FRP ou com score > 0.6 não podem ser Tier A.
#  5. Monte Carlo: índice da DÉCADA calculado em cada simulação (somas de área
#     na década) e só depois resumido entre simulações. Na v1 min/max/sd
#     misturavam variabilidade entre anos com sensibilidade aos parâmetros.
#     Acrescenta coeficientes de regressão padronizados (SRC) para dizer qual
#     o parâmetro que domina a incerteza em cada década.
# ==============================================================================

# Guard against dplyr verbs masked by other packages (e.g. MASS::select, raster::select, plyr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename
mutate <- dplyr::mutate; summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count

library(dplyr)
library(lubridate)
library(sf)
library(purrr)

if (!exists("output_dir")) output_dir <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "output")

# ------------------------------------------------------------------------------
# 0. PARÂMETROS DE BASE E INTERVALOS DO MONTE CARLO
# ------------------------------------------------------------------------------
PARAM_BASE <- list(lim_esp_A = 500, lim_esp_B = 2000,   # m
                   tau_alta = 20, tau_disc = 50,        # % (geometria)
                   tol_abs_ha = 1.0,                    # ha
                   score_max = 0.6,                     # score PBA-PRFD aceite
                   w_A = 1.0, w_B = 0.65, w_U = 0.0)

PARAM_MC <- list(lim_esp_A = c(300, 700), lim_esp_B = c(1500, 2500),
                 tau_alta = c(10, 30), tau_disc = c(40, 60),
                 w_B = c(0.50, 0.80), w_U = c(0.0, 0.65))
N_SIM <- 500
SEED  <- 123

R95_SEM_TOPONIMO  <- 3500  # m, só se não houver INE nem calibração (último recurso)
R95_COORD_CAMPO   <- 1000  # m, registos pós-2001 com coordenadas de campo e sem polígono (classe B)

decada <- function(ano) case_when(ano <= 1989 ~ "1980-1989", ano <= 1999 ~ "1990-1999",
                                  ano <= 2009 ~ "2000-2009", ano <= 2019 ~ "2010-2019",
                                  TRUE ~ "2020-2025")

# ==============================================================================
# 1. MODELO DE PRECISÃO ESPACIAL TOPONÍMICA (v2.1 - percentis empíricos)
# ------------------------------------------------------------------------------
# A v1/v2 usava R95 = previsão OLS da distância + P95 dos resíduos absolutos.
# Com R2 ~ 0.11 a previsão é quase constante e os resíduos dominam: todos os
# registos anteriores a 2001 ficavam com ~8 779 m. Agora R95 (e R50) são
# percentis EMPÍRICOS da distância topónimo -> coordenada de campo (registos
# pós-2001 de tmpDist), calculados na célula mais fina com >= R95_MIN_N casos:
#   concelho x tipo -> concelho -> distrito x tipo -> distrito -> tipo -> global
# Registos sem topónimo utilizável recebem o R95 do concelho/distrito (todas as
# categorias), não um valor fixo inferior ao dos registos com topónimo.
# validar_R95_cv() testa a cobertura com validação cruzada por concelho.
# ==============================================================================
R95_MIN_N <- 30       # nº mínimo de casos de calibração por célula
R95_DMAX  <- 50000    # m; desvios >= 50 km = erros grosseiros, excluídos

extrair_atributos_fogos <- function(df) {
  df %>%
    mutate(
      concelho = floor(as.numeric(INE) / 100),
      distrito = floor(as.numeric(INE) / 10000),
      local_str = trimws(as.character(Local)),
      comp_local = nchar(local_str),
      is_nulo = is.na(local_str) | local_str == "" | comp_local < 3,
      local_clean = tolower(chartr("áàãâéêíóôõúüçÁÀÃÂÉÊÍÓÔÕÚÜÇ",
                                   "aaaaeeiooouucAAAAEEIOOOUUC", local_str)),
      # \\b: palavra inteira (na v1 "se" apanhava "serra", "casa"...)
      tipo_local = case_when(
        is_nulo ~ "Ausente/Invalido",
        grepl("\\b(casal|aldeia|quinta|lugar|vila|rua|igreja|povoa|bairro|sao|santa|se|cidade)\\b", local_clean) ~ "Povoacao",
        grepl("\\b(serra|cabeco|alto|pico|lomba|monte|penha|cerro|penedo|moinho)\\b", local_clean) ~ "Oronimia",
        grepl("\\b(rio|ribeira|vale|val|lagoa|barragem|barrage|arroio|fonte|poco|foz|ria)\\b", local_clean) ~ "Hidronimia",
        grepl("\\b(freguesia|desconhecido|varios|mato|mata|floresta|pinhal)\\b", local_clean) ~ "Generico",
        TRUE ~ "Outro_Toponimo")
    )
}

NIVEIS_R95 <- list(c("concelho", "tipo_local"), "concelho",
                   c("distrito", "tipo_local"), "distrito", "tipo_local")
NOMES_NIVEIS_R95 <- c("concelho x tipo", "concelho", "distrito x tipo", "distrito", "tipo")

tabelas_R95 <- function(treino) {
  lapply(NIVEIS_R95, function(v) {
    treino %>% filter(if_all(all_of(v), ~ !is.na(.x))) %>%
      group_by(across(all_of(v))) %>%
      summarise(n_cal = n(),
                R50 = quantile(distance, 0.50, names = FALSE),
                R95 = quantile(distance, 0.95, names = FALSE), .groups = "drop") %>%
      filter(n_cal >= R95_MIN_N)
  })
}

# atribui R50/R95 descendo na hierarquia; 'niveis_validos' permite excluir níveis
atribuir_R95 <- function(prep, tabs, global, niveis_validos = seq_along(NIVEIS_R95)) {
  prep$R95 <- NA_real_; prep$R50 <- NA_real_; prep$R95_nivel <- NA_character_; prep$R95_n <- NA_integer_
  for (k in niveis_validos) {
    v <- NIVEIS_R95[[k]]
    # registos sem topónimo só usam níveis sem tipo (concelho, distrito)
    elegivel <- is.na(prep$R95) & (!prep$is_nulo | !("tipo_local" %in% v))
    idx <- which(elegivel)
    if (length(idx) == 0) next
    j <- left_join(prep[idx, v, drop = FALSE], tabs[[k]], by = v)
    ok <- !is.na(j$R95)
    prep$R95[idx[ok]] <- j$R95[ok]; prep$R50[idx[ok]] <- j$R50[ok]
    prep$R95_n[idx[ok]] <- j$n_cal[ok]; prep$R95_nivel[idx[ok]] <- NOMES_NIVEIS_R95[k]
  }
  falta <- is.na(prep$R95)
  prep$R95[falta] <- global["R95"]; prep$R50[falta] <- global["R50"]
  prep$R95_nivel[falta] <- "global"; prep$R95_n[falta] <- global["n"]
  prep
}

preparar_treino_R95 <- function(tmpDist_df) {
  extrair_atributos_fogos(tmpDist_df) %>%
    filter(!is.na(distance), distance > 0, distance < R95_DMAX)
}

aplicar_classificacao_espacial <- function(fogos_df, tmpDist_df) {
  treino <- preparar_treino_R95(tmpDist_df)
  tabs   <- tabelas_R95(treino)
  global <- c(R50 = quantile(treino$distance, 0.50, names = FALSE),
              R95 = quantile(treino$distance, 0.95, names = FALSE), n = nrow(treino))
  prep <- atribuir_R95(extrair_atributos_fogos(fogos_df), tabs, global)
  prep %>%
    mutate(dist_pred_m = R50,                 # distância mediana esperada
           erro_total_esperado = R95) %>%     # nome mantido por compatibilidade
    select(-local_clean, -R50, -R95)
}

# Validação cruzada bloqueada por concelho: os concelhos de teste não entram na
# calibração, logo os seus registos usam os níveis distrito/tipo/global.
validar_R95_cv <- function(tmpDist_df, k = 5, seed = 42) {
  treino <- preparar_treino_R95(tmpDist_df) %>% filter(!is.na(concelho))
  set.seed(seed)
  conc <- unique(treino$concelho)
  fold_de <- setNames(sample(rep(seq_len(k), length.out = length(conc))), conc)
  treino$fold <- fold_de[as.character(treino$concelho)]
  res <- bind_rows(lapply(seq_len(k), function(f) {
    cal <- treino[treino$fold != f, ]; tst <- treino[treino$fold == f, ]
    g <- c(R50 = quantile(cal$distance, .5, names = FALSE),
           R95 = quantile(cal$distance, .95, names = FALSE), n = nrow(cal))
    atribuir_R95(tst, tabelas_R95(cal), g) %>%
      transmute(fold = f, R95_nivel, distance, R95, coberto = distance <= R95)
  }))
  list(por_fold = res %>% group_by(fold) %>%
         summarise(n = n(), Cobertura_pct = round(100 * mean(coberto), 2),
                   R95_mediano_m = round(median(R95)), .groups = "drop"),
       por_nivel = res %>% group_by(R95_nivel) %>%
         summarise(n = n(), Cobertura_pct = round(100 * mean(coberto), 2),
                   R95_mediano_m = round(median(R95)), .groups = "drop"),
       total = res %>% summarise(n = n(), Cobertura_pct = round(100 * mean(coberto), 2)))
}

# ==============================================================================
# 2. VARIÁVEIS DE BASE POR REGISTO (calculadas uma só vez)
# ==============================================================================
get_col <- function(df, name) {
  alt <- gsub("-", ".", name, fixed = TRUE)
  if (name %in% names(df)) return(df[[name]])
  if (alt %in% names(df))  return(df[[alt]])
  rep(NA, nrow(df))
}
is_different_num <- function(x, y, tol = 1e-4) {
  res <- rep(FALSE, length(x)); ok <- !is.na(x) & !is.na(y)
  res[ok] <- abs(as.numeric(x[ok]) - as.numeric(y[ok])) > tol; res
}
is_different_time <- function(t1, t2, tol_mins = 1) {
  res <- rep(FALSE, length(t1)); ok <- !is.na(t1) & !is.na(t2)
  if (any(ok)) {
    d <- abs(as.numeric(difftime(as.POSIXct(t2[ok]), as.POSIXct(t1[ok]), units = "mins")))
    res[ok] <- !is.na(d) & d > tol_mins
  }
  res
}
has_value <- function(x) {
  if (is.numeric(x)) return(!is.na(x) & x != 0)
  x <- as.character(x); !is.na(x) & trimws(x) != "" & trimws(x) != "0"
}

# O anexo (marcas de imputação e anomalia) vem da memória ou, se a sessão foi reiniciada, de
# output/fogos_anexo.rds (gravado pelo I.13). Sem ele os tiers ficam errados (ver o aviso abaixo).
carregar_anexo <- function() {
  if (exists("fogos_anexo")) return(fogos_anexo)
  f <- file.path(output_dir, "fogos_anexo.rds")
  if (file.exists(f)) { message("Anexo lido de ", f); return(readRDS(f)) }
  NULL
}

preparar_base_auditoria <- function(fogos, tmpDist = NULL, anexo = carregar_anexo()) {
  if (!is.null(tmpDist)) fogos <- aplicar_classificacao_espacial(fogos, tmpDist)
  # v3: variáveis auxiliares (marcas) vêm do anexo, ligado por Codigo
  if (!is.null(anexo)) {
    extra <- setdiff(names(anexo), "Codigo")     # o anexo prevalece sobre colunas homónimas
    if (length(extra)) {
      m <- match(as.character(fogos$Codigo), as.character(anexo$Codigo))
      for (v in extra) fogos[[v]] <- anexo[[v]][m]
    }
  }
  
  geoms <- if ("geom" %in% names(fogos) && inherits(fogos[["geom"]], "sfc")) fogos[["geom"]] else NULL
  n <- nrow(fogos)
  
  # área do polígono associado (ha)
  area_geom <- rep(NA_real_, n)
  if (!is.null(geoms)) {
    vazio <- st_is_empty(geoms)
    area_geom[!vazio] <- as.numeric(st_area(geoms[!vazio])) / 10000
  }
  # distância ponto de ignição -> polígono (m)
  x <- suppressWarnings(as.numeric(get_col(fogos, "x_3763")))
  y <- suppressWarnings(as.numeric(get_col(fogos, "y_3763")))
  dist_geom <- rep(NA_real_, n)
  if (!is.null(geoms)) {
    ok <- which(!is.na(x) & !is.na(y) & !st_is_empty(geoms))
    if (length(ok) > 0) {
      pts <- st_as_sf(data.frame(x = x[ok], y = y[ok]), coords = c("x", "y"), crs = 3763)
      dist_geom[ok] <- as.numeric(st_distance(pts, st_set_crs(geoms[ok], 3763), by_element = TRUE))
    }
  }
  
  AreaTotal <- suppressWarnings(as.numeric(get_col(fogos, "AreaTotal")))
  # v3: AreaTotalEstimada (nome atual); AreaTotalEstimado em versões anteriores
  AreaEst   <- suppressWarnings(as.numeric(if ("AreaTotalEstimada" %in% names(fogos))
    get_col(fogos, "AreaTotalEstimada") else get_col(fogos, "AreaTotalEstimado")))
  frp   <- suppressWarnings(as.numeric(get_col(fogos, "Num_Pontos_FRP")))
  
  dif_rel_geom <- ifelse(AreaTotal > 0 & area_geom > 0,
                         abs(AreaTotal - area_geom) / pmax(AreaTotal, area_geom) * 100, NA_real_)
  dif_abs_geom <- abs(AreaTotal - area_geom)
  
  # v3: imputação lida das marcas de 4-FireDataImputation.R. Na v2, um valor
  # original em falta (NA) dava "não diferente" e os registos imputados por
  # falta de DHFim ou de área ficavam "Observed".
  tem_flag <- function(nm) nm %in% names(fogos) || gsub("-", ".", nm, fixed = TRUE) %in% names(fogos)
  if (!all(vapply(c("Flag_Area_Imputed", "Flag_DHFim_Imputed", "Flag_DHFim_Anomaly"), tem_flag, logical(1))))
    stop("Faltam as marcas Flag_Area_Imputed / Flag_DHFim_Imputed / Flag_DHFim_Anomaly: ",
         "carregue o anexo (fogos_anexo.rds). Sem elas todos os registos ficam 'Heavily Imputed' e o Tier C explode.")
  DHFim_o  <- get_col(fogos, "DHFim"); DHFim_e <- get_col(fogos, "DHFimEstimado")
  area_imp <- if (tem_flag("Flag_Area_Imputed")) as.logical(get_col(fogos, "Flag_Area_Imputed")) %in% TRUE else
    is_different_num(AreaTotal, AreaEst) | (is.na(AreaTotal) & !is.na(AreaEst))
  dur_imp  <- if (tem_flag("Flag_DHFim_Imputed")) as.logical(get_col(fogos, "Flag_DHFim_Imputed")) %in% TRUE else
    is_different_num(get_col(fogos, "DuracaoHoras"), get_col(fogos, "DuracaoHorasEstimado")) |
    is_different_time(DHFim_o, DHFim_e) | (is.na(DHFim_o) & !is.na(DHFim_e))
  # anomalias de 3-FireDHAAManagment.R (regras 8-9: duração fora dos limites -> o DHFim é substituído
  # por uma previsão a partir da área). ATENÇÃO: as barreiras da regra 8 são calculadas sobre todos
  # os fogos, por isso a quase totalidade dos fogos grandes (>= 100 ha) fica marcada.
  anomalia <- if (tem_flag("Flag_DHFim_Anomaly")) as.logical(get_col(fogos, "Flag_DHFim_Anomaly")) %in% TRUE else
    grepl("DHFim anomaly", as.character(get_col(fogos, "Observacoes")), fixed = TRUE)
  # Como tratar a anomalia no QI_Temporal (parâmetro anomalia_como, definido antes da chamada):
  #   "parcial" (por defeito): a duração foi modelada -> conta como UMA variável modelada, tal como uma
  #                imputação (sem dupla contagem se o MICE já a imputou);
  #   "ignorar": só contam as imputações (Flag_*_Imputed);
  #   "pesada" : qualquer anomalia -> Heavily Imputed / Anomaly (regra da v3, ligada ao MNAR; leva ~90 %
  #                da área ao Tier C porque os fogos grandes estão quase todos marcados).
  if (!exists("anomalia_como")) anomalia_como <- "parcial"
  n_imp <- area_imp + dur_imp + if (anomalia_como == "parcial") as.integer(anomalia & !dur_imp) else 0L
  temporal <- if (anomalia_como == "pesada") {
    case_when(n_imp == 0 & !anomalia ~ "Observed",
              n_imp == 1 & !anomalia ~ "Partially Imputed",
              TRUE ~ "Heavily Imputed / Anomaly")
  } else {
    case_when(n_imp == 0 ~ "Observed", n_imp == 1 ~ "Partially Imputed",
              TRUE ~ "Heavily Imputed / Anomaly")
  }
  message("Anomalia de DHFim marcada em ", round(100 * mean(anomalia), 1), "% dos registos (tratamento: ", anomalia_como, ")")
  tt <- table(temporal)
  message("QI_Temporal: ", paste0(names(tt), " ", round(100 * tt / sum(tt), 1), "%", collapse = " | "))
  
  # qualidade da associação PBA-PRFD (27- ID BA Fogos Association.R)
  metodo <- as.character(get_col(fogos, "BAMetodo"))
  score  <- suppressWarnings(as.numeric(get_col(fogos, "BAScore")))
  manual <- as.character(get_col(fogos, "BAMetodoManual"))
  
  # ATENÇÃO: em 27, 'BACodAssociacao = Codigo' é preenchido para TODOS os
  # registos após o left_join, logo não indica associação. Na v1 isto fazia
  # PBA_Matched = TRUE em todo o lado (QI_MultiSource >= 1 para todos).
  # Aqui a associação PBA = polígono não vazio ou código estimado/manual.
  pba <- (!is.na(area_geom) & area_geom > 0) | has_value(get_col(fogos, "BACodEstimado")) |
    manual %in% "Manual Introduction"
  # fontes independentes: perímetro PBA e pontos de calor SEVIRI FRP (as áreas EFFIS/PBSA/MODIS de
  # severidade vêm do próprio perímetro PBA e não contam como fontes independentes)
  multi <- pba + (!is.na(frp) & frp > 0)
  match_baixa_conf <- !is.na(area_geom) & !(manual %in% "Manual Introduction") &
    ((metodo %in% "FRP") | (!is.na(score) & score > PARAM_BASE$score_max))
  
  ano <- year(as.POSIXct(get_col(fogos, "DHInicio")))
  tem_coord <- !is.na(x) & !is.na(y)
  topo_r95  <- suppressWarnings(as.numeric(get_col(fogos, "erro_total_esperado")))
  sem_topo  <- as.logical(get_col(fogos, "is_nulo")) %in% TRUE
  
  # R95 (m) - hierarquia do Apêndice C do artigo
  R95 <- case_when(
    !is.na(dist_geom) & dist_geom == 0 ~ 0,
    !is.na(dist_geom)                  ~ dist_geom,
    ano < 2001 & !is.na(topo_r95)      ~ topo_r95,       # geocodificação toponímica (inclui sem topónimo -> R95 do concelho/distrito)
    ano < 2001                         ~ R95_SEM_TOPONIMO,
    ano >= 2001 & tem_coord            ~ R95_COORD_CAMPO,
    !is.na(topo_r95)                   ~ topo_r95,
    TRUE                               ~ R95_SEM_TOPONIMO)
  
  tibble(
    Codigo = get_col(fogos, "Codigo"), Ano = ano, Decada = decada(ano),
    AreaTotal = ifelse(is.na(AreaTotal) | AreaTotal < 0, 0, AreaTotal),
    Area_imputada = area_imp,
    Has_Geometry = !is.na(area_geom) & area_geom > 0,
    G4_Area_Geom_ha = area_geom, G4_RelDiff_AreaTotal_Geom_pct = dif_rel_geom,
    AbsDiff_Geom_ha = dif_abs_geom,
    G2_Dist_Ponto_Geom_m = dist_geom, QI_Spatial_R95_m = R95,
    QI_Spatial_R50_Toponym_m = suppressWarnings(as.numeric(get_col(fogos, "dist_pred_m"))),
    QI_Spatial_R95_Level = get_col(fogos, "R95_nivel"),
    QI_Spatial_R95_Ncal = get_col(fogos, "R95_n"),
    QI_Spatial_Toponym_Class = get_col(fogos, "tipo_local"),
    QI_Temporal_Imputation_Level = temporal,
    QI_MultiSource_Validation_Score = multi,
    BAMetodo = metodo, BAScore = score, Match_Low_Confidence = match_baixa_conf
  )
}

# ==============================================================================
# 3. CLASSIFICAÇÃO (ÚNICA, PARAMETRIZADA)
# ==============================================================================
classificar_tiers <- function(b, p = PARAM_BASE, usar_tier_U = TRUE) {
  esp <- case_when(b$QI_Spatial_R95_m <= p$lim_esp_A ~ "A",
                   b$QI_Spatial_R95_m <= p$lim_esp_B ~ "B",
                   TRUE ~ "C")
  area <- case_when(
    b$Has_Geometry & (b$G4_RelDiff_AreaTotal_Geom_pct <= p$tau_alta | b$AbsDiff_Geom_ha <= p$tol_abs_ha) ~ "High (Geom)",
    b$Has_Geometry & b$G4_RelDiff_AreaTotal_Geom_pct <= p$tau_disc ~ "Moderate (Geom)",
    b$Has_Geometry ~ "Discrepant (Geom)",
    !b$Area_imputada & b$AreaTotal > 0 ~ "Tabular only (not verified)",
    TRUE ~ "Imputed / no reference")
  temp <- b$QI_Temporal_Imputation_Level
  ms   <- b$QI_MultiSource_Validation_Score
  
  tier <- case_when(
    area == "Discrepant (Geom)" |
      temp == "Heavily Imputed / Anomaly" |
      (!b$Has_Geometry & esp == "C" & ms == 0) ~ "Tier C (Bronze / Caution)",
    (b$Has_Geometry & area == "High (Geom)" & esp == "A" & temp == "Observed" & !b$Match_Low_Confidence) |
      (!b$Has_Geometry & area == "Tabular only (not verified)" &
         esp %in% c("A", "B") & temp == "Observed" & ms >= 1) ~ "Tier A (Gold)",
    usar_tier_U & !b$Has_Geometry & ms == 0 ~ "Tier U (Unverifiable)",
    TRUE ~ "Tier B (Silver)")
  
  list(tier = tier, esp = esp, area = area)
}

# ==============================================================================
# 4. KPIs ANUAIS E DECADAIS (LINHA DE BASE)
# ==============================================================================
kpis_integridade <- function(audit, p = PARAM_BASE, grupo = Ano) {
  w <- with(audit, case_when(PRFD_Overall_Quality_Tier == "Tier A (Gold)" ~ p$w_A,
                             PRFD_Overall_Quality_Tier == "Tier B (Silver)" ~ p$w_B,
                             PRFD_Overall_Quality_Tier == "Tier U (Unverifiable)" ~ p$w_U,
                             TRUE ~ 0))
  w_v1 <- ifelse(audit$PRFD_Overall_Quality_Tier == "Tier U (Unverifiable)", p$w_B, w)
  audit %>% mutate(w = w, w_v1 = w_v1) %>%
    filter(!is.na(Ano), Ano >= 1980, Ano <= 2025) %>%
    group_by({{ grupo }}) %>%
    summarise(
      Total_Records = n(),
      Total_Area_ha = round(sum(AreaTotal), 2),
      Geometry_Coverage_Pct_Count = round(100 * mean(Has_Geometry), 2),
      Geometry_Coverage_Pct_Area  = round(100 * sum(AreaTotal[Has_Geometry]) / sum(AreaTotal), 2),
      Tier_A_Pct_Count = round(100 * mean(PRFD_Overall_Quality_Tier == "Tier A (Gold)"), 2),
      Tier_B_Pct_Count = round(100 * mean(PRFD_Overall_Quality_Tier == "Tier B (Silver)"), 2),
      Tier_U_Pct_Count = round(100 * mean(PRFD_Overall_Quality_Tier == "Tier U (Unverifiable)"), 2),
      Tier_C_Pct_Count = round(100 * mean(PRFD_Overall_Quality_Tier == "Tier C (Bronze / Caution)"), 2),
      Tier_A_Pct_Area  = round(100 * sum(AreaTotal[PRFD_Overall_Quality_Tier == "Tier A (Gold)"]) / sum(AreaTotal), 2),
      Tier_B_Pct_Area  = round(100 * sum(AreaTotal[PRFD_Overall_Quality_Tier == "Tier B (Silver)"]) / sum(AreaTotal), 2),
      Tier_U_Pct_Area  = round(100 * sum(AreaTotal[PRFD_Overall_Quality_Tier == "Tier U (Unverifiable)"]) / sum(AreaTotal), 2),
      Tier_C_Pct_Area  = round(100 * sum(AreaTotal[PRFD_Overall_Quality_Tier == "Tier C (Bronze / Caution)"]) / sum(AreaTotal), 2),
      Weighted_RelErr_Geom_pct = round(sum((G4_RelDiff_AreaTotal_Geom_pct * AreaTotal)[Has_Geometry], na.rm = TRUE) /
                                         sum(AreaTotal[Has_Geometry & !is.na(G4_RelDiff_AreaTotal_Geom_pct)]), 2),
      Median_RelErr_Geom_pct = round(median(G4_RelDiff_AreaTotal_Geom_pct, na.rm = TRUE), 2),
      DB_Integrity_Index_Count    = round(100 * mean(w), 2),
      DB_Integrity_Index_Area     = round(100 * sum(w * AreaTotal) / sum(AreaTotal), 2),
      DB_Integrity_Index_Area_wU_eq_wB = round(100 * sum(w_v1 * AreaTotal) / sum(AreaTotal), 2),
      .groups = "drop")
}

# ==============================================================================
# 5. MONTE CARLO + COEFICIENTES DE REGRESSÃO PADRONIZADOS
# ==============================================================================
simular_sensibilidade <- function(base, n_sim = N_SIM, rng = PARAM_MC, seed = SEED) {
  set.seed(seed)
  base <- base %>% filter(!is.na(Ano), Ano >= 1980, Ano <= 2025)
  params <- map_dfr(seq_len(n_sim), function(i) {
    p <- PARAM_BASE
    for (k in names(rng)) p[[k]] <- runif(1, rng[[k]][1], rng[[k]][2])
    if (p$lim_esp_B <= p$lim_esp_A) p$lim_esp_B <- p$lim_esp_A + 1
    if (p$w_U > p$w_B) p$w_U <- p$w_B       # um registo não verificável nunca vale mais que B
    as_tibble(p) %>% mutate(Simulacao_ID = i)
  })
  res <- map_dfr(seq_len(n_sim), function(i) {
    p <- as.list(params[i, ])
    cl <- classificar_tiers(base, p)
    w <- case_when(cl$tier == "Tier A (Gold)" ~ p$w_A, cl$tier == "Tier B (Silver)" ~ p$w_B,
                   cl$tier == "Tier U (Unverifiable)" ~ p$w_U, TRUE ~ 0)
    tibble(Ano = base$Ano, Decada = base$Decada, A = base$AreaTotal, wA = w * base$AreaTotal, w = w) %>%
      group_by(Decada) %>%
      summarise(Idx_Area = 100 * sum(wA) / sum(A), Idx_Count = 100 * mean(w), .groups = "drop") %>%
      mutate(Simulacao_ID = i)
  })
  list(params = params, res = res)
}

resumir_sensibilidade <- function(sim) {
  sim$res %>% group_by(Decada) %>%
    summarise(Mean = mean(Idx_Area), SD = sd(Idx_Area),
              P05 = quantile(Idx_Area, 0.05), P95 = quantile(Idx_Area, 0.95),
              Min = min(Idx_Area), Max = max(Idx_Area), Range = Max - Min,
              .groups = "drop") %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

# SRC: regressão do índice padronizado nos parâmetros padronizados, por década.
# SRC^2 ~ fracção da variância explicada por cada parâmetro (modelo quase linear se R2 > 0.7)
src_sensibilidade <- function(sim) {
  d <- sim$res %>% left_join(sim$params, by = "Simulacao_ID")
  pars <- names(PARAM_MC)
  d %>% group_by(Decada) %>% group_modify(function(g, k) {
    z <- as.data.frame(scale(g[, c("Idx_Area", pars)]))
    z <- z[, colSums(is.na(z)) == 0, drop = FALSE]
    if (!"Idx_Area" %in% names(z)) return(tibble(Parametro = pars, SRC = 0, SRC2 = 0, R2 = NA_real_))
    m <- lm(Idx_Area ~ ., data = z)
    cf <- coef(m)[-1]
    tibble(Parametro = names(cf), SRC = round(cf, 3), SRC2 = round(cf^2, 3),
           R2 = round(summary(m)$r.squared, 3))
  }) %>% ungroup()
}

# ==============================================================================
# 6. EXECUÇÃO
# ==============================================================================
if (exists("fogos")) {
  cat(">>> A preparar base de auditoria...\n")
  # REVIEW: calibration table of the toponym uncertainty (Sect. 2.1.2) built from
  # data/ValidationTopo.csv (toponym distance of the 2001-2025 records, computed in the
  # SGIF database; see README, Inputs) when tmpDist is not in memory
  if (!exists("tmpDist")) {
    if (!exists("data_dir")) data_dir <- file.path(dirname(output_dir), "data")
    f <- c(file.path(data_dir, "ValidationTopo.csv"), file.path(data_dir, "processed", "ValidationTopo.csv"))
    f <- f[file.exists(f)][1]
    if (!is.na(f)) {
      vt <- read.csv(f, stringsAsFactors = FALSE)
      names(vt) <- tolower(names(vt))
      
      # Função auxiliar para encontrar a primeira coluna correspondente disponível
      encontrar_coluna <- function(df, opcoes) {
        match_col <- intersect(opcoes, names(df))
        if (length(match_col) > 0) df[[match_col[1]]] else rep(NA, nrow(df))
      }
      
      tmpDist <- data.frame(
        Codigo   = encontrar_coluna(vt, c("codigo", "id", "gid", "fogoid")),
        Ano      = encontrar_coluna(vt, c("ano", "year")),
        INE      = encontrar_coluna(vt, c("ine", "dicodigo", "concelho")),
        Local    = encontrar_coluna(vt, c("local", "localidade", "toponimo")),
        distance = as.numeric(encontrar_coluna(vt, c("distancia", "dist", "distance")))
      )
      
      # Remover linhas com NA essenciais para evitar erros a jusante
      tmpDist <- tmpDist %>% filter(!is.na(distance))
      
      message("tmpDist built from ", f, ": ", nrow(tmpDist), " records (distance in m)")
    } else {
      message("ValidationTopo.csv not found: R95 calibration skipped")
    }
  }
  base_audit <- preparar_base_auditoria(fogos, if (exists("tmpDist")) tmpDist else NULL)
  
  if (exists("tmpDist")) {
    cat(">>> Validação cruzada do R95 toponímico (por concelho)...\n")
    cv_R95 <- validar_R95_cv(tmpDist)
    print(cv_R95$total); print(cv_R95$por_nivel)
    write.csv(cv_R95$por_fold,  file.path(output_dir, "prfd_R95_cv_por_fold.csv"), row.names = FALSE)
    write.csv(cv_R95$por_nivel, file.path(output_dir, "prfd_R95_cv_por_nivel.csv"), row.names = FALSE)
  }
  R95_resumo <- base_audit %>% filter(!Has_Geometry) %>% group_by(Decada) %>%
    summarise(n = n(), R95_P05 = quantile(QI_Spatial_R95_m, .05, na.rm = TRUE),
              R95_mediana = median(QI_Spatial_R95_m, na.rm = TRUE),
              R95_P95 = quantile(QI_Spatial_R95_m, .95, na.rm = TRUE),
              pct_ClasseA = 100 * mean(QI_Spatial_R95_m <= PARAM_BASE$lim_esp_A, na.rm = TRUE),
              pct_ClasseB = 100 * mean(QI_Spatial_R95_m > PARAM_BASE$lim_esp_A & QI_Spatial_R95_m <= PARAM_BASE$lim_esp_B, na.rm = TRUE),
              pct_ClasseC = 100 * mean(QI_Spatial_R95_m > PARAM_BASE$lim_esp_B, na.rm = TRUE),
              .groups = "drop")
  print(R95_resumo)
  write.csv(R95_resumo, file.path(output_dir, "prfd_R95_resumo_decadal.csv"), row.names = FALSE)
  write.csv(base_audit %>% filter(Ano < 2001) %>% count(QI_Spatial_R95_Level),
            file.path(output_dir, "prfd_R95_nivel_pre2001.csv"), row.names = FALSE)
  
  cl <- classificar_tiers(base_audit, PARAM_BASE)
  audit_table_prfd <- base_audit %>%
    mutate(PRFD_Overall_Quality_Tier = cl$tier,
           QI_Spatial_Class = paste("Class", cl$esp),
           QI_Area_Agreement_Class = cl$area)
  
  cat(">>> KPIs anuais e decadais...\n")
  kpis_anuais   <- kpis_integridade(audit_table_prfd, PARAM_BASE, Ano)
  kpis_decadais <- kpis_integridade(audit_table_prfd, PARAM_BASE, Decada)
  
  cat(">>> Monte Carlo (N =", N_SIM, ")...\n")
  sim <- simular_sensibilidade(base_audit)
  sens_decadal <- resumir_sensibilidade(sim)
  sens_src     <- src_sensibilidade(sim)
  
  write.csv(audit_table_prfd,  file.path(output_dir, "FOGOS_audit_v2.csv"), row.names = FALSE)
  write.csv(kpis_anuais,       file.path(output_dir, "prfd_annual_integrity_kpis_v2.csv"), row.names = FALSE)
  write.csv(kpis_decadais,     file.path(output_dir, "prfd_decadal_integrity_kpis_v2.csv"), row.names = FALSE)
  write.csv(sens_decadal,     file.path(output_dir, "prfd_sensitivity_kpis_decadal_v2.csv"), row.names = FALSE)
  write.csv(sens_src,          file.path(output_dir, "prfd_sensitivity_SRC_v2.csv"), row.names = FALSE)
  write.csv(sim$params,        file.path(output_dir, "prfd_sensitivity_params_v2.csv"), row.names = FALSE)
  
  # REVIEW: the record-level quality fields are stored in the annex fogos_anexo
  # (as stated in the manuscript, Sect. 2.4 and Table "files"), and the audit table
  # is saved for 15_integrity_table.R
  saveRDS(audit_table_prfd, file.path(output_dir, "audit_table_prfd.rds"))
  qi_cols <- c("Codigo", "QI_Spatial_R95_m", "QI_Spatial_Class", "QI_Area_Agreement_Class",
               "QI_Temporal_Imputation_Level", "QI_MultiSource_Validation_Score",
               "Match_Low_Confidence", "PRFD_Overall_Quality_Tier")
  anexo_qi <- carregar_anexo()
  if (is.null(anexo_qi)) anexo_qi <- audit_table_prfd["Codigo"]
  fogos_anexo <- anexo_qi %>%
    select(-any_of(setdiff(qi_cols, "Codigo"))) %>%
    left_join(audit_table_prfd[, qi_cols], by = "Codigo")
  write.csv(fogos_anexo, file.path(output_dir, "fogos_anexo.csv"), row.names = FALSE, na = "")
  saveRDS(fogos_anexo, file.path(output_dir, "fogos_anexo.rds"))
  message("Quality fields added to fogos_anexo (", nrow(fogos_anexo), " records)")

  print(kpis_decadais %>% select(Decada, Total_Records, Tier_A_Pct_Area, Tier_B_Pct_Area, Tier_U_Pct_Area,
                                 Tier_C_Pct_Area, DB_Integrity_Index_Area, DB_Integrity_Index_Area_wU_eq_wB))
  print(sens_decadal)
  print(sens_src %>% arrange(Decada, desc(SRC2)))
} else {
  cat("Aviso: objecto 'fogos' não encontrado.\n")
}