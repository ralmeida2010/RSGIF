# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 09 - former 63-Validacao_externa_e_associacao_PBA.R
# =============================================================================
# 09_PBA_linkage_validation.R  --  Validation of the PBA-PRFD linkage and annual
#                                  totals (Tables 5-7, Sect. 3.1.4)
# =============================================================================
# SUMMARY (English)
#   Uses the linkage produced by 08_PBA_linkage.R (BA_Final: one row per PBA
#   polygon with the estimated fire, method and score) and does NOT repeat it.
#   PART 1  Load BA_Final: from memory (BA_Final or resultados_totais) or from
#           output/checkpoints/BA_Final.rds or output/pba_linkage/BA_Final.gpkg.
#           Optional test of variants of the search function (MODO_B / UMBRAL),
#           on a few years, to compare with the accuracy of the manuscript.
#   PART 2  Quality of the linkage against the polygons with a manual Cod_SGIF:
#           coverage (a fire was proposed), precision (correct / proposed) and
#           recall (correct / manual = the "accuracy" of the manuscript), in
#           total, by method (Table 5), by area class (Table 6, same classes as
#           the manuscript), by period, by score class and by year (>= 10 ha);
#           logistic model of the probability of a correct link (BAMatchProb),
#           with 5-fold cross-validation and calibration.
#   PART 3  Annual burned area of the PRFD vs a reference: external file
#           (data/referencia_anual_EFFIS_ICNF.csv) or the PBA polygons >= 10 ha
#           (Table 7: only the AREA is comparable, the PBA counts polygons).
#   All results are written as CSV to output/pba_linkage/ (list printed at
#   the end).
#
# REVIEW NOTES
#   - Removed the repetition of script 08: the preparation of the polygons, the
#     PAFA and FRP windows and the association are no longer done here (they
#     took hours and gave a second, possibly different, BA_Final). If BA_Final
#     is not found, the script stops and asks to run 08.
#   - The search function is no longer copied here: 08a_PBA_search_function.R
#     is the only definition (used only by the optional variant test).
#   - The cache of 08 (output/checkpoints/BA_Final.rds) was not the file read
#     here (output/BA_Final.rds), so the association was always recomputed.
#   - The area classes follow Table 6 (<1, 1-10, 10-50, 50-100, 100-500,
#     500-1000, >=1000 ha) instead of grouping all polygons below 10 ha.
#   - BAMatchProb is an auxiliary variable: when propagated, it goes to
#     fogos_anexo, not to fogos.
#   - Folders from 00_Config_paths.R (drive V: or C:).
# =============================================================================

select <- dplyr::select; filter <- dplyr::filter; mutate <- dplyr::mutate
summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count
suppressPackageStartupMessages({ library(sf); library(dplyr); library(purrr); library(lubridate); library(tibble) })

# ------------------------------------------------------------------------------
# PART 0. CONFIGURATION
# ------------------------------------------------------------------------------
cfg <- function(nome, valor) if (!exists(nome)) assign(nome, valor, envir = .GlobalEnv)
.proj <- c(Sys.getenv("RSGIF_DIR", getwd()), Sys.getenv("RSGIF_DIR", getwd()))
cfg("project_dir", .proj[dir.exists(.proj)][1])
cfg("output_dir",  file.path(project_dir, "output"))
cfg("data_dir",    file.path(project_dir, "data"))
cfg("scripts_dir", file.path(project_dir, "R"))

cfg("EXECUTAR_TESTE_VARIANTES", FALSE)  # TRUE: compares variants of the search function and STOPS
cfg("ANOS_TESTE", 2010:2012)            # years of the variant test
cfg("PROPAGAR_PARA_ANEXO", FALSE)       # TRUE: adds BAMatchProb / BA_N_Poligonos to fogos_anexo
cfg("LIMIAR_HA_REF", 10)                # minimum area (ha) of the PBA reference and of the PRFD (part 3)

pba_dir <- file.path(output_dir, "pba_linkage"); dir.create(pba_dir, recursive = TRUE, showWarnings = FALSE)
gravados <- character(0)
gravar <- function(x, f) {
  write.csv(x, file.path(pba_dir, f), row.names = FALSE, na = "")
  gravados <<- c(gravados, f); cat("   saved:", f, "\n")
}
manual_ok <- function(x) !is.na(x) & !as.character(x) %in% c("0", "", " ")

# ------------------------------------------------------------------------------
# PART 1. BA_Final (from script 08) and optional variant test
# ------------------------------------------------------------------------------
carregar_BA_Final <- function() {
  for (obj in c("BA_Final", "resultados_totais"))
    if (exists(obj, envir = .GlobalEnv)) {
      x <- get(obj, envir = .GlobalEnv)
      if (all(c("CODIGOSGIF_ESTIMADO", "SCORE_ESTIMADO", "Cod_SGIF") %in% names(x))) {
        cat(">>> BA_Final from '", obj, "' in memory\n", sep = ""); return(x) }
    }
  f <- c(file.path(output_dir, "checkpoints", "BA_Final.rds"), file.path(pba_dir, "BA_Final.gpkg"),
         file.path(output_dir, "BA_Final.rds"))
  f <- f[file.exists(f)][1]
  if (is.na(f)) stop("BA_Final not found (memory, checkpoints/BA_Final.rds, pba_linkage/BA_Final.gpkg). ",
                     "Run 08_PBA_linkage.R first.", call. = FALSE)
  cat(">>> BA_Final read from", f, "\n")
  if (grepl("\\.gpkg$", f)) st_read(f, quiet = TRUE) else readRDS(f)
}

# --- optional: test of variants of the search function (needs BA_sf with the windows) ---
testar_variantes <- function(anos = ANOS_TESTE,
                             variantes = list(raio_06 = list(modo = "raio", umbral = 0.6),
                                              raio_04 = list(modo = "raio", umbral = 0.4),
                                              max     = list(modo = "max",  umbral = 0.6))) {
  ba <- if (exists("BA_sf")) BA_sf else {
    f <- file.path(output_dir, "checkpoints", "BA_sf_com_datas.rds")
    if (!file.exists(f)) stop("BA_sf (polygons with PAFA/FRP windows) not found: run 08 first.")
    readRDS(f) }
  ba <- ba[ba$Ano %in% anos, ]
  src  <- if (exists("fogos_data")) fogos_data else fogos
  cand <- if (inherits(src, "sf")) st_drop_geometry(src) else as.data.frame(src)
  cand <- cand[cand$Ano %in% anos & !is.na(cand$x_3763) & !is.na(cand$y_3763), ]
  cand$X_cent <- cand$x_3763; cand$Y_cent <- cand$y_3763
  out <- lapply(names(variantes), function(v) {
    assign("MODO_B", variantes[[v]]$modo, envir = .GlobalEnv)
    assign("UMBRAL", variantes[[v]]$umbral, envir = .GlobalEnv)
    source(file.path(scripts_dir, "08a_pba_search_function.R"))   # the only definition
    cat("\n##### Variant:", v, "#####\n")
    res <- map_dfr(split(ba, ba$Ano), function(df) {
      cy <- cand[cand$Ano == df$Ano[1], ]; usados <- character(0)
      df$CODIGOSGIF_ESTIMADO <- NA_character_; df$METODO_ESTIMATIVA <- NA_character_
      df$SCORE_ESTIMADO <- NA_real_
      a <- df$Ano[1]; ini <- as.POSIXct(paste0(a, "-01-01"), tz = "UTC"); fim <- as.POSIXct(paste0(a, "-12-31"), tz = "UTC")
      for (i in seq_len(nrow(df))) {
        b <- st_bbox(df[i, ]); viz <- cy[cy$X_cent >= b["xmin"] - 15000 & cy$X_cent <= b["xmax"] + 15000 &
                                           cy$Y_cent >= b["ymin"] - 15000 & cy$Y_cent <= b["ymax"] + 15000, ]
        if (!nrow(viz)) next
        r <- df[i, ]
        res <- executar_busca_exclusiva(r, viz, ini, fim, seq(2000, 15000, 3000), "BLIND", usados)
        if ((is.null(res) || res$sc > 0.6) && !is.na(r$d_pafa_min)) {
          r2 <- executar_busca_exclusiva(r, viz, r$d_pafa_min - days(5), r$d_pafa_max + days(5),
                                         seq(1000, 8000, 2000), "PAFA", usados); if (!is.null(r2)) res <- r2 }
        if ((is.null(res) || res$sc > 0.8) && !is.na(r$d_frp_min)) {
          r3 <- executar_busca_exclusiva(r, viz, r$d_frp_min, r$d_frp_max,
                                         seq(500, 5000, 1500), "FRP", usados); if (!is.null(r3)) res <- r3 }
        if (!is.null(res)) { df$CODIGOSGIF_ESTIMADO[i] <- res$cod; df$METODO_ESTIMATIVA[i] <- res$met
        df$SCORE_ESTIMADO[i] <- res$sc; usados <- c(usados, res$cod) }
      }
      df })
    d <- st_drop_geometry(res); d <- d[manual_ok(d$Cod_SGIF), ]
    d$acerto <- !is.na(d$CODIGOSGIF_ESTIMADO) & as.character(d$Cod_SGIF) == as.character(d$CODIGOSGIF_ESTIMADO)
    met <- coalesce(d$METODO_ESTIMATIVA, "(no link)")
    bind_rows(tibble(Variante = v, Metodo = "GLOBAL", n = nrow(d), pct = round(100 * mean(d$acerto), 1)),
              tibble(Variante = v, Metodo = met, acerto = d$acerto) %>% group_by(Variante, Metodo) %>%
                summarise(n = n(), pct = round(100 * mean(acerto), 1), .groups = "drop"))
  })
  out <- bind_rows(out)
  cat("\nManuscript: GLOBAL 76.4 | BLIND 83.9 | PAFA 66.9 | FRP 20.9\n"); print(out, n = Inf)
  gravar(out, "teste_variantes_funcao_procura.csv")
  invisible(out)
}

if (EXECUTAR_TESTE_VARIANTES) {
  testar_variantes()
  stop("Variant test finished (teste_variantes_funcao_procura.csv). Set MODO_B / UMBRAL, ",
       "rerun 08, and set EXECUTAR_TESTE_VARIANTES <- FALSE.", call. = FALSE)
}

BA_Final <- carregar_BA_Final()
cat(">>> BA_Final:", nrow(BA_Final), "polygons |", sum(!is.na(BA_Final$CODIGOSGIF_ESTIMADO)), "linked |",
    sum(manual_ok(BA_Final$Cod_SGIF)), "with a manual code\n")

# ==============================================================================
# PART 2. QUALITY OF THE PBA-PRFD LINKAGE
# ==============================================================================
classe_area <- function(a) cut(a, c(0, 1, 10, 50, 100, 500, 1000, Inf), right = FALSE,
                               labels = c("<1", "1-10", "10-50", "50-100", "100-500", "500-1000", ">=1000"))
periodo_ba  <- function(ano) case_when(ano < 2001 ~ "1980-2000", ano < 2010 ~ "2001-2009",
                                       ano < 2017 ~ "2010-2016", TRUE ~ "2017-2025")

assoc <- (if (inherits(BA_Final, "sf")) st_drop_geometry(BA_Final) else BA_Final) %>%
  mutate(Manual   = manual_ok(Cod_SGIF),
         Proposto = !is.na(CODIGOSGIF_ESTIMADO),
         Correcto = Manual & Proposto & as.character(Cod_SGIF) == as.character(CODIGOSGIF_ESTIMADO),
         Metodo   = coalesce(METODO_ESTIMATIVA, "Sem associação"),
         Classe_Area  = classe_area(AreaHaPoly),
         Periodo      = periodo_ba(Ano),
         Classe_Score = cut(SCORE_ESTIMADO, c(-Inf, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5, Inf)))

metricas <- function(d) d %>% filter(Manual) %>% summarise(
  N_manual = n(), N_proposto = sum(Proposto), Correctos = sum(Correcto),
  Cobertura_pct = round(100 * N_proposto / N_manual, 1),            # a fire was proposed
  Precisao_pct  = round(100 * Correctos / pmax(N_proposto, 1), 1),  # correct / proposed
  Recall_pct    = round(100 * Correctos / N_manual, 1),             # correct / manual (= accuracy of the manuscript)
  Score_medio   = round(mean(SCORE_ESTIMADO, na.rm = TRUE), 3), .groups = "drop")

m_total   <- metricas(assoc)
m_metodo  <- assoc %>% group_by(Metodo) %>% metricas()
m_area    <- assoc %>% group_by(Classe_Area) %>% metricas()
m_periodo <- assoc %>% group_by(Periodo, Metodo) %>% metricas()
m_score   <- assoc %>% filter(Proposto) %>% group_by(Classe_Score) %>% metricas()
m_ano10   <- assoc %>% filter(AreaHaPoly >= 10) %>% group_by(Ano) %>% metricas()

cat("\n>>> Linkage quality (Recall_pct = accuracy of the manuscript)\n")
print(m_total); print(m_metodo); print(m_area)
gravar(m_total,   "validacao_total.csv")
gravar(m_metodo,  "validacao_por_metodo.csv")              # Table 5
gravar(m_area,    "validacao_por_classe_area.csv")         # Table 6
gravar(m_periodo, "validacao_por_periodo_metodo.csv")
gravar(m_score,   "validacao_por_classe_score.csv")
gravar(m_ano10,   "validacao_por_ano_10ha.csv")             # accuracy over time (Sect. 3.1.4)

# --- probability of a correct link (logistic model on the manual polygons) -------
treino <- assoc %>% filter(Manual, Proposto, !is.na(SCORE_ESTIMADO), AreaHaPoly > 0)
termos <- c("SCORE_ESTIMADO", "log(AreaHaPoly)")
if (n_distinct(treino$Metodo)  > 1) termos <- c(termos, "Metodo")
if (n_distinct(treino$Periodo) > 1) termos <- c(termos, "Periodo")
form <- as.formula(paste("Correcto ~", paste(termos, collapse = " + ")))
assoc$BAMatchProb <- NA_real_
if (nrow(treino) >= 30 && n_distinct(treino$Correcto) == 2) {
  mod_prob <- glm(form, family = binomial, data = treino)
  ok <- assoc$Proposto & !is.na(assoc$SCORE_ESTIMADO) & assoc$AreaHaPoly > 0 &
    assoc$Metodo %in% unique(treino$Metodo) & assoc$Periodo %in% unique(treino$Periodo)
  assoc$BAMatchProb[ok] <- predict(mod_prob, newdata = assoc[ok, ], type = "response")
  set.seed(42)
  treino$fold <- sample(rep(1:5, length.out = nrow(treino)))
  cv <- bind_rows(lapply(1:5, function(k) {
    m <- glm(form, family = binomial, data = treino[treino$fold != k, ]); te <- treino[treino$fold == k, ]
    tibble(p = predict(m, newdata = te, type = "response"), y = te$Correcto) }))
  calib <- cv %>% mutate(Classe_p = cut(p, seq(0, 1, 0.1), include.lowest = TRUE)) %>% group_by(Classe_p) %>%
    summarise(n = n(), p_prevista = round(mean(p), 3), acerto_obs = round(mean(y), 3), .groups = "drop")
  gravar(calib, "probabilidade_calibracao_cv.csv")
  gravar(tibble(termo = names(coef(mod_prob)), coef = round(coef(mod_prob), 4),
                brier_cv = round(mean((cv$p - cv$y)^2), 4)), "probabilidade_modelo.csv")
  cat("Brier score (5-fold CV):", round(mean((cv$p - cv$y)^2), 4), "\n")
} else cat("Warning: too few manual polygons with a proposal for the logistic model (n =", nrow(treino), ")\n")

gravar(assoc %>% select(any_of(c("OBJECTID", "temp_id")), Ano, AreaHaPoly, Cod_SGIF, CODIGOSGIF_ESTIMADO,
                        Metodo, SCORE_ESTIMADO, Correcto, BAMatchProb), "probabilidade_por_poligono.csv")

# probability per fire: manual = 1; estimated = lowest of the linked polygons
prob_est <- assoc %>% filter(!is.na(BAMatchProb)) %>% group_by(Codigo = as.character(CODIGOSGIF_ESTIMADO)) %>%
  summarise(BAMatchProb = min(BAMatchProb), BA_N_Poligonos = n(), .groups = "drop")
cod_manual <- assoc %>% filter(Manual) %>% transmute(Codigo = as.character(Cod_SGIF)) %>% distinct()
prob_fogo <- bind_rows(cod_manual %>% mutate(BAMatchProb = 1, BA_N_Poligonos = NA_integer_, Origem = "manual"),
                       prob_est %>% anti_join(cod_manual, by = "Codigo") %>% mutate(Origem = "estimada"))
gravar(prob_fogo, "probabilidade_por_fogo.csv")

if (PROPAGAR_PARA_ANEXO && exists("fogos")) {                # auxiliary variable -> annex
  if (!exists("fogos_anexo")) fogos_anexo <- data.frame(Codigo = fogos$Codigo)
  m <- match(as.character(fogos_anexo$Codigo), prob_fogo$Codigo)
  fogos_anexo$BAMatchProb <- prob_fogo$BAMatchProb[m]; fogos_anexo$BA_N_Poligonos <- prob_fogo$BA_N_Poligonos[m]
  cat(">>> BAMatchProb and BA_N_Poligonos added to fogos_anexo\n")
}

# ==============================================================================
# PART 3. ANNUAL TOTALS vs REFERENCE (Table 7)
# ==============================================================================
ref_file <- file.path(data_dir, "referencia_anual_EFFIS_ICNF.csv")
fogos_B  <- if (exists("fogos")) fogos else if (exists("fogos_data")) fogos_data else NULL
if (!is.null(fogos_B)) {
  fdf <- if (inherits(fogos_B, "sf")) st_drop_geometry(fogos_B) else as.data.frame(fogos_B)
  col_est <- intersect(c("AreaTotalEstimada", "AreaTotalEstimado"), names(fdf))[1]
  fdf$BA_est <- if (is.na(col_est)) fdf$AreaTotal else coalesce(fdf[[col_est]], fdf$AreaTotal)
  limiar_prfd <- 0
  if (file.exists(ref_file)) {
    ref <- read.csv(ref_file, stringsAsFactors = FALSE); cat(">>> External reference:", ref_file, "\n")
  } else {
    ref <- assoc %>% filter(Ano >= 1980, Ano <= 2025, !is.na(AreaHaPoly), AreaHaPoly >= LIMIAR_HA_REF) %>%
      group_by(Ano) %>% summarise(N_Ref = n(), BA_Ref = sum(AreaHaPoly), .groups = "drop") %>% mutate(Fonte = "PBA")
    limiar_prfd <- LIMIAR_HA_REF
    gravar(ref, "referencia_anual_PBA_derivada.csv")
    cat(">>> Reference derived from the PBA (>= ", LIMIAR_HA_REF, " ha): use only dBA_pct (N_Ref counts polygons)\n", sep = "")
  }
  anual <- fdf %>% mutate(Ano = year(as.POSIXct(DHInicio))) %>% filter(coalesce(AreaTotal, 0) >= limiar_prfd) %>%
    group_by(Ano) %>%
    summarise(N_PRFD = n(), BA_PRFD = sum(AreaTotal, na.rm = TRUE), BA_PRFD_est = sum(BA_est, na.rm = TRUE),
              .groups = "drop") %>%
    inner_join(ref, by = "Ano") %>%
    mutate(dN_pct = round(100 * (N_PRFD - N_Ref) / N_Ref, 2), dBA_pct = round(100 * (BA_PRFD - BA_Ref) / BA_Ref, 2),
           Decada = paste0(floor(Ano / 10) * 10, "-", pmin(floor(Ano / 10) * 10 + 9, 2025)))
  resumo <- bind_rows(
    anual %>% summarise(Periodo = "1980-2025", Anos = n(), r_BA = round(cor(BA_PRFD, BA_Ref), 4),
                        MAPE_BA = round(mean(abs(dBA_pct)), 2)),
    anual %>% filter(!Ano %in% 1984:1989) %>% summarise(Periodo = "sem 1984-1989", Anos = n(),
                                                        r_BA = round(cor(BA_PRFD, BA_Ref), 4), MAPE_BA = round(mean(abs(dBA_pct)), 2)),
    anual %>% group_by(Periodo = Decada) %>% summarise(Anos = n(), r_BA = round(cor(BA_PRFD, BA_Ref), 4),
                                                       MAPE_BA = round(mean(abs(dBA_pct)), 2), BA_PRFD = sum(BA_PRFD), BA_Ref = sum(BA_Ref),
                                                       .groups = "drop"))
  gravar(anual,  "validacao_anual_referencia.csv")
  gravar(resumo, "validacao_anual_resumo.csv")             # Table 7
  print(resumo)
} else cat("Warning: neither 'fogos' nor 'fogos_data' exist: part 3 skipped.\n")

cat("\n>>> ", length(gravados), " CSV files in ", pba_dir, ":\n   ", paste(gravados, collapse = "\n   "), "\n", sep = "")