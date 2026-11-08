# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# 62-Validacao_MICE_mascaramento_e_intervalos.R  --  Validation of the MICE
#                                                     imputation (MAR)
# =============================================================================
# SUMMARY (English)
#   Validates the imputation of script 4 (4-FireDataImputation.R):
#   A. Masking experiment: observed durations and burned areas are hidden and
#      re-imputed with PMM; errors against the true values by data structure
#      (t1-t5) and size class; naive reference = district x month median.
#      Records are hidden at random within each data structure (MAR), the
#      assumption of the imputation model.
#   B. Per-record range of the m imputations of the production run.
#   C. Convergence: Gelman-Rubin on the MICE chains (chainMean).
#   D. Where the real imputations occur (size class, data structure).
#   E. Diagnostics of the observed duration by data structure.
#   The preparation of the data follows script 4 exactly (flags of script 3,
#   end time corrected by script 3), so the experiment validates the same model.
#   Auxiliary variables are read from, and the per-record intervals written to,
#   the annex table fogos_anexo (linked by Codigo); the main table keeps only
#   AreaTotalEstimada, DHFimEstimado, DuracaoHorasEstimado, and HaHoraEstimado.
# =============================================================================
# ==============================================================================
# 62 (v3) - VALIDAÇÃO DA IMPUTAÇÃO MICE (4-FireDataImputation.R)
# ------------------------------------------------------------------------------
#  A. Experiência de mascaramento: esconde valores OBSERVADOS de duração e área,
#     reimputa-os (PMM) e mede o erro contra o valor verdadeiro, por estrutura de
#     dados (t1-t5) e classe de área. Referência ingénua: mediana distrito x mês.
#  B. Intervalo por registo (range das m imputações) a partir de 'imputacao'.
#  C. Convergência: Gelman-Rubin sobre as cadeias do MICE (chainMean).
#  D. Onde ocorrem as imputações REAIS (classe de área, estrutura).
#  E. Diagnóstico da duração observada por estrutura (anomalias, p.ex. t3).
#
# ALTERAÇÕES face à v1
#   1. KS corrigido: compara y com UMA imputação e com todas as imputações juntas
#      (a mediana das m imputações é mais concentrada que os valores verdadeiros,
#      logo o KS contra y_hat dá D alto por construção, mesmo com imputação correcta).
#   2. Configuração (m, maxit) em parâmetros; guarda-se no CSV de configuração.
#      O artigo (secção 2.1.4) diz 10 imputações x 100 iterações: para validar a
#      configuração de produção usar M_IMP <- 10; MAXIT <- 100.
#   3. Cenário opcional com Estrutura e Ano como preditores (COMPARAR_COM_ESTRUTURA).
#   4. Skill calculado no mesmo subconjunto que o ingénuo; IC exacto da cobertura;
#      cobertura esperada com m imputações permutáveis (referência de calibração).
#   5. Todos os ficheiros gravados em output_dir.
#   6. Intervalos no anexo 'fogos_anexo' (ADICIONAR_INTERVALOS_AO_ANEXO) e com
#      verificação de que 'imputacao' tem o mesmo nº de linhas que 'fogos'.
#
# Pré-requisitos: 'fogos' com Codigo, Distrito, Mes, Hora, fwi, AreaTotal, DHInicio,
# DHFim, Observacoes; para B, C(produção) e D o objecto 'imputacao' de 4.1.
# ==============================================================================

library(dplyr)
library(mice)
library(lubridate)
library(tibble)

if (!exists("output_dir")) output_dir <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
saida <- function(f) file.path(output_dir, f)

# ------------------------------------------------------------------------------
# 0. PARÂMETROS
# ------------------------------------------------------------------------------
P_MASK        <- 0.05     # fracção de casos completos a mascarar (por estrutura)
N_SUBAMOSTRA  <- 150000   # nº de linhas da experiência (NULL = todas)
M_IMP         <- 20       # nº de imputações (produção no artigo: 10)
MAXIT         <- 20       # iterações (produção no artigo: 100)
SEED          <- 1234
TIPOS_MASCARA <- c(dur = 0.45, area = 0.45, ambas = 0.10)   # proporção por tipo de máscara
COMPARAR_COM_ESTRUTURA       <- FALSE  # TRUE: repete com Estrutura e Ano como preditores
ADICIONAR_INTERVALOS_AO_ANEXO <- TRUE  # v3: intervalos vão para 'fogos_anexo' (FALSE: só CSV)

PRED_BASE   <- c("Distrito", "Mes", "Hora", "fwi")   # preditores de 4.1 (confirmar!)
VARS_ALVO   <- c("AreaTotalEstimado", "duration")
CLASSES_BA  <- c(0, 1, 10, 100, 500, Inf)
ROT_CLASSES <- c("<1", "1-10", "10-100", "100-500", ">=500")

estrutura_t <- function(ano) {
  case_when(ano <= 1992 ~ "t1 (1980-1992)",
            ano == 1993 ~ "t2 (1993)",
            ano <= 1995 ~ "t3 (1994-1995)",
            ano <= 2000 ~ "t4 (1996-2000)",
            TRUE        ~ "t5 (2001-2025)")
}

# cobertura esperada do intervalo p5-p95 de m imputações PERMUTÁVEIS (sem informação):
# quantis do tipo 7 -> posições 1+0.05(m-1) e 1+0.95(m-1)
cobertura_esperada <- function(m) 100 * (1 - 2 * (1 + 0.05 * (m - 1)) / (m + 1))

# ------------------------------------------------------------------------------
# 1. PREPARAÇÃO (mesmas regras de 4.1-AreaDHImputation.R)
# ------------------------------------------------------------------------------
fogos_df <- if (inherits(fogos, "sf")) sf::st_drop_geometry(fogos) else fogos
# v3: auxiliary variables (flags, mechanisms, DHFimRegras) come from the annex
if (exists("fogos_anexo")) fogos_df <- fogos_df %>%
  left_join(fogos_anexo[, c("Codigo", setdiff(names(fogos_anexo), names(fogos_df)))], by = "Codigo")

# v3: same rules as script 4 (flags of script 3; end time corrected by script 3).
# The old pattern "estimated" (case-insensitive) also matched the notes
# ";Area Estimated" / ";DHFim Estimated" written by the imputation itself.
obs_txt  <- coalesce(as.character(fogos_df$Observacoes), "")
flag_est <- if ("Flag_DHFim_Estimated" %in% names(fogos_df)) fogos_df$Flag_DHFim_Estimated %in% TRUE else
  grepl("DHFim estimated", obs_txt, fixed = TRUE)
flag_ano <- if ("Flag_DHFim_Anomaly" %in% names(fogos_df)) fogos_df$Flag_DHFim_Anomaly %in% TRUE else
  grepl("DHFim anomaly", obs_txt, fixed = TRUE)
# end time: original, corrected by the rules of script 3 (DHFimEstimado of script 3
# is overwritten by script 4; use the column of script 3 if it was kept)
dh_end <- if ("DHFimRegras" %in% names(fogos_df)) coalesce(as.POSIXct(fogos_df$DHFimRegras, tz = "UTC"),
                                                           as.POSIXct(fogos_df$DHFim, tz = "UTC")) else
                                                             as.POSIXct(fogos_df$DHFim, tz = "UTC")

base <- fogos_df %>%
  mutate(
    DHInicio  = as.POSIXct(DHInicio, tz = "UTC"),
    Ano       = year(DHInicio),
    Estrutura = estrutura_t(Ano),
    AreaTotalEstimado = ifelse(is.na(AreaTotal) | AreaTotal <= 0, NA, AreaTotal),
    duration  = as.numeric(difftime(dh_end, DHInicio, units = "mins")),
    duration  = ifelse(is.na(DHFim) | duration <= 0 | flag_est | (flag_ano & AreaTotal < 10) %in% TRUE,
                       NA, duration),
    Distrito  = as.factor(Distrito),
    Mes       = as.factor(Mes),
    Hora      = as.factor(Hora)
  ) %>%
  select(Codigo, Ano, Estrutura, Distrito, Mes, Hora, fwi, AreaTotalEstimado, duration)

# ------------------------------------------------------------------------------
# A. EXPERIÊNCIA DE MASCARAMENTO
# ------------------------------------------------------------------------------
set.seed(SEED)
exp_df <- if (!is.null(N_SUBAMOSTRA) && nrow(base) > N_SUBAMOSTRA) {
  base %>% group_by(Estrutura) %>%
    slice_sample(prop = N_SUBAMOSTRA / nrow(base)) %>% ungroup()
} else base

completos <- which(!is.na(exp_df$AreaTotalEstimado) & !is.na(exp_df$duration))

# o tipo de máscara é sorteado primeiro; os registos são escondidos ao acaso (MAR)
# dentro de cada estrutura de dados
cand <- exp_df[completos, ] %>%
  mutate(linha = completos,
         tipo  = sample(names(TIPOS_MASCARA), n(), replace = TRUE, prob = TIPOS_MASCARA))

preparar_mascara <- function() {
  set.seed(SEED)
  m <- cand %>% group_by(Estrutura) %>%
    slice_sample(prop = P_MASK) %>% ungroup()
  em <- exp_df
  em$duration[m$linha[m$tipo %in% c("dur", "ambas")]] <- NA
  em$AreaTotalEstimado[m$linha[m$tipo %in% c("area", "ambas")]] <- NA
  list(mascara = m, exp_mask = em, verdade = exp_df[m$linha, VARS_ALVO])
}

# referência ingénua (mediana distrito x mês dos valores observados), calculada uma vez
ingenuo <- function(var) {
  ref <- exp_mask %>% group_by(Distrito, Mes) %>%
    summarise(med = median(.data[[var]], na.rm = TRUE), .groups = "drop")
  exp_mask[mascara$linha, ] %>% left_join(ref, by = c("Distrito", "Mes")) %>% pull(med)
}
tipos_var <- list(duration = c("dur", "ambas"), AreaTotalEstimado = c("area", "ambas"))
nome_var  <- c(duration = "Duracao (min)", AreaTotalEstimado = "AreaTotal (ha)")

# matriz [registos mascarados x m] com os valores imputados
extrair_imputados <- function(imp, var, linhas) {
  mat <- as.matrix(imp$imp[[var]])
  mat[match(linhas, as.integer(rownames(mat))), , drop = FALSE]
}

rodar_cenario <- function(nome, preditores) {
  cat(">>> Cenário '", nome, "': ", nrow(mascara), " registos mascarados | m = ", M_IMP,
      " | maxit = ", MAXIT, " | preditores: ", paste(preditores, collapse = ", "), "\n", sep = "")
  dados <- exp_mask %>% select(all_of(c(preditores, VARS_ALVO))) %>%
    mutate(across(where(is.character), as.factor))
  imp <- mice(dados, method = "pmm", m = M_IMP, maxit = MAXIT, seed = SEED, printFlag = FALSE)
  
  res <- bind_rows(lapply(VARS_ALVO, function(var) {
    sel   <- mascara$tipo %in% tipos_var[[var]]
    mat   <- extrair_imputados(imp, var, mascara$linha[sel])
    tibble(Cenario = nome, Variavel = nome_var[[var]],
           Codigo = mascara$Codigo[sel], Estrutura = mascara$Estrutura[sel],
           Classe_BA = cut(verdade$AreaTotalEstimado[sel], CLASSES_BA, labels = ROT_CLASSES, right = FALSE),
           y = verdade[[var]][sel],
           y_hat = apply(mat, 1, median),
           y_1imp = mat[, 1],
           p05 = apply(mat, 1, quantile, 0.05, names = FALSE),
           p95 = apply(mat, 1, quantile, 0.95, names = FALSE),
           y_ingenuo = y_ingenuo[[var]][sel])
  }))
  list(imp = imp, res = res, mat = lapply(setNames(VARS_ALVO, VARS_ALVO), function(var) {
    sel <- mascara$tipo %in% tipos_var[[var]]; list(sel = sel, mat = extrair_imputados(imp, var, mascara$linha[sel]))
  }))
}

metricas <- function(d) {
  d %>% summarise(
    n = n(),
    MAE   = mean(abs(y_hat - y)),
    MedAE = median(abs(y_hat - y)),
    Vies  = mean(y_hat - y),                      # imputado - observado
    Vies_mediana_erro = median(y_hat - y),        # mediana dos erros
    Dif_medianas      = median(y_hat) - median(y),# (v1: 'Vies_mediana')
    RMSE_log          = sqrt(mean((log1p(y_hat) - log1p(y))^2)),   # log(1 + x)
    n_com_ingenuo     = sum(!is.na(y_ingenuo)),
    RMSE_log_mesmo_subconj = sqrt(mean(((log1p(y_hat) - log1p(y))^2)[!is.na(y_ingenuo)])),
    RMSE_log_ingenuo       = sqrt(mean(((log1p(y_ingenuo) - log1p(y))^2)[!is.na(y_ingenuo)])),
    Skill_vs_ingenuo       = 1 - RMSE_log_mesmo_subconj / RMSE_log_ingenuo,
    Cobertura_p5_p95 = 100 * mean(y >= p05 & y <= p95),
    Cob_IC95_inf = 100 * binom.test(sum(y >= p05 & y <= p95), n())$conf.int[1],
    Cob_IC95_sup = 100 * binom.test(sum(y >= p05 & y <= p95), n())$conf.int[2],
    Cobertura_esperada_permutavel = cobertura_esperada(M_IMP),
    Largura_mediana_p5_p95 = median(p95 - p05),
    .groups = "drop")
}

cenarios <- list(base = PRED_BASE)
if (COMPARAR_COM_ESTRUTURA) cenarios$com_estrutura <- c(PRED_BASE, "Estrutura", "Ano")

# o experimento corre uma vez, com mascaramento MAR
out_cen <- list(); res_list <- list()
pm <- preparar_mascara()
mascara <- pm$mascara; exp_mask <- pm$exp_mask; verdade <- pm$verdade
y_ingenuo <- setNames(lapply(VARS_ALVO, ingenuo), VARS_ALVO)
for (nm in names(cenarios)) {
  o <- rodar_cenario(nm, cenarios[[nm]])
  o$verdade <- verdade
  out_cen[[nm]] <- o; res_list[[nm]] <- o$res
}
resultados <- bind_rows(res_list)
imp_exp    <- out_cen[["base"]]$imp

met_total     <- resultados %>% group_by(Cenario, Variavel) %>% metricas()
met_estrutura <- resultados %>% group_by(Cenario, Variavel, Estrutura) %>% metricas()
met_classe    <- resultados %>% group_by(Cenario, Variavel, Classe_BA) %>% metricas()

# KS (descritivo, sem p-valores: os mesmos registos entram nas duas amostras).
# Compara y com UMA imputação e com todas as imputações juntas.
ks <- bind_rows(lapply(names(out_cen), function(nm) bind_rows(lapply(VARS_ALVO, function(var) {
  o <- out_cen[[nm]]$mat[[var]]; y <- out_cen[[nm]]$verdade[[var]][o$sel]
  tibble(Cenario = nm, Variavel = nome_var[[var]],
         KS_D_1_imputacao      = unname(suppressWarnings(ks.test(y, o$mat[, 1])$statistic)),
         KS_D_todas_imputacoes = unname(suppressWarnings(ks.test(y, as.vector(o$mat))$statistic)),
         KS_D_mediana_m        = unname(suppressWarnings(ks.test(y, apply(o$mat, 1, median))$statistic)))
}))))

config <- tibble(P_MASK, N_SUBAMOSTRA, M_IMP, MAXIT, SEED,
                 n_exp = nrow(exp_df), n_completos = length(completos), n_mascarados = nrow(mascara),
                 n_mask_area = sum(mascara$tipo %in% c("area", "ambas")),
                 n_mask_dur  = sum(mascara$tipo %in% c("dur", "ambas")),
                 preditores_base = paste(PRED_BASE, collapse = "+"),
                 cobertura_esperada_permutavel = cobertura_esperada(M_IMP))

write.csv(config,        saida("mice_mask_config.csv"), row.names = FALSE)
write.csv(met_total,     saida("mice_mask_metricas_total.csv"), row.names = FALSE)
write.csv(met_estrutura, saida("mice_mask_metricas_estrutura.csv"), row.names = FALSE)
write.csv(met_classe,    saida("mice_mask_metricas_classeBA.csv"), row.names = FALSE)
write.csv(ks,            saida("mice_mask_ks.csv"), row.names = FALSE)
write.csv(resultados,    saida("mice_mask_resultados_por_registo.csv"), row.names = FALSE)
print(config); print(met_total %>% select(Cenario, Variavel, n, MedAE, Vies, RMSE_log, Skill_vs_ingenuo,
                                          Cobertura_p5_p95, Cobertura_esperada_permutavel))
print(ks)

# ------------------------------------------------------------------------------
# B. INTERVALO (RANGE DAS m IMPUTAÇÕES) POR REGISTO, A PARTIR DE 'imputacao'
# ------------------------------------------------------------------------------
# Nota: p05-p95 das m imputações NÃO é um intervalo de confiança de 90 %: a
# cobertura medida em A é ~ a de imputações permutáveis (ver 'Cobertura_esperada')
# e cai para <40 % acima de 10 ha. Descrever como "range of imputations".
intervalos_por_registo <- function(imp, var) {
  mat <- as.matrix(imp$imp[[var]])
  if (nrow(mat) == 0) return(NULL)
  tibble(linha = as.integer(rownames(mat)),
         p05 = apply(mat, 1, quantile, 0.05, names = FALSE),
         p50 = apply(mat, 1, median),
         p95 = apply(mat, 1, quantile, 0.95, names = FALSE),
         m   = ncol(mat))
}

if (exists("imputacao")) {
  if (nrow(imputacao$data) != nrow(fogos))
    stop("'imputacao' tem ", nrow(imputacao$data), " linhas e 'fogos' tem ", nrow(fogos),
         ": a correspondência por posição não é válida.")
  ia <- intervalos_por_registo(imputacao, "AreaTotalEstimado")
  id <- intervalos_por_registo(imputacao, "duration")
  
  n <- nrow(fogos)
  iv <- tibble(Codigo = fogos$Codigo,
               AreaTotalEstimada_p05 = NA_real_, AreaTotalEstimada_p50 = NA_real_, AreaTotalEstimada_p95 = NA_real_,
               DuracaoHorasEstimado_p05 = NA_real_, DuracaoHorasEstimado_p50 = NA_real_, DuracaoHorasEstimado_p95 = NA_real_,
               N_Imputacoes = 0L)
  if (!is.null(ia)) { iv$AreaTotalEstimada_p05[ia$linha] <- ia$p05; iv$AreaTotalEstimada_p50[ia$linha] <- ia$p50
  iv$AreaTotalEstimada_p95[ia$linha] <- ia$p95; iv$N_Imputacoes[ia$linha] <- ia$m }
  if (!is.null(id)) { iv$DuracaoHorasEstimado_p05[id$linha] <- id$p05 / 60; iv$DuracaoHorasEstimado_p50[id$linha] <- id$p50 / 60
  iv$DuracaoHorasEstimado_p95[id$linha] <- id$p95 / 60; iv$N_Imputacoes[id$linha] <- id$m }
  write.csv(iv, saida("mice_intervalos_por_registo.csv"), row.names = FALSE)
  write.csv(tibble(campo = c("*_p05", "*_p50", "*_p95", "N_Imputacoes"),
                   descricao = c("5.º percentil das m imputações (range, não IC)", "mediana das m imputações",
                                 "95.º percentil das m imputações (range, não IC)", "nº de imputações m")),
            saida("mice_intervalos_dicionario.csv"), row.names = FALSE)
  
  # v3: os intervalos são variáveis auxiliares -> anexo (não alteram 'fogos')
  if (ADICIONAR_INTERVALOS_AO_ANEXO) {
    if (!exists("fogos_anexo")) fogos_anexo <- tibble(Codigo = fogos$Codigo)
    fogos_anexo <- fogos_anexo %>% select(-any_of(setdiff(names(iv), "Codigo"))) %>%
      left_join(iv, by = "Codigo")
    write.csv(fogos_anexo, saida("fogos_anexo.csv"), row.names = FALSE, na = "")
    saveRDS(fogos_anexo, saida("fogos_anexo.rds"))
    cat("✔ Intervalos adicionados a 'fogos_anexo':", sum(iv$N_Imputacoes > 0), "registos imputados\n")
  } else cat("Intervalos gravados em CSV (anexo não alterado):", sum(iv$N_Imputacoes > 0), "registos imputados\n")
  
  # ----------------------------------------------------------------------------
  # D. ONDE OCORREM AS IMPUTAÇÕES REAIS
  # ----------------------------------------------------------------------------
  real <- tibble(Codigo = fogos$Codigo, Ano = year(as.POSIXct(fogos_df$DHInicio, tz = "UTC"))) %>%
    mutate(Estrutura = estrutura_t(Ano)) %>% bind_cols(iv %>% select(-Codigo))
  d_area <- real %>% filter(!is.na(AreaTotalEstimada_p50)) %>%
    mutate(Classe = cut(AreaTotalEstimada_p50, CLASSES_BA, labels = ROT_CLASSES, right = FALSE)) %>%
    count(Classe) %>% mutate(Variavel = "AreaTotal (ha)", pct = round(100 * n / sum(n), 1))
  d_dur  <- real %>% filter(!is.na(DuracaoHorasEstimado_p50)) %>% count(Estrutura) %>%
    mutate(Variavel = "Duracao (min)", pct = round(100 * n / sum(n), 1)) %>% rename(Classe = Estrutura)
  d_real <- bind_rows(d_area, d_dur)
  write.csv(d_real, saida("mice_imputacoes_reais_distribuicao.csv"), row.names = FALSE)
  print(d_real)
}

# ------------------------------------------------------------------------------
# C. CONVERGÊNCIA: GELMAN-RUBIN SOBRE AS CADEIAS DO MICE
# ------------------------------------------------------------------------------
# chainMean: [variável, iteração, cadeia]; cada imputação é uma cadeia. R-hat na 2.ª
# metade das iterações. (O R-hat calculado sobre as linhas dos dados completados
# NÃO mede convergência.)
rhat_mice <- function(imp) {
  cm <- imp$chainMean
  vars <- dimnames(cm)[[1]][apply(cm, 1, function(x) all(is.finite(x)))]
  it <- dim(cm)[2]; keep <- (floor(it / 2) + 1):it
  sapply(vars, function(v) {
    x <- matrix(cm[v, keep, ], nrow = length(keep))   # iterações x cadeias
    n <- nrow(x)
    W <- mean(apply(x, 2, var))
    B <- n * var(colMeans(x))
    sqrt(((n - 1) / n * W + B / n) / W)
  })
}

for (par in list(list(nome = "experiencia", obj = imp_exp),
                 if (exists("imputacao")) list(nome = "producao", obj = imputacao))) {
  if (is.null(par)) next
  rhat <- rhat_mice(par$obj)
  cat("R-hat (", par$nome, "): ", paste(names(rhat), round(rhat, 3), collapse = " | "), "\n", sep = "")
  write.csv(data.frame(Variavel = names(rhat), Rhat = round(rhat, 4), maxit = par$obj$iteration, m = par$obj$m),
            saida(paste0("mice_rhat_cadeias_", par$nome, ".csv")), row.names = FALSE)
  png(saida(paste0("mice_traceplot_", par$nome, ".png")), width = 1600, height = 900, res = 130)
  print(plot(par$obj, VARS_ALVO))
  dev.off()
}

# ------------------------------------------------------------------------------
# E. DIAGNÓSTICO DA DURAÇÃO OBSERVADA POR ESTRUTURA (p.ex. viés em t3)
# ------------------------------------------------------------------------------
diag_dur <- base %>% filter(!is.na(duration)) %>% group_by(Estrutura) %>%
  summarise(n = n(), mediana_min = median(duration), p90_min = quantile(duration, 0.9),
            pct_multiplo_60 = round(100 * mean(duration %% 60 == 0), 1),
            pct_ge_720 = round(100 * mean(duration >= 720), 1), .groups = "drop")
write.csv(diag_dur, saida("mice_diag_duracao_por_estrutura.csv"), row.names = FALSE)
print(diag_dur)