# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 04 - former 4-FireDataImputation.R
# =============================================================================
# 4-FireDataImputation.R  --  Multiple imputation of fire duration and burned
#                             area (Step 3 of the PRFD pipeline)
#   Merges the former scripts 4-FireDataImputation.R and 4_1-AreaDHImputation.R
# =============================================================================
#
# PURPOSE
#   Imputes, with Multivariate Imputation by Chained Equations (MICE, predictive
#   mean matching), the fire durations and burned areas that are missing, zero,
#   or flagged by script 3, then rebuilds the estimated end time, writes the
#   imputation flags and notes, runs the convergence and distribution checks
#   reported in Sect. 3.1.3, and exports the table.
#
# INPUT
#   fogosClosed (preferred) or fogos, output of script 3, with the columns
#   DHInicio, DHFim, DHFimEstimado, AreaTotal, Distrito, Mes, Hora, fwi,
#   Observacoes, and (if available) Flag_DHFim_Estimated / Flag_DHFim_Anomaly.
#
# PROCESSING
#   1. Values to impute (set to NA before MICE):
#        burned area  AreaTotal missing or <= 0;
#        duration     end time missing or not after the start; or record marked
#                     "DHFim estimated" by script 3 (rules 1, 6, 7); or marked
#                     "DHFim anomaly" (rules 8, 9) and AreaTotal < 10 ha.
#      The duration is computed from the end time corrected by script 3
#      (DHFimEstimado, or DHFim when it is missing), so that the date
#      corrections of rules 2-4 are kept.
#   2. MICE: predictors district, month, hour, FWI, duration, and burned area;
#      predictive mean matching for the numeric variables; m = 10 imputations,
#      maxit = 100 iterations, seed 1234. Missing FWI values are imputed in the
#      same model. The value stored for each record is the median of the m
#      imputations (or, optionally, the first imputation). The imputation assumes
#      MAR; this is a limitation for duration anomalies and zero areas (see Methods).
#   3. Checks: Welch t-tests and variances of observed vs completed series
#      (Table 4); Gelman-Rubin potential scale reduction factor computed on the
#      chain means of each imputed variable over the second half of the
#      iterations (one chain per imputation).
#   4. Results in the main table, one estimated column per original variable,
#      (MAR imputation):
#        AreaTotal -> AreaTotalEstimada      DHFim    -> DHFimEstimado
#        duration  -> DuracaoHorasEstimado   area/hour -> HaHoraEstimado
#      Notes ";Area Estimated" and ";DHFim Estimated" in Observacoes.
#      Auxiliary variables (flags, rules of script 3, location
#      flag) go to the annex table fogos_anexo, linked by Codigo.
#   5. Consistency checks of the event times (intervention, resolution,
#      conclusion) against the start and estimated end: counts are printed.
#   6. Column selection, descriptive summary of every column, and export
#      (output/fogos.csv / .rds and output/fogos_anexo.csv / .rds).
#
# REVIEW NOTES (changes marked "# REVIEW:" in the code)
#   - The former script 4 repeated the INE correction and the administrative
#     join (now in scripts 2 and 3) and imputed with random forests (m = 5,
#     with Codigo, INE and DHInicio as predictors, and missing durations set to
#     0). Only the MICE/PMM imputation described in the manuscript is kept.
#   - The former 4_1 read `fogos` and computed the duration from the ORIGINAL
#     DHFim, so the corrections of script 3 were lost; it now reads fogosClosed
#     and uses DHFimEstimado.
#   - The search for "estimated" also matched the notes written by this script
#     (";Area Estimated", ";DHFim Estimated"); the flags of script 3 are used,
#     or exact, case-sensitive notes.
#   - MICE assumes MAR (no MNAR adjustment is applied; the assumption is discussed as a
#     limitation for anomalies and zero areas).
#   - Gelman-Rubin was computed with records as iterations; it is now computed
#     on the chain means (imputacao$chainMean), as stated in the Methods.
#   - Observacoes became NA for every record whose area or end time was missing;
#     the notes are now written without losing information.
#   - DH1Intervencao was converted to a date, losing its time; it is kept as
#     date-time (1900-01-01 placeholders set to NA).
#   - Column names: one estimated column per original variable
#     (AreaTotalEstimada, DHFimEstimado, DuracaoHorasEstimado, HaHoraEstimado);
#     auxiliary variables are written to the annex fogos_anexo.
#   - The final column selection listed DuracaoHorasEstimado twice, required
#     columns that may not exist, and dropped the new flags; any_of() is used.
#   - Progress messages: every step prints its start time and elapsed time;
#     MICE runs in blocks of iterations (mice.mids), with the percentage done
#     and the estimated time left. The results are identical to a single run.
#   - The validation check columns were created and removed without use; their
#     counts are now printed. The duplicated select() that stopped the script
#     at the end was removed.
# =============================================================================

# Guard against dplyr verbs masked by other packages (e.g. MASS::select, raster::select, plyr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename
mutate <- dplyr::mutate; summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count

suppressPackageStartupMessages({
  library(dplyr); library(lubridate); library(mice); library(purrr); library(tidyr)
})

# -----------------------------------------------------------------------------
# 0. CONFIGURATION
# -----------------------------------------------------------------------------
if (!exists("output_dir")) output_dir <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

mice_m      <- 10          # number of imputations
mice_maxit  <- 100         # iterations
mice_seed   <- 1234
store_value <- "median"    # "median" of the m imputations (as evaluated in the masking experiment) or "first"

progress_block <- 5         # MICE iterations between progress messages
use_parallel   <- FALSE     # TRUE: the m imputations run in parallel (mice::futuremice);
#   much faster, reproducible with the same seed, but not
#   identical to the sequential run; no per-iteration progress
n_cores        <- max(1, parallel::detectCores() - 1)
save_imputation <- TRUE     # saves the mice object (output/imputacao.rds) right after the run

# -----------------------------------------------------------------------------
# Progress helpers
# -----------------------------------------------------------------------------
.t_start <- Sys.time()
fmt_time <- function(sec) {
  sec <- as.numeric(sec)
  if (!is.finite(sec)) return("--")
  if (sec < 60) sprintf("%.0f s", sec) else if (sec < 3600) sprintf("%.1f min", sec / 60) else sprintf("%.1f h", sec / 3600)
}
step <- function(...) message(sprintf("\n[%s | %s] ", format(Sys.time(), "%H:%M:%S"),
                                      fmt_time(difftime(Sys.time(), .t_start, units = "secs"))), ...)
info <- function(...) message("      ", ...)

# MICE in blocks of iterations, with progress and estimated time left
# (mice.mids continues the same chains: identical result to a single mice() call)
mice_progress <- function(data, method, m, maxit, seed, post = NULL, label = "MICE", block = progress_block) {
  t0 <- Sys.time()
  args <- list(data = data, method = method, m = m, maxit = 0, seed = seed, printFlag = FALSE)
  if (!is.null(post)) args$post <- post
  imp <- do.call(mice, args)
  info(label, ": initial values ready (", m, " imputations x ", maxit, " iterations, ",
       format(nrow(data), big.mark = ","), " records)")
  done <- 0
  while (done < maxit) {
    k   <- min(block, maxit - done)
    imp <- mice.mids(imp, maxit = k, printFlag = FALSE)
    done <- done + k
    el  <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    eta <- el / done * (maxit - done)
    info(sprintf("%s: iteration %d/%d (%3.0f%%) | elapsed %s | left ~%s",
                 label, done, maxit, 100 * done / maxit, fmt_time(el), fmt_time(eta)))
  }
  imp
}

step("Script 4 - MICE imputation of duration and burned area: start")

# REVIEW: use the output of script 3
if (exists("fogosClosed")) { fogos <- fogosClosed; info("Input: fogosClosed (output of script 3)") } else
  info("Input: fogos (fogosClosed not found)")
info(format(nrow(fogos), big.mark = ","), " records, ", ncol(fogos), " columns")

# -----------------------------------------------------------------------------
# 1. VALUES TO IMPUTE
# -----------------------------------------------------------------------------
step("1/6 Selecting the values to impute")
has <- function(v) v %in% names(fogos)

# Flags of script 3 (logical columns, or exact notes as a fallback)
# REVIEW: exact and case-sensitive patterns, so that the notes written by this
# script (";Area Estimated", ";DHFim Estimated") are never matched
flag_est <- if (has("Flag_DHFim_Estimated")) fogos$Flag_DHFim_Estimated else
  grepl("DHFim estimated", coalesce(fogos$Observacoes, ""), fixed = TRUE)
flag_ano <- if (has("Flag_DHFim_Anomaly")) fogos$Flag_DHFim_Anomaly else
  grepl("DHFim anomaly", coalesce(fogos$Observacoes, ""), fixed = TRUE)
flag_est[is.na(flag_est)] <- FALSE; flag_ano[is.na(flag_ano)] <- FALSE

# REVIEW: end time corrected by script 3 when available
dh_end <- if (has("DHFimEstimado")) coalesce(as.POSIXct(fogos$DHFimEstimado), fogos$DHFim) else fogos$DHFim

dados <- tibble(
  Distrito          = as.factor(fogos$Distrito),
  Mes               = as.factor(fogos$Mes),
  Hora              = as.factor(fogos$Hora),
  fwi               = as.numeric(fogos$fwi),
  AreaTotalEstimado = ifelse(is.na(fogos$AreaTotal) | fogos$AreaTotal <= 0, NA, fogos$AreaTotal),
  duration          = as.numeric(difftime(dh_end, fogos$DHInicio, units = "mins"))
)
dados$duration[is.na(fogos$DHFim) | dados$duration <= 0 | flag_est |
                 (flag_ano & fogos$AreaTotal < 10) %in% TRUE] <- NA

cat("Values to impute:\n",
    " burned area (missing or <= 0):", sum(is.na(dados$AreaTotalEstimado)), "\n",
    " duration (missing, <= 0, estimated, or anomaly < 10 ha):", sum(is.na(dados$duration)), "\n",
    " FWI:", sum(is.na(dados$fwi)), "\n")

# -----------------------------------------------------------------------------
# 2. MICE (predictive mean matching)
# -----------------------------------------------------------------------------
# REVIEW: PMM for the numeric variables; categorical predictors keep the default
# method (only used if they have missing values).
step("2/6 Main MICE run")
meth <- make.method(dados)
meth[c("fwi", "AreaTotalEstimado", "duration")][meth[c("fwi", "AreaTotalEstimado", "duration")] != ""] <- "pmm"

n_cores <- min(n_cores, mice_m, parallel::detectCores() - 1)
if (use_parallel && n_cores < 2) { info("Parallel run not possible (", n_cores, " core): sequential run"); use_parallel <- FALSE }
if (use_parallel) {
  info("Parallel run: ", mice_m, " imputations on ", n_cores, " cores (no per-iteration progress)")
  t_par <- Sys.time()
  imputacao <- futuremice(dados, m = mice_m, maxit = mice_maxit, method = meth,
                          parallelseed = mice_seed, n.core = n_cores, packages = "mice")
  info("Parallel run finished in ", fmt_time(difftime(Sys.time(), t_par, units = "secs")))
} else {
  imputacao <- mice_progress(dados, meth, mice_m, mice_maxit, mice_seed, label = "Main run")
}
info("Stored value: ", if (store_value == "median") "median of the imputations" else "first imputation")
if (save_imputation) {                                   # do not lose days of computation
  saveRDS(imputacao, file.path(output_dir, "imputacao.rds"))
  info("mice object saved: ", file.path(output_dir, "imputacao.rds"))
}

if (store_value == "median") {
  long <- complete(imputacao, "long")
  dados_imputados <- long %>% group_by(.id) %>%
    summarise(AreaTotalEstimado = median(AreaTotalEstimado),
              duration = median(duration), fwi = median(fwi), .groups = "drop") %>%
    arrange(.id)
} else {
  dados_imputados <- complete(imputacao, 1)
}

# -----------------------------------------------------------------------------
# 3. CHECKS (Sect. 3.1.3, Table 4)
# -----------------------------------------------------------------------------
step("3/6 Checks: Welch t-tests and Gelman-Rubin")
print(t.test(dados$AreaTotalEstimado, dados_imputados$AreaTotalEstimado))
print(t.test(dados$duration,          dados_imputados$duration))
cat("Variance of burned area, observed vs completed:",
    var(dados$AreaTotalEstimado, na.rm = TRUE), var(dados_imputados$AreaTotalEstimado), "\n")

# REVIEW: Gelman-Rubin on the chain means over the second half of the iterations
cm <- imputacao$chainMean                       # variables x iterations x chains
it <- (dim(cm)[2] %/% 2 + 1):dim(cm)[2]
vars_imp <- intersect(c("AreaTotalEstimado", "duration", "fwi"), dimnames(cm)[[1]])
rhat <- sapply(vars_imp, function(v) {
  x <- cm[v, it, , drop = TRUE]; n <- nrow(x)
  W <- mean(apply(x, 2, var)); B <- n * var(colMeans(x))
  sqrt(((n - 1) / n * W + B / n) / W)
})
cat("Gelman-Rubin (chain means, second half of iterations):\n"); print(round(rhat, 3))
if (any(rhat > 1.1, na.rm = TRUE)) info("WARNING: Gelman-Rubin > 1.1 for ", paste(names(rhat)[rhat > 1.1], collapse = ", "),
                                        " - consider more iterations") else info("Convergence OK (all values <= 1.1)")

# -----------------------------------------------------------------------------
# 4. RESULTS, FLAGS, AND NOTES
# -----------------------------------------------------------------------------
step("4/6 Writing the estimated columns, flags, and notes")
# End time after the rules of script 3, before the imputation (used by script 62)
fogos$DHFimRegras          <- dh_end
fogos$AreaTotalEstimada    <- dados_imputados$AreaTotalEstimado
fogos$DuracaoHorasEstimado <- dados_imputados$duration / 60
fogos$DHFimEstimado        <- fogos$DHInicio + lubridate::duration(fogos$DuracaoHorasEstimado * 3600)
fogos$HaHoraEstimado       <- ifelse(fogos$DuracaoHorasEstimado > 0,
                                     fogos$AreaTotalEstimada / fogos$DuracaoHorasEstimado, NA)
# remove the intermediate columns of script 3 / older versions
fogos <- fogos[, setdiff(names(fogos), c("AreaTotalEstimado", "haHoraEstimado"))]

# REVIEW: flags computed without NA (records with missing values are imputed)
fogos$Flag_Area_Imputed  <- is.na(dados$AreaTotalEstimado)
fogos$Flag_DHFim_Imputed <- is.na(dados$duration)

obs <- coalesce(fogos$Observacoes, "")
obs <- ifelse(fogos$Flag_Area_Imputed  & !grepl("Area Estimated",  obs, fixed = TRUE), paste0(obs, ";Area Estimated"),  obs)
obs <- ifelse(fogos$Flag_DHFim_Imputed & !grepl("DHFim Estimated", obs, fixed = TRUE), paste0(obs, ";DHFim Estimated"), obs)
fogos$Observacoes <- sub("^;", "", obs)

info("Imputed: burned area ", format(sum(fogos$Flag_Area_Imputed), big.mark = ","),
     " | duration ", format(sum(fogos$Flag_DHFim_Imputed), big.mark = ","), " records")

# -----------------------------------------------------------------------------
# 5. CONSISTENCY OF THE EVENT TIMES
# -----------------------------------------------------------------------------
step("5/6 Consistency of the event times")
# REVIEW: DH1Intervencao kept as date-time (it was converted to a date)
if (has("DH1Intervencao")) {
  fogos$DH1Intervencao <- as.POSIXct(fogos$DH1Intervencao)
  fogos$DH1Intervencao[as.Date(fogos$DH1Intervencao) == as.Date("1900-01-01")] <- NA
}
between_ok <- function(t) is.na(t) | is.na(fogos$DHInicio) | is.na(fogos$DHFimEstimado) |
  (t >= fogos$DHInicio & t <= fogos$DHFimEstimado)
checks <- c(
  end_after_start          = sum(!(is.na(fogos$DHFimEstimado) | fogos$DHFimEstimado >= fogos$DHInicio), na.rm = TRUE),
  intervention_in_window   = if (has("DH1Intervencao")) sum(!between_ok(fogos$DH1Intervencao), na.rm = TRUE) else NA,
  resolution_in_window     = if (has("DHResolucao"))    sum(!between_ok(fogos$DHResolucao),    na.rm = TRUE) else NA,
  conclusion_in_window     = if (has("DHConclusao"))    sum(!between_ok(fogos$DHConclusao),    na.rm = TRUE) else NA)
cat("Records failing each time-consistency check:\n"); print(checks)

# -----------------------------------------------------------------------------
# 6. COLUMN SELECTION, SUMMARY, AND EXPORT
# -----------------------------------------------------------------------------
step("6/6 Column selection, summary, and export")
if (has("TipoIncendio") && !has("Tipo")) fogos <- rename(fogos, Tipo = TipoIncendio)

cols_keep <- c("Codigo", "CodigoSado", "Ano", "Dia", "Mes", "Hora", "Tipo", "DHInicio", "DH1Intervencao",
               "DHResolucao", "DHConclusao", "DHFim", "DHFimEstimado", "DuracaoHoras", "DuracaoHorasEstimado",
               "Tempo1Intervencao", "TempoResolucao", "TempoRescaldo",
               "NUTS2", "NUTS3", "Distrito", "Concelho", "Freguesia", "Freguesia2014", "Local", "INE",
               "DDCCFF2014", "QO", "x_20790", "y_20790", "x_3763", "y_3763", "Lat_4326", "Lon_4326",
               "AreaPov", "AreaMato", "AreaAgric", "AreaTotal", "AreaTotalEstimada", "HaHora", "HaHoraEstimado", "ClasseArea",
               "CodCausa", "TipoCausa", "GrupoCausa", "DescricaoCausa", "Reacendimento", "Reacendimento_IncendioPai",
               "OriginouReacendimento", "Reacendimentos", "FonteAlerta", "AltitudeMedia", "DecliveMedio",
               "HorasExposicaoMedia", "Rugosidade", "Perimetro", "APS", "ModFarsite", "AreaManchaModFarsite",
               "RCM", "DendidadeRV", "CosN5Variedade", "Perigosidade", "Dist_CBS_m", "CBS", "DensidadeResidentes",
               "DensidadeEdificios", "AreaTotalIncSimul5000", "NIncSimulDistrito", "NIncSimulConcelho",
               "NIncSimulDistrito90", "NIncSimulConcelho90", "NIncSimul500090", "DistIncSimul500090",
               "AreaTotalIncSimul500090", "EstadoRegisto", "Temperatura", "HumidadeRelativa", "VentoIntensidade",
               "VentoIntensidade_vetor", "VentoDirecao_vetor", "Precepitacao", "VentoDirecao", "fwi", "dsr",
               "isi", "dc", "dmc", "ffmc", "bui", "hFWI", "hFFMC", "hISI", "MaxFWIh_48h_PosExtincao",
               "MaxFFMCh_48h_PosExtincao", "MaxISIh_48h_PosExtincao", "MaxDC_48h_DiaPosExtincao",
               "MaxDMC_48h_PosExtincao", "MaxBUI_48h_PosExtincao", "NIncSimul5000", "DistIncSimul5000",
               "ClassificacaoRegisto", "Observacoes")

# REVIEW: auxiliary variables of scripts 3 and 4 -> annex table (linked by Codigo)
cols_anexo <- c("TipoEstimado", "DHFim_Rules", "DHFimRegras", "Flag_DHFim_Estimated", "Flag_DHFim_Anomaly",
                "Flag_Area_Imputed", "Flag_DHFim_Imputed", 
                "Outside_PT")
fogos_anexo <- fogos[, c("Codigo", intersect(cols_anexo, names(fogos)))]
if (inherits(fogos_anexo, "sf")) fogos_anexo <- sf::st_drop_geometry(fogos_anexo)

missing_cols <- setdiff(cols_keep, names(fogos))
if (length(missing_cols)) message("Columns not found (skipped): ", paste(missing_cols, collapse = ", "))
fogos <- fogos[, intersect(unique(cols_keep), names(fogos))]

# Descriptive summary of every column (summary() as a table)
fogos_df <- if (inherits(fogos, "sf")) sf::st_drop_geometry(fogos) else fogos
ts_fogos_df <- map_dfr(names(fogos_df), function(v) {
  s <- summary(fogos_df[[v]])
  tibble(Column = v, Attribute = names(s), Value = as.character(s))
}) %>% pivot_wider(names_from = Attribute, values_from = Value)

info("Writing fogos.csv / fogos.rds and fogos_anexo.csv / fogos_anexo.rds ...")
write.csv(fogos_df, file.path(output_dir, "fogos.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
saveRDS(fogos, file.path(output_dir, "fogos.rds"))
write.csv(fogos_anexo, file.path(output_dir, "fogos_anexo.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
saveRDS(fogos_anexo, file.path(output_dir, "fogos_anexo.rds"))
cat("✔ Exported:", nrow(fogos), "records to fogos.csv and", ncol(fogos_anexo) - 1,
    "auxiliary variables to fogos_anexo.csv\n")

step("Script 4 finished. Total time: ", fmt_time(difftime(Sys.time(), .t_start, units = "secs")))

rm(dados, dados_imputados, dh_end, flag_est, flag_ano, obs, cm, it, meth, between_ok, fogos_df, has,
   step, info, fmt_time, mice_progress, .t_start)