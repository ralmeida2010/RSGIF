# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# =============================================================================
# 54-Teste_t_de_Welch.R  --  Welch two-sample t-tests, observed vs completed
#                            series (Table 4)
# =============================================================================
# SUMMARY (English)
#   Compares the observed burned area and duration with the completed series
#   (observed + imputed) of script 4 with Welch's two-sample t-test (Table 4,
#   Sect. 3.1.3), and repeats the comparison for the imputed values alone
#   against the observed values.
#   Because the observed and completed series share most of their values, the
#   test only shows whether the imputation shifts the overall means; the error
#   of individual values is measured by the masking experiment (script 62).
#
# REVIEW NOTES
#   - The observed area excludes zero and missing values, as in script 4 (the
#     previous version compared AreaTotal including zeros, so its means did not
#     match the check printed by script 4).
#   - The duration is in hours (the previous labels said minutes, but the values
#     were multiplied by 1).
#   - Also tests the imputed values alone against the observed values.
#   - Results are saved in output/welch_tests.csv.
#   - Columns: AreaTotalEstimada / DuracaoHorasEstimado (main table); flags read
#     from the annex fogos_anexo.
# =============================================================================

library(dplyr)

if (!exists("output_dir")) output_dir <- file.path(Sys.getenv("RSGIF_DIR", getwd()), "output")

df <- if (inherits(fogos, "sf")) sf::st_drop_geometry(fogos) else fogos
# auxiliary variables (flags) are in the annex table
if (exists("fogos_anexo")) df <- df %>%
  left_join(fogos_anexo[, c("Codigo", setdiff(names(fogos_anexo), names(df)))], by = "Codigo")

area_obs <- ifelse(is.na(df$AreaTotal) | df$AreaTotal <= 0, NA, df$AreaTotal)
dur_obs  <- ifelse(df$Flag_DHFim_Imputed %in% TRUE, NA, df$DuracaoHorasEstimado)   # observed durations (h)

welch <- function(x, y, variavel, comparacao) {
  tt <- t.test(x, y, alternative = "two.sided", var.equal = FALSE)
  tibble(Variable = variavel, Comparison = comparacao,
         n_x = sum(!is.na(x)), n_y = sum(!is.na(y)),
         Mean_x = unname(tt$estimate[1]), Mean_y = unname(tt$estimate[2]),
         t_value = unname(tt$statistic), df = unname(tt$parameter), p_value = tt$p.value,
         CI95_low = tt$conf.int[1], CI95_high = tt$conf.int[2])
}

res <- list(
  # Table 4: observed vs completed series
  welch(area_obs, df$AreaTotalEstimada,   "Burned area (ha)", "Observed vs completed"),
  welch(dur_obs,  df$DuracaoHorasEstimado, "Duration (h)",     "Observed vs completed"))

# Imputed values only vs observed values
ia <- df$Flag_Area_Imputed  %in% TRUE
id <- df$Flag_DHFim_Imputed %in% TRUE
if (sum(ia) > 1) res[[length(res) + 1]] <- welch(area_obs, df$AreaTotalEstimada[ia],
                                                 "Burned area (ha)", "Observed vs imputed")
if (sum(id) > 1) res[[length(res) + 1]] <- welch(dur_obs, df$DuracaoHorasEstimado[id],
                                                 "Duration (h)", "Observed vs imputed")
res <- bind_rows(res)
print(res, width = Inf)
write.csv(res, file.path(output_dir, "welch_tests.csv"), row.names = FALSE)

# Density of the observed and completed durations
plot(density(na.omit(dur_obs)), main = "Fire duration (h)", col = "darkgreen", xlab = "Hours")
lines(density(na.omit(df$DuracaoHorasEstimado)), col = "orange", lty = 2)
legend("topright", legend = c("Observed", "Completed"), col = c("darkgreen", "orange"), lty = 1:2)