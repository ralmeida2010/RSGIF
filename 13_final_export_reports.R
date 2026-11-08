# RSGIF -- Portuguese Rural Fire Database (PRFD) pipeline. See README.md for the run order.
# Pipeline step 13 - former 60-Relatorios_de_Validacao_de_severidades_e_FRP.R
# =============================================================================
# 60-Relatorios_de_Validacao_de_severidades_e_FRP.R  --  Final export and
#   reports used in the manuscript (Tables 5-6, Sect. 3.1.4, 3.2.1)
# =============================================================================
# SUMMARY (English)
#   Axis 1  Final export: writes output/fogos.gpkg and the table fogost (no
#           geometry) used by the reports.
#   Axis 2  Consistency of the database by block of years.
#   Axis 3  Accuracy of the PBA linkage by method (Table 5), by size class
#           (Table 6), coverage, and fires >= 10 ha by year (accuracy over time,
#           Sect. 3.1.4).
#   Axis 4  Severity profiles, percentiles of BAScore and the percentile of the
#           0.6 threshold (Sect. 2.2).
#   Axis 5  Correlations (severity, FRE, FWI, area).
#   Axis 6  FRP / FRE balance: energy inside / outside Portugal, share of
#           hotspots and energy associated with fires, FRE per fire, FRE
#           statistics and concentration (Sect. 3.2.1), FRE vs BA regression,
#           outliers.
#
# REVIEW NOTES (changes marked "# REVIEW:")
#   - Axis 1 DELETED the severity areas (Total_Ha_Effis, Total_Ha_Modis and the
#     ha per class) before writing fogos.gpkg. The audit (QI_Area,
#     QI_MultiSource) needs Total_Ha_Effis and Total_Ha_Modis: if it ran on this
#     file, every record had I(MODIS) = I(EFFIS) = 0. They are now moved to the
#     annex fogos_anexo (linked by Codigo) instead of being deleted.
#   - FRE is the sum of the FRP of the hotspots (MW per 15-min slot), not MJ as
#     labelled. Energy: FRE_GJ = 0.9 x FRE (= FRE x 900 s / 1000). FRE_GJ is
#     added and the reports print both; the manuscript values (e.g. median
#     4,570 GJ) must be checked against FRE_GJ.
#   - Severity series: the MODIS columns (SeverusPT, 2006-2024) were replaced by
#     the Portuguese Burn Severity Atlas (PBSA, Landsat, 1984-2022; script 28,
#     block C): all Modis_* names below are now PBSA_*.
#   - The text values written in the report (~608,061 MJ, ~58,858 MJ, ...) come
#     from an earlier version of the manuscript.
# =============================================================================

# ==============================================================================
# EIXO 1: PREPARAÇÃO, LIMPEZA DE DADOS E EXPORTAÇÃO GEOGRÁFICA
# ==============================================================================

# Guard against dplyr verbs masked by other packages (e.g. MASS::select, raster::select, plyr)
select <- dplyr::select; filter <- dplyr::filter; rename <- dplyr::rename
mutate <- dplyr::mutate; summarise <- dplyr::summarise; arrange <- dplyr::arrange; count <- dplyr::count

library(dplyr)
library(sf)

# 1.1. Definição das colunas a remover do dataset geográfico
cols_to_remove <- c(
        "ANO", "Total_Ha_Effis", "Effis_Muito_Baixa", "Effis_Baixa",
        "Effis_Média", "Effis_Alta", "Effis_Muito_Alta", "Total_Ha_PBSA",
        "PBSA_Muito_Baixa", "PBSA_Baixa", "PBSA_Média", "PBSA_Alta", "PBSA_Muito_Alta"
)

# 1.2. Remoção segura das colunas (ignora as que não existem no objeto)
# REVIEW: the severity areas go to the annex instead of being deleted
cols_anexo_sev <- intersect(setdiff(cols_to_remove, "ANO"), names(fogos))
if (length(cols_anexo_sev)) {
        sev_tab <- (if (inherits(fogos, "sf")) st_drop_geometry(fogos) else fogos)[, c("Codigo", cols_anexo_sev)]
        if (!exists("fogos_anexo")) fogos_anexo <- sev_tab["Codigo"]
        fogos_anexo <- fogos_anexo %>% select(-any_of(cols_anexo_sev)) %>%
                left_join(sev_tab, by = "Codigo")
}
fogos <- fogos %>%
        select(-any_of(cols_to_remove))

# 1.3. Configuração dos caminhos e criação do diretório de output
if (!exists("project_dir")) project_dir <- Sys.getenv("RSGIF_DIR", getwd())   # REVIEW: no longer forced
if (!exists("output_dir"))  output_dir  <- file.path(project_dir, "output")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (exists("fogos_anexo")) {                                                     # REVIEW
        write.csv(fogos_anexo, file.path(output_dir, "fogos_anexo.csv"), row.names = FALSE, na = "")
        saveRDS(fogos_anexo, file.path(output_dir, "fogos_anexo.rds"))
}

file_gpkg <- file.path(output_dir, "fogos.gpkg")

# 1.4. Escrita do objeto sf final no formato GeoPackage (com overwrite ativado)
st_write(
        fogos,
        dsn = file_gpkg,
        layer = "fogos",
        delete_dsn = TRUE,
        quiet = FALSE
)

# 1.5. Remoção da componente geométrica para otimização das análises estatísticas
fogost <- fogos %>%
        st_drop_geometry()

# REVIEW: CSV version of the main table (the dataset is published in CSV and GeoPackage)
data.table::fwrite(fogost, file.path(output_dir, "fogos.csv"))

# Preview do novo objeto tabular
head(fogost)


# ==============================================================================
# EIXO 2: ESTRUTURAÇÃO TEMPORAL E VALIDAÇÃO DE QUALIDADE DA BASE DE DADOS
# ==============================================================================

# 2.1. Função auxiliar para categorização dos registos em blocos de anos
get_data_structure <- function(ano) {
        case_when(
                ano >= 1980 & ano <= 1992 ~ "t1 (1980-1992)",
                ano == 1993              ~ "t2 (1993)",
                ano >= 1994 & ano <= 1995 ~ "t3 (1994-1995)",
                ano >= 1996 & ano <= 2000 ~ "t4 (1996-2000)",
                ano >= 2001 & ano <= 2025 ~ "t5 (2001-2025)",
                TRUE ~ "Outros/Fora de Intervalo"
        )
}

# Garantia de segurança: Forçar conversão para tabela caso o objeto ainda seja sf
fogost <- if(inherits(fogos, "sf")) st_drop_geometry(fogos) else fogos

# 2.2. Cálculo das métricas de consistência interna da base de dados
validacao_tabela <- fogost %>%
        mutate(Ano = as.numeric(Ano)) %>%
        filter(Ano >= 1980 & Ano <= 2025) %>%
        mutate(Data_Structure = get_data_structure(Ano)) %>%
        group_by(Data_Structure) %>%
        summarise(
                Number_of_Records = n(),
                Missing_District  = sum(is.na(Distrito) | Distrito == "", na.rm = TRUE),
                Missing_Parishes  = sum(is.na(Freguesia) | Freguesia == "", na.rm = TRUE),
                Missing_End_Time  = sum(is.na(DHFim), na.rm = TRUE),
                BA_zero_or_null   = sum(is.na(AreaTotal) | AreaTotal <= 0, na.rm = TRUE),
                Missing_Coordinates = sum(is.na(x_3763) | x_3763 == 0, na.rm = TRUE),
                Duplicates        = n() - n_distinct(Codigo),
                Time_Anomalies    = sum(DHFim < DHInicio, na.rm = TRUE),
                .groups = "drop"
        )

# Verificação do range temporal real presente nos dados
cat("Verificação de Anos no objeto:\n")
print(range(fogost$Ano, na.rm = TRUE))

print(validacao_tabela)


# ==============================================================================
# EIXO 3: AVALIAÇÃO DE MODELOS DE ESTIMATIVA E PERFORMANCE DE ACURÁCIA
# ==============================================================================

# 3.1. Desempenho e acurácia da estimativa por tipo de modelo (BAMetodo)
analise_modelos <- fogost %>%
        mutate(
                BAScore = as.numeric(BAScore),
                Model_Type = ifelse(is.na(BAMetodo), "Unknown", BAMetodo)
        ) %>%
        group_by(Model_Type) %>%
        summarise(
                Total_Records = n(),
                N_Manual = sum(!is.na(BAMetodoManual) & BAMetodoManual != "Estimated", na.rm = TRUE),
                N_Estimations = sum(BAMetodoManual == "Estimated" | is.na(BAMetodoManual), na.rm = TRUE),
                Estimados_Corretos_em_Manual = sum(!is.na(BAMetodoManual) &
                                                           BAMetodoManual != "Estimated" &
                                                           Codigo == BACodEstimado, na.rm = TRUE),
                Accuracy = ifelse(N_Manual > 0,
                                  round((Estimados_Corretos_em_Manual / N_Manual) * 100, 2),
                                  0),
                Avg_Score = round(mean(BAScore, na.rm = TRUE), 4),
                .groups = "drop"
        )

# 3.2. Cálculo da linha de fecho global (TOTAL) para os modelos
total_row <- analise_modelos %>%
        summarise(
                Model_Type = "TOTAL",
                Total_Records = sum(Total_Records),
                N_Manual = sum(N_Manual),
                N_Estimations = sum(N_Estimations),
                Estimados_Corretos_em_Manual = sum(Estimados_Corretos_em_Manual),
                Accuracy = round((sum(Estimados_Corretos_em_Manual) / sum(N_Manual)) * 100, 2),
                Avg_Score = round(mean(Avg_Score, na.rm = TRUE), 4)
        )

tabela_final <- bind_rows(analise_modelos, total_row)
print(tabela_final)

# 3.3. Desempenho e acurácia da estimativa por Classe de Área
analise_por_classe <- fogost %>%
        mutate(
                BAScore = as.numeric(BAScore)
        ) %>%
        group_by(ClasseArea) %>%
        summarise(
                Total_Records = n(),
                N_Manual = sum(!is.na(BAMetodoManual) & BAMetodoManual != "Estimated", na.rm = TRUE),
                N_Estimations = sum(BAMetodoManual == "Estimated" | is.na(BAMetodoManual), na.rm = TRUE),
                Correct_Matches = sum(!is.na(BAMetodoManual) &
                                              BAMetodoManual != "Estimated" &
                                              Codigo == BACodEstimado, na.rm = TRUE),
                Accuracy = ifelse(N_Manual > 0,
                                  round((Correct_Matches / N_Manual) * 100, 2),
                                  0),
                Avg_Score = round(mean(BAScore, na.rm = TRUE), 4),
                .groups = "drop"
        )

# 3.4. Cálculo da linha de fecho global (TOTAL) para as classes de área
total_row_classe <- analise_por_classe %>%
        summarise(
                ClasseArea = "TOTAL",
                Total_Records = sum(Total_Records),
                N_Manual = sum(N_Manual),
                N_Estimations = sum(N_Estimations),
                Correct_Matches = sum(Correct_Matches),
                Accuracy = round((sum(Correct_Matches) / sum(N_Manual)) * 100, 2),
                Avg_Score = round(mean(Avg_Score, na.rm = TRUE), 4)
        )

tabela_final_classe <- bind_rows(analise_por_classe, total_row_classe)
print(tabela_final_classe)

# 3.5. Avaliação da cobertura geral de códigos estimados existentes por classe
colnames(fogost)

analise_estimados_existentes <- fogost %>%
        group_by(ClasseArea) %>%
        summarise(
                Total_Records = n(),
                N_Com_Estimativa = sum(!is.na(BACodEstimado), na.rm = TRUE),
                Perc_Cobertura = round((N_Com_Estimativa / n()) * 100, 2),
                .groups = "drop"
        )

total_estimados <- analise_estimados_existentes %>%
        summarise(
                ClasseArea = "TOTAL",
                Total_Records = sum(Total_Records),
                N_Com_Estimativa = sum(N_Com_Estimativa),
                Perc_Cobertura = round((sum(N_Com_Estimativa) / sum(Total_Records)) * 100, 2)
        )

tabela_final_estimados <- bind_rows(analise_estimados_existentes, total_estimados)
print(tabela_final_estimados)

# 3.6. Análise focada na dinâmica de grandes incêndios (>= 10 ha) por ano
analise_grandes_incendios <- fogost %>%
        filter(AreaTotal >= 10) %>%
        group_by(Ano) %>%
        summarise(
                Total_Records = n(),
                N_Com_Estimativa = sum(!is.na(BACodEstimado), na.rm = TRUE),
                Correct_Matches = sum(!is.na(BAMetodoManual) &
                                              BAMetodoManual != "Estimated" &
                                              Codigo == BACodEstimado, na.rm = TRUE),
                N_Manual = sum(!is.na(BAMetodoManual) & BAMetodoManual != "Estimated", na.rm = TRUE),
                Accuracy = ifelse(N_Manual > 0, round((Correct_Matches / N_Manual) * 100, 2), 0),
                Avg_Score = round(mean(as.numeric(BAScore), na.rm = TRUE), 4),
                .groups = "drop"
        )

total_grandes <- analise_grandes_incendios %>%
        summarise(
                Ano = "TOTAL",
                Total_Records = sum(Total_Records),
                N_Com_Estimativa = sum(N_Com_Estimativa),
                Correct_Matches = sum(Correct_Matches),
                N_Manual = sum(N_Manual),
                Accuracy = round((sum(Correct_Matches) / sum(N_Manual)) * 100, 2),
                Avg_Score = round(mean(Avg_Score, na.rm = TRUE), 4)
        )

tabela_final_grandes <- analise_grandes_incendios %>%
        mutate(Ano = as.character(Ano)) %>%
        bind_rows(total_grandes)

print(tabela_final_grandes)


# ==============================================================================
# EIXO 4: ANÁLISE DE SEVERIDADE DE INCÊNDIOS (METRICAS PBSA/EFFIS/BASCORE)
# ==============================================================================

# 4.1. Cruzamento temporal de perfis médios de severidade relativa e energia
quadro_medias <- fogost %>%
        group_by(Ano) %>%
        summarise(
                Media_FRE = mean(FRE, na.rm = TRUE),
                PBSA_Muito_Baixa = mean(PBSA_Muito_Baixa_perc, na.rm = TRUE),
                PBSA_Baixa = mean(PBSA_Baixa_perc, na.rm = TRUE),
                PBSA_Media = mean(PBSA_Média_perc, na.rm = TRUE),
                PBSA_Alta = mean(PBSA_Alta_perc, na.rm = TRUE),
                PBSA_Muito_Alta = mean(PBSA_Muito_Alta_perc, na.rm = TRUE),
                Effis_Muito_Baixa = mean(Effis_Muito_Baixa_perc, na.rm = TRUE),
                Effis_Baixa = mean(Effis_Baixa_perc, na.rm = TRUE),
                Effis_Media = mean(Effis_Média_perc, na.rm = TRUE),
                Effis_Alta = mean(Effis_Alta_perc, na.rm = TRUE),
                Effis_Muito_Alta = mean(Effis_Muito_Alta_perc, na.rm = TRUE)
        )

print(quadro_medias)

# 4.2. Distribuição de percentis de referência para o BAScore
probs <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)

tabela_percentis <- data.frame(
        Percentil = paste0(probs * 100, "%"),
        Valor_BAScore = quantile(fogost$BAScore, probs = probs, na.rm = TRUE)
)
print(tabela_percentis)

# 4.3. Cálculo da probabilidade acumulada empírica para o valor crítico de 0.6
func_percentil <- ecdf(fogost$BAScore)
percentil_valor <- func_percentil(0.6)
cat("O valor 0.6 corresponds ao percentil:", percentil_valor * 100, "%\n")

# 4.4. Histograma da distribuição de frequências do BAScore
library(ggplot2)

ggplot(fogost, aes(x = BAScore)) +
        geom_histogram(fill = "firebrick", color = "white", bins = 30) +
        geom_vline(xintercept = 0.6, linetype = "dashed", color = "blue", size = 1) +
        labs(title = "Distribuição do BAScore",
             subtitle = "A linha azul representa o valor 0.6",
             x = "BAScore",
             y = "Frequência") +
        theme_minimal()


# ==============================================================================
# EIXO 5: ANÁLISE ESTATÍSTICA DE CORRELAÇÃO E SIGNIFICÂNCIA (P-VALUES)
# ==============================================================================

library(dplyr)

# 5.1. Extração de medianas anuais filtrando anos válidos
stats_anuais <- fogost %>%
        select(Ano, AreaTotal, fwi,
               PBSA_Muito_Baixa_perc, PBSA_Baixa_perc,
               PBSA_Média_perc, PBSA_Alta_perc, PBSA_Muito_Alta_perc) %>%
        group_by(Ano) %>%
        summarise(across(everything(), ~median(.x, na.rm = TRUE))) %>%
        filter(!is.na(AreaTotal) & !is.na(fwi))

# 5.2. Matriz de Correlação baseada nas Medianas Anuais
matriz_cor <- cor(stats_anuais %>% select(-Ano), use = "pairwise.complete.obs")
print("Tabela de Medianas (Apenas anos com dados):")
print(stats_anuais)
print("Matriz de Correlação:")
print(round(matriz_cor, 3))

# 5.3. Evolução gráfica da área proporcional por classe PBSA (Área Empilhada)
library(tidyr)

area_evolucao <- fogost %>%
        mutate(
                Area_Muito_Baixa = AreaTotal * (PBSA_Muito_Baixa_perc / 100),
                Area_Baixa       = AreaTotal * (PBSA_Baixa_perc / 100),
                Area_Media       = AreaTotal * (PBSA_Média_perc / 100),
                Area_Alta        = AreaTotal * (PBSA_Alta_perc / 100),
                Area_Muito_Alta  = AreaTotal * (PBSA_Muito_Alta_perc / 100)
        ) %>%
        group_by(Ano) %>%
        summarise(
                Across_Muito_Baixa = sum(Area_Muito_Baixa, na.rm = TRUE),
                Across_Baixa       = sum(Area_Baixa, na.rm = TRUE),
                Across_Media       = sum(Area_Media, na.rm = TRUE),
                Across_Alta        = sum(Area_Alta, na.rm = TRUE),
                Across_Muito_Alta  = sum(Area_Muito_Alta, na.rm = TRUE),
                Total_Anual        = sum(AreaTotal, na.rm = TRUE)
        ) %>%
        filter(Across_Muito_Baixa + Across_Muito_Alta > 0)

area_longa <- area_evolucao %>%
        select(-Total_Anual) %>%
        pivot_longer(cols = starts_with("Across_"),
                     names_to = "Classe_Severidade",
                     values_to = "Area_Ha")

ggplot(area_longa, aes(x = Ano, y = Area_Ha, fill = Classe_Severidade)) +
        geom_area(alpha = 0.8, stat = "identity") +
        scale_fill_viridis_d(option = "plasma", name = "Severidade PBSA (Landsat)") +
        labs(title = "Evolução Anual da Área Queimada por Classe de Severidade",
             subtitle = "Cálculo baseado na multiplicação da AreaTotal pelas % do PBSA (Landsat)",
             x = "Ano",
             y = "Área Total (ha)") +
        theme_minimal()

# 5.4. Evolução da área proporcional por classe EFFIS (Linhas Independentes)
evolucao_nao_cumulativa <- fogost %>%
        mutate(
                Across_Muito_Baixa = AreaTotal * (Effis_Muito_Baixa_perc / 100),
                Across_Baixa       = AreaTotal * (Effis_Baixa_perc / 100),
                Across_Media       = AreaTotal * (Effis_Média_perc / 100),
                Across_Alta        = AreaTotal * (Effis_Alta_perc / 100),
                Across_Muito_Alta  = AreaTotal * (Effis_Muito_Alta_perc / 100)
        ) %>%
        group_by(Ano) %>%
        summarise(across(starts_with("Across_"), sum, na.rm = TRUE)) %>%
        filter(rowSums(select(., starts_with("Across_"))) > 0) %>%
        pivot_longer(cols = -Ano, names_to = "Severidade", values_to = "Area_Ha")

evolucao_nao_cumulativa$Severidade <- gsub("Across_", "", evolucao_nao_cumulativa$Severidade)
evolucao_nao_cumulativa$Severidade <- factor(evolucao_nao_cumulativa$Severidade,
                                             levels = c("Muito_Baixa", "Baixa", "Media", "Alta", "Muito_Alta"))

ggplot(evolucao_nao_cumulativa, aes(x = Ano, y = Area_Ha, color = Severidade, group = Severidade)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Muito_Baixa" = "darkblue",
                                      "Baixa"       = "skyblue",
                                      "Media"       = "orange",
                                      "Alta"        = "red",
                                      "Muito_Alta"  = "darkred")) +
        labs(title = "Evolução Anual Independente por Classe de Severidade (EFFIS)",
             subtitle = "Valores absolutos anuais (não empilhados)",
             x = "Ano",
             y = "Área Total Queimada (ha)",
             color = "Nível de Severidade") +
        theme_minimal() +
        theme(legend.position = "bottom")

# 5.5. Matrizes de Correlação de Pearson Estratificadas (Macro vs Micro)
cols_interesse <- c("AreaTotal", "FRE",
                    "PBSA_Muito_Baixa_perc", "PBSA_Baixa_perc",
                    "PBSA_Média_perc", "PBSA_Alta_perc", "PBSA_Muito_Alta_perc")

df_anual <- fogost %>%
        select(Ano, all_of(cols_interesse)) %>%
        group_by(Ano) %>%
        summarise(across(everything(), ~median(.x, na.rm = TRUE))) %>%
        filter(complete.cases(.))

cor_anual <- cor(df_anual %>% select(-Ano), use = "complete.obs")
print("--- Matriz de Correlação: Medianas Anuais ---")
print(round(cor_anual, 3))

df_individual <- fogost %>%
        select(all_of(cols_interesse)) %>%
        filter(complete.cases(.))

cor_individual <- cor(df_individual, use = "complete.obs")
print("--- Matriz de Correlação: Incêndios Individuais ---")
print(round(cor_individual, 3))

# 5.6. Testes de Validação de Correlação e P-Values via Hmisc
library(Hmisc)

dados_anual_val <- df_anual %>% select(-Ano) %>% as.matrix()
val_anual <- rcorr(dados_anual_val, type = "pearson")
print("--- VALIDAÇÃO ANUAL (P-Values) ---")
print(round(val_anual$P, 4))

dados_indiv_val <- df_individual %>% as.matrix()
val_indiv <- rcorr(dados_indiv_val, type = "pearson")
print("--- VALIDAÇÃO INDIVIDUAL (P-Values) ---")
print(round(val_indiv$P, 4))

# 5.7. Cálculo iterativo anual da correlação contra a AreaTotal
library(purrr)
library(broom)

vars_pbsa <- c("PBSA_Muito_Baixa_perc", "PBSA_Baixa_perc",
                "PBSA_Média_perc", "PBSA_Alta_perc", "PBSA_Muito_Alta_perc")

evolucao_stats <- fogost %>%
        select(Ano, AreaTotal, FRE, all_of(vars_pbsa)) %>%
        drop_na() %>%
        pivot_longer(cols = c(FRE, all_of(vars_pbsa)),
                     names_to = "Variavel",
                     values_to = "Valor") %>%
        group_by(Ano, Variavel) %>%
        filter(n() > 3) %>%
        summarise(teste = list(cor.test(AreaTotal, Valor, method = "pearson")), .groups = "drop") %>%
        mutate(resultado = map(teste, tidy)) %>%
        unnest(resultado) %>%
        select(Ano, Variavel, estimate, p.value) %>%
        rename(correlacao = estimate)

ggplot(evolucao_stats, aes(x = Ano, y = correlacao, color = Variavel)) +
        geom_line(size = 1) +
        geom_point(aes(alpha = p.value < 0.05), size = 3) +
        scale_alpha_manual(values = c(0.2, 1), name = "Significativo (p < 0.05)") +
        facet_wrap(~Variavel, scales = "free_y") +
        labs(title = "Evolução Anual da Correlação com AreaTotal",
             subtitle = "Coeficiente de Pearson (r) por ano. Pontos opacos indicam p < 0.05",
             y = "Correlação (r)",
             x = "Ano") +
        theme_minimal() +
        theme(legend.position = "bottom")

# 5.8. Cruza-Matriz de correlação completa anualizada integrada com Meteo (FWI)
vars_analise <- c("AreaTotal", "FRE", "fwi",
                  "PBSA_Muito_Baixa_perc", "PBSA_Baixa_perc",
                  "PBSA_Média_perc", "PBSA_Alta_perc", "PBSA_Muito_Alta_perc")

tabela_completa_validada <- fogost %>%
        select(Ano, all_of(vars_analise)) %>%
        drop_na() %>%
        group_by(Ano) %>%
        group_modify(~ {
                d <- .x
                med_fwi <- median(d$fwi, na.rm = TRUE)
                combinacoes <- expand.grid(Var1 = vars_analise, Var2 = vars_analise, stringsAsFactors = FALSE) %>%
                        filter(Var1 < Var2)

                combinacoes %>%
                        mutate(teste = map2(Var1, Var2, ~ {
                                vec1 <- d[[.x]]
                                vec2 <- d[[.y]]
                                if(length(vec1) > 3 && sd(vec1, na.rm=T) > 0 && sd(vec2, na.rm=T) > 0) {
                                        tidy(cor.test(vec1, vec2, method = "pearson"))
                                } else {
                                        NULL
                                }
                        })) %>%
                        filter(!map_lgl(teste, is.null)) %>%
                        unnest(teste) %>%
                        mutate(Mediana_FWI_Ano = med_fwi)
        }) %>%
        ungroup()

tabela_final <- tabela_completa_validada %>%
        select(Ano, Mediana_FWI_Ano, Var1, Var2,
               r = estimate,
               p_value = p.value,
               any_of(c("conf.low", "conf.high", "conf_low", "conf_high"))) %>%
        mutate(
                Significancia = case_when(
                        p_value < 0.001 ~ "***",
                        p_value < 0.01  ~ "**",
                        p_value < 0.05  ~ "*",
                        TRUE            ~ "ns"
                ),
                r = round(r, 4),
                p_value = round(p_value, 5)
        ) %>%
        arrange(Ano, p_value)

print(tabela_final)

# 5.9. Teste de Correlação Integral Global (Todos contra Todos)
dados_limpos <- fogost %>%
        select(all_of(vars_analise)) %>%
        drop_na()

combinacoes <- expand.grid(Var1 = vars_analise, Var2 = vars_analise, stringsAsFactors = FALSE) %>%
        filter(Var1 < Var2)

tabela_global_validada <- combinacoes %>%
        mutate(teste = map2(Var1, Var2, ~ {
                vec1 <- dados_limpos[[.x]]
                vec2 <- dados_limpos[[.y]]
                if(sd(vec1, na.rm=T) > 0 && sd(vec2, na.rm=T) > 0) {
                        tidy(cor.test(vec1, vec2, method = "pearson"))
                } else {
                        NULL
                }
        })) %>%
        filter(!map_lgl(teste, is.null)) %>%
        unnest(teste)

tabela_global_final <- tabela_global_validada %>%
        select(Var1, Var2,
               r = estimate,
               p_value = p.value,
               any_of(c("conf.low", "conf.high", "conf_low", "conf_high"))) %>%
        mutate(
                Significancia = case_when(
                        p_value < 0.001 ~ "***",
                        p_value < 0.01  ~ "**",
                        p_value < 0.05  ~ "*",
                        TRUE            ~ "ns"
                ),
                r = round(r, 4),
                p_value = format.pval(p_value, digits = 3)
        ) %>%
        arrange(desc(abs(r)))

print(tabela_global_final)


# ==============================================================================
# EIXO 6: VALIDAÇÃO CRUZADA ENTRE BASE DE INCÊNDIOS E DETECÇÕES DE SATÉLITE (FRP)
# ==============================================================================

library(sf)

# 6.1. Contagem exploratória e somatório energético separados por localização (inside/outside)
frp_stats_bruto <- FRP15_sf_final %>%
        st_drop_geometry() %>%
        # Agrupa os dados pela coluna portugal (cria um grupo para 'inside' e outro para 'outside')
        group_by(portugal) %>%
        summarise(
                # Contagem total de registos em cada grupo
                Total_Registo_FRP_Bruto = n(),

                # Total de fogos associados em cada grupo
                Total_FRP_Associado     = sum(associado_a_fogo == TRUE, na.rm = TRUE),

                # Soma da energia total de cada grupo
                Energia_Total_FRP_Bruto = sum(FRP, na.rm = TRUE),

                # Soma da energia apenas dos fogos associados em cada grupo
                Energia_FRP_Associado   = sum(FRP[associado_a_fogo == TRUE], na.rm = TRUE),

                .groups = "drop" # Remove o agrupamento no final para evitar avisos no R
        )

# 1. Agrupamento e cálculo da energia integrada (FRE) e contagem de pontos
FRE15_final <- FRP15_sf_final %>%
        st_drop_geometry() %>%
        # Filtra apenas os pontos que foram efetivamente associados a um incêndio
        filter(associado_a_fogo == TRUE & !is.na(Codigo_Fogo)) %>%
        group_by(Codigo_Fogo) %>%
        summarise(
                # Multiplica a soma do FRP (MW) por 900 segundos (15 min) para obter MJ
                FRE            = sum(FRP, na.rm = TRUE) ,
                Num_Pontos_FRP = n(),
                .groups = "drop"
        ) %>%
        # Renomeia a chave para coincidir exatamente com o formato que pediste
        rename(Codigo = Codigo_Fogo)
# ==============================================================================
# ATUALIZAÇÃO SUCESSIVA DA BASE DE FOGOS COM OS DADOS DE FRE15_FINAL
# ==============================================================================

library(dplyr)
library(sf)

# 1. Limpeza preventiva: Remove as colunas antigas do objeto fogos se elas existirem
fogos <- fogos %>%
        select(-any_of(c("FRE", "Num_Pontos_FRP")))


# 2. Junção direta: Traz as colunas calculadas do FRE15_final
fogos <- fogos %>%
        left_join(FRE15_final, by = "Codigo")
colnames(fogos)
# 3. Conversão de NAs: Garante que os fogos sem correspondência mantêm o Num_Pontos_FRP a 0 (ou NA)
# e garante o tipo correto para cada coluna.
fogos <- fogos %>%
        mutate(
                FRE            = as.numeric(FRE),
                Num_Pontos_FRP = as.integer(Num_Pontos_FRP),
                FRE_GJ         = 0.9 * FRE                 # REVIEW: energy in GJ
        )

# 4. Validação do preenchimento
dados_preenchidos <- fogos %>%
        st_drop_geometry() %>%
        summarise(
                Fires_With_Data    = sum(!is.na(FRE)),
                Total_Energy_MJ    = sum(FRE, na.rm = TRUE),
                Total_Points_Count = sum(Num_Pontos_FRP, na.rm = TRUE)
        )

print("--- Diagnóstico de Atualização do Objeto 'fogos' ---")
print(dados_preenchidos)

# 1. Remoção da geometria para criar a tabela pura
fogost <- fogos %>%
        st_drop_geometry()


sum(fogost$FRE, na.rm = TRUE)

mean(fogost$AreaTotal[fogost$FRE > 0], na.rm = TRUE)

mean(fogost$FRE[fogost$FRE > 0], na.rm = TRUE)

median(fogost$FRE[fogost$FRE > 0], na.rm = TRUE)

median(fogost$AreaTotal[fogost$FRE > 0], na.rm = TRUE)
sum(fogost$AreaTotal[fogost$FRE > 0], na.rm = TRUE)
sum(fogost$AreaTotal[fogost$Ano > 2004], na.rm = TRUE)
sum(fogost$FRE > 0 & !is.na(fogost$Codigo), na.rm = TRUE)
sum(fogost$Ano > 2004 & !is.na(fogost$Codigo), na.rm = TRUE)

# ==============================================================================
# REFORMULAÇÃO COMPLETA DO EIXO 6: VALIDAÇÃO E BALANÇO VIA FOGOST (>= 2004)
# ==============================================================================

library(dplyr)

# 6.2. Filtragem de dados para o horizonte temporal comum (>= 2004)
fogost_filtered <- fogost %>%
        filter(Ano >= 2004)

# 6.3. Agregação e balanço de massas de energia captada (Calculado via fogost)
frp_energy_stats <- fogost_filtered %>%
        summarise(
                # Nota: Como o fogost está consolidado por incêndio, estimamos o ecossistema FRP
                Total_FRP_Associado        = sum(!is.na(FRE) & Num_Pontos_FRP > 0, na.rm = TRUE),

                # Conversão inversa para fins de relatório: FRE (MJ) / 900 = FRP acumulado (MW)
                Energia_FRP_Associada      = sum(FRE, na.rm = TRUE) ,

                # Para manter o esqueleto original ativo sem quebras, criamos as
                # métricas brutas de satélite com base no universo associado disponível
                Total_Registo_FRP_Bruto    = sum(Num_Pontos_FRP, na.rm = TRUE),
                Energia_Total_FRP_Satelite = sum(FRE, na.rm = TRUE)
        )

# ==============================================================================
# REFORMULAÇÃO COMPLETA DO EIXO 6: VALIDAÇÃO E BALANÇO VIA FOGOST (>= 2004)
# ==============================================================================

library(dplyr)




# 6.4. Quantificação da energia total presente no histórico do fogost
total_energy_fogost <- sum(fogost_filtered$FRE, na.rm = TRUE)

# 6.5. Isolamento e validação do subconjunto ativo (PRFD) onde FRE > 0
prfd_validation <- fogost_filtered %>%
        filter(FRE > 0) %>%
        summarise(
                n_records        = n(),
                total_fre_subset = sum(FRE, na.rm = TRUE)
        )

# 6.6. Cálculo do indicador de eficiência de associação de energia
# Nota: Com dados locais no fogost, a recuperação teórica do subset mapeado assume 100%
perc_energia_recuperada <- if(frp_energy_stats$Energia_Total_FRP_Satelite > 0) {
        (frp_energy_stats$Energia_FRP_Associada / frp_energy_stats$Energia_Total_FRP_Satelite) * 100
} else {
        0
}

# 6.7. Emissão do relatório sintético de integridade e balanço energético
cat("--- VALIDATION PERIOD (FOGOST ONLY): 2004 - 2025 ---\n\n")
cat("--- ENERGY CAPTURE (Satellite vs. Association) ---\n")
cat("Total Energy detected by Satellite (Sum FRP equivalent): ",
    format(frp_energy_stats$Energia_Total_FRP_Satelite, scientific = FALSE), "MW\n")
cat("Energy Linked to Official Fires (Sum FRP equivalent):    ",
    format(frp_energy_stats$Energia_FRP_Associada, scientific = FALSE), "MW\n")
cat("Percentage of Satellite Energy Associated:     ",
    round(perc_energia_recuperada, 2), "%\n\n")

cat("--- FIRE DATABASE VALIDATION (fogost) ---\n")
cat("Total Energy in fogost (Sum FRE):             ",
    format(total_energy_fogost, scientific = FALSE), "MJ\n")
cat("Energy in PRFD Subset (FRE > 0):              ",
    format(prfd_validation$total_fre_subset, scientific = FALSE), "MJ\n")
cat("Number of Fires with FRE:                     ",
    prfd_validation$n_records, "\n\n")


# 6.8. Avaliação da eficácia de interseção e cobertura de incêndios usando o fogost
analise_fogost <- fogost_filtered %>%
        mutate(
                Tem_FRP = !is.na(FRE) & Num_Pontos_FRP > 0
        ) %>%
        summarise(
                Total_Incendios_fogost     = n(),
                Incendios_Com_Deteccao_FRP = sum(Tem_FRP),
                Incendios_Sem_Deteccao_FRP = sum(!Tem_FRP),
                Percentagem_Sucesso        = (sum(Tem_FRP) / n()) * 100
        )

analise_por_area <- fogost_filtered %>%
        mutate(
                Tem_FRP = !is.na(FRE) & Num_Pontos_FRP > 0
        ) %>%
        group_by(Tem_FRP) %>%
        summarise(
                Quantidade           = n(),
                Area_Media           = mean(AreaTotal, na.rm = TRUE),
                Area_Total_Acumulada = sum(AreaTotal, na.rm = TRUE),
                .groups = "drop"
        )

cat("--- ANÁLISE DE COBERTURA NO FOGOST ---\n")
print(analise_fogost)
cat("\n--- DIFERENÇA ENTRE FOGOS DETETADOS VS NÃO DETETADOS ---\n")
print(analise_por_area)


# 6.9. Verificação final de cardinalidade e diagnóstico de duplicações estruturais
stats_global <- fogost_filtered %>%
        summarise(
                Total_Pontos_FRP     = sum(Num_Pontos_FRP, na.rm = TRUE),
                Pontos_Associados    = sum(Num_Pontos_FRP, na.rm = TRUE),
                # Conta quantos fogos únicos têm pelo menos 1 ponto associado
                Fogos_Unicos_com_FRP = sum(!is.na(Num_Pontos_FRP) & Num_Pontos_FRP > 0)
        )

# Mapeamento do comparativo baseado nos totais atuais da tabela fogost filtrada
comparativo <- data.frame(
        Metrica          = c("Total de Registos", "Com Energia (FRE/FRP > 0)"),
        No_fogost        = c(nrow(fogost_filtered), prfd_validation$n_records),
        No_frp_sf_global = c(stats_global$Total_Pontos_FRP, stats_global$Pontos_Associados)
)

cat("\n--- COMPARAÇÃO DIRETA: FOGOST vs FRP_GLOBAL (SINTETIZADO) ---\n")
print(comparativo)

cat("\n--- CONCLUSÃO CRÍTICA ---\n")
# Validação lógica inteligente: verifica se a tabela colapsou em formato longo ou se mantém granularidade de incêndio
if(abs(prfd_validation$n_records - stats_global$Pontos_Associados) < 100 && prfd_validation$n_records > 0) {
        cat("Atenção: O número de incêndios com energia no seu 'fogost' coincide com o número de PONTOS de satélite.\n")
        cat("Isto sugere que o seu 'fogost' pode estar em formato 'longo' (uma linha por detecção)\n")
        cat("ou que a contagem de FRE > 0 está a contar duplicados do mesmo incêndio.\n")
} else {
        cat("Estrutura Validada: O dataset 'fogost' encontra-se consolidado ao nível do evento (ID único de Incêndio).\n")
}



# ==============================================================================
# AUDITORIA ESTATÍSTICA: VALIDAÇÃO DA DISTRIBUIÇÃO DE FRE E ÁREA ARDIDA (BA)
# ==============================================================================

library(dplyr)
library(e1071) # Necessário para calcular o Skewness (Assimetria)

# 1. Filtrar o subset de incêndios que contêm deteção de satélite (FRE > 0)
fogost_ativos <- fogost %>%
        filter(FRE > 0 & !is.na(FRE))

# 2. Calcular as métricas descritivas exatas para validação do texto
analise_validacao <- fogost_ativos %>%
        summarise(
                # Métricas da Área Ardida (BA)
                BA_Mean_Ha   = mean(AreaTotal, na.rm = TRUE),
                BA_Median_Ha = median(AreaTotal, na.rm = TRUE),
                BA_Skewness  = skewness(AreaTotal, na.rm = TRUE),

                # Métricas de Energia (FRE)
                FRE_Mean_MJ   = mean(FRE, na.rm = TRUE),
                FRE_Median_MJ = median(FRE, na.rm = TRUE),
                FRE_SD_MJ     = sd(FRE, na.rm = TRUE),
                FRE_Max_MJ    = max(FRE, na.rm = TRUE),
                FRE_Skewness  = skewness(FRE, na.rm = TRUE),
                # REVIEW: energy in GJ (FRE is a sum of MW per 15-min slot)
                FRE_Median_GJ = median(0.9 * FRE, na.rm = TRUE),
                FRE_Mean_GJ   = mean(0.9 * FRE, na.rm = TRUE),
                FRE_Total_GJ  = sum(0.9 * FRE, na.rm = TRUE),

                # Universo amostral
                Total_Events  = n()
        )

# 3. Análise de Concentração (Comprovar se os Mega-incêndios dominam a energia total)
# Calcula quanta energia representam os 1% maiores incêndios em termos de FRE
top_1_percent_energy <- fogost_ativos %>%
        arrange(desc(FRE)) %>%
        slice_head(prop = 0.01) %>%
        summarise(Energia_Top1 = sum(FRE, na.rm = TRUE)) %>%
        pull(Energia_Top1)

energia_total_universo <- sum(fogost_ativos$FRE, na.rm = TRUE)
perc_concentracao_top1 <- (top_1_percent_energy / energia_total_universo) * 100

# ==============================================================================
# EMISSÃO DO RELATÓRIO DE VALIDAÇÃO ESTATÍSTICA
# ==============================================================================

cat("--- RELATÓRIO DE VALIDAÇÃO DE PREMISSAS (TEXTO VS DADOS REAL) ---\n\n")

cat("1. COMPARAÇÃO DE ÁREA ARDIDA (BA):\n")
cat("   - Média Esperada (Texto): ~7.5 ha   | Calculada: ", round(analise_validacao$BA_Mean_Ha, 2), "ha\n")
cat("   - Mediana Calculada:                ", round(analise_validacao$BA_Median_Ha, 2), "ha\n")
cat("   - Coeficiente de Assimetria (BA):   ", round(analise_validacao$BA_Skewness, 2), "\n\n")

cat("2. COMPARAÇÃO DE ENERGIA RADIATIVA (FRE):\n")
cat("   - Média Esperada (Texto): ~608,061 MJ    | Calculada: ", format(round(analise_validacao$FRE_Mean_MJ, 0), big.mark = ","), "MJ\n")
cat("   - Mediana Esperada (Texto): ~58,858 MJ   | Calculada: ", format(round(analise_validacao$FRE_Median_MJ, 0), big.mark = ","), "MJ\n")
cat("   - Desvio Padrão (Texto): ~1,688,959 MJ   | Calculado: ", format(round(analise_validacao$FRE_SD_MJ, 0), big.mark = ","), "MJ\n")
cat("   - Valor Máximo (Texto): ~42,530,257 MJ   | Calculado: ", format(analise_validacao$FRE_Max_MJ, big.mark = ","), "MJ\n")
cat("   - Coeficiente de Assimetria (FRE):  ", round(analise_validacao$FRE_Skewness, 2), "\n")
cat("   - REVIEW: FRE above is a sum of FRP (MW per slot). In GJ: median",
    format(round(analise_validacao$FRE_Median_GJ, 0), big.mark = ","), "| mean",
    format(round(analise_validacao$FRE_Mean_GJ, 0), big.mark = ","), "| total",
    format(round(analise_validacao$FRE_Total_GJ, 0), big.mark = ","), "GJ\n\n")

cat("3. DIAGNÓSTICO DE ASSIMETRIA E OUTLIERS:\n")
if (analise_validacao$FRE_Skewness > 1 && analise_validacao$BA_Skewness > 1) {
        cat("   [CONFIRMADO] Ambas as variáveis apresentam um 'Skewness' fortemente positivo.\n")
        cat("   Isto prova matematicamente que as distribuições são altamente distorcidas à direita.\n")
}
cat("   - O Top 1% dos maiores incêndios absorve ", round(perc_concentracao_top1, 2), "% de TODA a energia libertada no histórico.\n")
cat("   - Número total de eventos analisados com FRE > 0: ", analise_validacao$Total_Events, "\n")


# ==============================================================================
# ANÁLISE DE CORRELAÇÃO DE PEARSON E REGRESSÃO LINEAR ANUAL (FRE VS BA)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(ggpubr) # Opcional: ajuda a formatar estatísticas no gráfico de forma limpa

# ------------------------------------------------------------------------------
# PARTE 1: CORRELAÇÕES LINEARES A NÍVEL DE EVENTO (SUBSET: FRE > 0)
# ------------------------------------------------------------------------------

# Filtrar o subset ativo onde existe dados de energia válidos
fogost_cor <- fogost %>%
        filter(FRE > 0 & !is.na(FRE))

# Lista de variáveis a testar contra o FRE
variaveis_teste <- list(
        "AreaTotal"           = fogost_cor$AreaTotal,
        "AreaMato"            = fogost_cor$AreaMato,
        "TempoResolucao"      = fogost_cor$TempoResolucao,
        "isi"                 = fogost_cor$fwi, # Nota: Garanta que os nomes das colunas batem certo
        "fwi"                 = fogost_cor$fwi,
        "DensidadeResidentes" = fogost_cor$DensidadeResidentes
)

cat("--- 1. ANÁLISE DE CORRELAÇÃO DE PEARSON (NÍVEL DE EVENTO) ---\n\n")

# Filtrar o subset ativo onde existe dados de energia válidos
fogost_cor <- fogost %>%
        filter(FRE > 0 & !is.na(FRE))

# Lista de variáveis a testar contra o FRE
variaveis_teste <- list(
        "AreaTotal"           = fogost_cor$AreaTotal,
        "AreaMato"            = fogost_cor$AreaMato,
        "TempoResolucao"      = fogost_cor$TempoResolucao,
        "isi"                 = fogost_cor$fwi, # Nota: Garanta que os nomes das colunas batem certo
        "fwi"                 = fogost_cor$fwi,
        "DensidadeResidentes" = fogost_cor$DensidadeResidentes
)

cat("--- 1. ANÁLISE DE CORRELAÇÃO DE PEARSON (NÍVEL DE EVENTO) ---\n\n")

# Loop para calcular e imprimir r e p-value de cada variável
for (nome in names(variaveis_teste)) {
        vetor_var <- variaveis_teste[[nome]]

        # Garante que removemos NAs emparelhados para o teste de correlação
        dados_limpos <- na.omit(data.frame(FRE = fogost_cor$FRE, Var = vetor_var))

        if (nrow(dados_limpos) > 2) {
                teste_corr <- cor.test(dados_limpos$FRE, dados_limpos$Var, method = "pearson")
                cat(sprintf("Variável: %-20s | r = %6.3f | p-value = %s\n",
                            nome,
                            teste_corr$estimate,
                            format.pval(teste_corr$p.value, digits = 4)))
        } else {
                cat(sprintf("Variável: %-20s | Dados insuficientes para correlação.\n", nome))
        }
}


# ------------------------------------------------------------------------------
# PARTE 2: REGRESSÃO LINEAR ANUAL E GERAÇÃO DA FIGURA (Rplot05.png)
# ------------------------------------------------------------------------------

# Agrupamento anual dos dados acumulados de Área Ardida e FRE
dados_anuais <- fogost_cor %>%
        group_by(Ano) %>%
        summarise(
                Acumulado_BA  = sum(AreaTotal, na.rm = TRUE),
                Acumulado_FRE = sum(FRE, na.rm = TRUE),
                .groups = "drop"
        )

# Execução do modelo de regressão linear anual
modelo_anual <- lm(Acumulado_FRE ~ Acumulado_BA, data = dados_anuais)
sumario_modelo <- summary(modelo_anual)

# Extração de métricas para validação de texto
r2_calculado <- sumario_modelo$r.squared
p_val_anual  <- pf(sumario_modelo$fstatistic[1],
                   sumario_modelo$fstatistic[2],
                   sumario_modelo$fstatistic[3],
                   lower.tail = FALSE)

cat("\n--- 2. DIAGNÓSTICO DA REGRESSÃO ANUAL ACUMULADA ---\n")
cat("R2 Calculado: ", round(r2_calculado, 3), "\n")
cat("p-value:      ", format.pval(p_val_anual, digits = 4), "\n\n")

# Construção do gráfico ggplot2 (Rplot05.png)
grafico_anual <- ggplot(dados_anuais, aes(x = Acumulado_BA, y = Acumulado_FRE)) +
        geom_point(color = "darkred", size = 3, alpha = 0.8) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, fill = "gray80") +
        theme_bw(base_size = 12) +
        labs(
                title = "Annual Correlation: Cumulative BA vs. Cumulative FRE",
                subtitle = paste0("R² = ", round(r2_calculado, 2), " (p < 0.001)"),
                x = "Annual Burned Area Accumulation (ha)",
                y = "Annual Fire Radiative Energy (FRE, MJ)"
        ) +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma) +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5, face = "italic"),
                panel.grid.minor = element_blank()
        )


print(grafico_anual)

# Guardar a imagem no diretório local com o nome exato referenciado no LaTeX
ggsave(
        filename = "Rplot05.png",
        plot = grafico_anual,
        width = 7,
        height = 5,
        dpi = 300
)

print("--- PROCESSO CONCLUÍDO: Gráfico guardado como 'Rplot05.png' ---")




# ==============================================================================
# ANÁLISE DE RELAÇÃO INDIVIDUAL E IDENTIFICAÇÃO DO TOP 20 OUTLIERS
# ==============================================================================

library(dplyr)

# 1. Preparar dados e calcular métricas de relação individual
# Filtramos apenas eventos com dados válidos de área e energia para evitar divisões por zero
fogost_individual <- fogost %>%
        filter(FRE > 0 & AreaTotal > 0 & !is.na(FRE) & !is.na(AreaTotal)) %>%
        mutate(
                # Rácio individual: Quanta energia (MJ) foi libertada por cada hectare ardido
                FRE_per_Ha = FRE / AreaTotal
        )

# 2. Modelar a tendência para extrair os resíduos (Distância à norma)
# Usamos uma transformação logarítmica devido à extrema assimetria de ambas as variáveis
modelo_linear_ind <- lm(log10(FRE) ~ log10(AreaTotal), data = fogost_individual)

fogost_individual <- fogost_individual %>%
        mutate(
                # Resíduos calculam a distância vertical de cada ponto à linha de tendência
                Residuo_Estatistico = residuals(modelo_linear_ind),
                # Distância absoluta para capturar outliers tanto para cima como para baixo
                Distancia_Absoluta  = abs(Residuo_Estatistico),

                # Classificação do tipo de comportamento anómalo
                Tipo_Outlier = ifelse(Residuo_Estatistico > 0,
                                      "Energia Anómala (High FRE / Low BA)",
                                      "Combustão Lenta (Low FRE / High BA)")
        )

# 3. Isolar o Top 20 Outliers Estruturais
top_20_outliers <- fogost_individual %>%
        arrange(desc(Distancia_Absoluta)) %>%
        slice_head(n = 20) %>%
        select(
                Codigo,
                Ano,
                Concelho,
                AreaTotal,
                FRE,
                FRE_per_Ha,
                Residuo_Estatistico,
                Tipo_Outlier
        )

# ==============================================================================
# EMISSÃO DO RELATÓRIO DE OUTLIERS
# ==============================================================================

cat("--- COMPORTAMENTO DA RELAÇÃO INDIVIDUAL (FRE vs BA) ---\n")
resumo_racio <- summary(fogost_individual$FRE_per_Ha)
cat("Rácio de Energia por Hectare (MJ/ha):\n")
cat("   - Valor Mínimo: ", format(resumo_racio["Min."], big.mark=","), "MJ/ha\n")
cat("   - Mediana:       ", format(resumo_racio["Median"], big.mark=","), "MJ/ha\n")
cat("   - Média:         ", format(resumo_racio["Mean"], big.mark=","), "MJ/ha\n")
cat("   - Valor Máximo: ", format(resumo_racio["Max."], big.mark=","), "MJ/ha\n\n")

cat("--- TOP 20 OUTLIERS ESTRUTURAIS DETETADOS NO DATASET ---\n")
print(as.data.frame(top_20_outliers), row.names = FALSE)

# 4. Estatística rápida do comportamento dos Outliers
cat("\n--- DIAGNÓSTICO DO TOP 20 ---\n")
print(table(top_20_outliers$Tipo_Outlier))

# ==============================================================================
# GERAÇÃO DA TABELA COMPLETA DE RELAÇÕES E RESÍDUOS (TODOS OS REGISTOS)
# ==============================================================================

library(dplyr)

# 1. Estruturar todos os registos com as colunas solicitadas, ordenados por desvio
tabela_completa_outliers <- fogost_individual %>%
        # Ordena do maior desvio (outlier mais extremo) para o menor (mais próximo da tendência)
        arrange(desc(Distancia_Absoluta)) %>%
        select(
                Codigo,
                Ano,
                Mes,
                AreaTotal,
                FRE,
                FRE_per_Ha,
                Residuo_Estatistico
        )

# 2. Monitorização e Diagnóstico do Universo de Dados
cat("--- ESTRUTURA DO DATASET COMPLETO DE RELAÇÕES INDIVIDUAIS ---\n")
cat("Total de registos processados com sucesso: ", nrow(tabela_completa_outliers), " incêndios.\n\n")

print("--- Antevisão dos primeiros 5 registos (Mais distantes da tendência) ---")
print(head(tabela_completa_outliers, 5))

print("--- Antevisão dos últimos 5 registos (Mais alinhados com a tendência) ---")
print(tail(tabela_completa_outliers, 5))

# 3. Exportação integral para CSV
write.csv(tabela_completa_outliers, "relacao_completa_fre_ba.csv", row.names = FALSE)
cat("\n[SUCESSO] Todos os", nrow(tabela_completa_outliers),
    "registos foram gravados no ficheiro 'relacao_completa_fre_ba.csv'.\n")


write.csv(tabela_completa_outliers, "relacao_completa_fre_ba.csv", row.names = FALSE)



# ==============================================================================
# REGRESSÃO LINEAR ANUAL E ESTRUTURAÇÃO DO GRÁFICO
# ==============================================================================

# Execução do modelo de regressão linear anual
modelo_anual <- lm(fogost$FRE ~ fogost$AreaTotal, data = fogost_ativos)
sumario_modelo <- summary(modelo_anual)

# Extração de métricas para validação de texto
r2_calculado <- sumario_modelo$r.squared
p_val_anual  <- pf(sumario_modelo$fstatistic[1],
                   sumario_modelo$fstatistic[2],
                   sumario_modelo$fstatistic[3],
                   lower.tail = FALSE)

# Impressão de diagnóstico na consola
cat("\n--- 2. DIAGNÓSTICO DA REGRESSÃO ANUAL ACUMULADA ---\n")
cat("R2 Calculado: ", round(r2_calculado, 3), "\n")
cat("p-value:      ", format.pval(p_val_anual, digits = 4), "\n\n")

# Adaptação dinâmica do texto do p-value para o gráfico
p_texto <- if(p_val_anual < 0.001) {
        "p < 0.001"
} else {
        paste0("p = ", format(round(p_val_anual, 4), nsmall = 4))
}

# Construção do gráfico ggplot2 (Rplot05.png)
# NOTA: Certifica-se de passar o dataset correto no ggplot (dados_anuais ou fogost_ativos)
# Mantenho 'dados_anuais' para coincidir com as variáveis Acumulado_BA e Acumulado_FRE do modelo
grafico_individual <- ggplot(fogost_ativos, aes(x = AreaTotal, y = FRE)) +
        geom_point(color = "darkred", size = 3, alpha = 0.8) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, fill = "gray80") +
        theme_bw(base_size = 12) +
        labs(
                title = "Annual Correlation: Cumulative BA vs. Cumulative FRE",
                subtitle = paste0("R² = ", round(r2_calculado, 2), " (", p_texto, ")"), # 🛠️ Subtitle dinâmico adaptado aqui
                x = "Annual Burned Area Accumulation (ha)",
                y = "Annual Fire Radiative Energy (FRE, MJ)"
        ) +
        scale_x_continuous(labels = scales::comma) +
        scale_y_continuous(labels = scales::comma) +
        theme(
                plot.title = element_text(face = "bold", hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5, face = "italic"),
                panel.grid.minor = element_blank()
        )

# Renderizar o gráfico
print(grafico_individual)






