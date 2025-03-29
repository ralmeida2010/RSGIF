# Write data to CSV+
#write.csv(fogos, file = "fogos.csv", quote = TRUE, col.names = TRUE,  fileEncoding = "UTF-8", na = "")
# Pacotes necessários
# Pacotes necessários
# Pacotes necessários
library(ggplot2)
library(dplyr)
library(scales)  # Para formatar os rótulos percentuais


fogos <- fogos %>%
        mutate(ClasseArea = case_when(
                AreaTotal <= 1 ~ "[0-1ha]",
                AreaTotal > 1 & AreaTotal <= 10 ~ "]1-10ha]",
                AreaTotal > 10 & AreaTotal <= 100 ~ "]10-100ha]",
                AreaTotal > 100 & AreaTotal <= 1000 ~ "]100-1000ha]",
                AreaTotal > 1000 ~ "]1000-+1000ha]"
        ))


#fogos <- fogos %>%
#        mutate(ClasseArea = case_when(
#                AreaTotal <= 10000 ~ "[0-10000ha]",
#                AreaTotal > 10000 ~ "]10000-+10000ha]"
#        ))


# Calcular a contagem total de incêndios por ano
df_percent <- fogos %>%
        group_by(Ano, ClasseArea) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(Ano) %>%
        mutate(percent = (n / sum(n)) * 100)

# Encontrar a posição final para os labels
df_labels <- df_percent %>%
        group_by(ClasseArea) %>%
        filter(Ano == max(Ano))  # Último ano disponível para cada ClasseArea

# Criar o gráfico
ggplot(df_percent, aes(x = Ano, y = percent, color = ClasseArea, group = ClasseArea)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.8) +  # Linhas de tendência
        scale_color_grey(start = 0.2, end = 0.8) +  # Tons de cinza
        scale_y_log10(labels = label_percent(scale = 1)) +  # Escala log e valores percentuais
        annotation_logticks(sides = "l") +  # Adiciona ticks ao eixo Y
        labs(
                title = "Proporção de incêndios por Classe de Área ao longo dos anos",
                x = "Ano",
                y = "Percentagem (%) (escala log10)"
        ) +
        theme_minimal() +
        theme(legend.position = "none") +  # Remover legenda
        geom_text(data = df_labels, 
                  aes(x = Ano, y = percent, label = ClasseArea), 
                  hjust = 0, vjust = 0.5,  # Melhor alinhamento dentro do gráfico
                  size = 3, fontface = "bold", nudge_x = 1)  # Reduz tamanho e move para dentro do gráfico

#---------------------------------------------------------------------------------------------------------
# Calcular o número de incêndios por ano e por ClasseArea
df_count <- fogos %>%
        group_by(Ano, ClasseArea) %>%
        summarise(n = n(), .groups = "drop")

# Criar uma função para calcular o declive da reta de ajuste nos 10 anos anteriores
calc_slope <- function(df, window = 10) {
        df %>%
                arrange(Ano) %>%
                mutate(slope = map_dbl(seq_len(n()), ~ {
                        if (.x > window) {
                                modelo <- lm(n ~ Ano, data = df[(.x - window + 1):.x, ])
                                coef(modelo)[2]  # Retorna o coeficiente angular (declive)
                        } else {
                                NA_real_
                        }
                })) %>%
                filter(!is.na(slope))
}

# Aplicar a função para calcular o declive estratificado por ClasseArea
df_slope <- df_count %>%
        group_by(ClasseArea) %>%
        do(calc_slope(.)) %>%
        ungroup()

# Criar o gráfico do declive ao longo do tempo, estratificado por ClasseArea
ggplot(df_slope, aes(x = Ano, y = slope, color = ClasseArea, group = ClasseArea)) +
        geom_line(size = 1) +  # Exibe a linha baseada nos dados
        geom_point(size = 2) +
        scale_color_manual(values = grey.colors(length(unique(df_slope$ClasseArea)))) +  # Usar tons de cinza
        labs(
                title = "Declive da tendência do número de incêndios nos 10 anos anteriores por Classe de Área",
                x = "Ano",
                y = "Declive da reta de ajuste (10 anos)"
        ) +
        theme_minimal() +
        theme(
                legend.position = "none",  # Remove a legenda
                strip.text = element_text(size = 10),  # Ajuste no tamanho do título de cada gráfico
                plot.title = element_text(hjust = 0.5),  # Centraliza o título
                axis.text.x = element_text(angle = 45, hjust = 1)  # Ajuste de leitura dos valores do eixo X
        ) +
        facet_wrap(~ClasseArea, scales = "free_y") +  # Escalas livres no eixo Y
        scale_x_continuous(breaks = seq(min(df_slope$Ano), max(df_slope$Ano), by = 2))  # Eixo X de 2 em 2 anos

#---------------------------------------------------------------------


library(dplyr)
library(purrr)
library(ggplot2)

# 1. Calcular o número de incêndios por ano e por ClasseArea
df_count <- fogos %>%
        group_by(Ano, ClasseArea) %>%
        summarise(n = n(), .groups = "drop")

# 2. Função para calcular slope e RMSE
calc_slope_rmse <- function(df, window = 10) {
        df %>%
                arrange(Ano) %>%
                mutate(
                        slope = map_dbl(seq_len(n()), ~ {
                                if (.x > window) {
                                        modelo <- lm(n ~ Ano, data = df[(.x - window + 1):.x, ])
                                        coef(modelo)[2]  # Coeficiente angular (slope)
                                } else {
                                        NA_real_
                                }
                        }),
                        rmse = map_dbl(seq_len(n()), ~ {
                                if (.x > window) {
                                        modelo <- lm(n ~ Ano, data = df[(.x - window + 1):.x, ])
                                        sqrt(mean(residuals(modelo)^2))  # RMSE
                                } else {
                                        NA_real_
                                }
                        })
                ) %>%
                filter(!is.na(slope) & !is.na(rmse))  # Remove linhas com NA
}

# 3. Aplicar a função para calcular slope e RMSE estratificado por ClasseArea
df_slope_rmse <- df_count %>%
        group_by(ClasseArea) %>%
        do(calc_slope_rmse(.)) %>%
        ungroup()

# 4. Tabela com RMSE ao longo do tempo para cada classe
tabela_rmse <- df_slope_rmse %>%
        select(Ano, ClasseArea, rmse) %>%
        arrange(ClasseArea, Ano)

# Exibir a tabela
print(tabela_rmse)

# 5. Gráfico do RMSE ao longo do tempo para cada classe
ggplot(df_slope_rmse, aes(x = Ano, y = rmse, color = ClasseArea, group = ClasseArea)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = grey.colors(length(unique(df_slope_rmse$ClasseArea)))) +
        labs(
                title = "RMSE da regressão móvel do número de incêndios por Classe de Área",
                x = "Ano",
                y = "RMSE"
        ) +
        theme_minimal() +
        theme(
                legend.position = "none",
                strip.text = element_text(size = 10),
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        facet_wrap(~ClasseArea, scales = "free_y") +
        scale_x_continuous(breaks = seq(min(df_slope_rmse$Ano), max(df_slope_rmse$Ano), by = 2))

#---------------------------------------------------------------------
# Remover valores NA e garantir que 'Ano' seja numérico
df_count <- df_count %>%
        filter(!is.na(Ano)) %>%
        mutate(Ano = as.numeric(Ano))

# Criar o gráfico
ggplot(df_count, aes(x = Ano, y = n, color = ClasseArea, group = ClasseArea)) +
        geom_line(size = 1, alpha = 0.7) +
        geom_point(size = 2, alpha = 0.7) +
        scale_color_grey(start = 0.2, end = 0.8) +
        scale_y_log10(
                labels = scales::label_number(scale_cut = scales::cut_short_scale()),
                n.breaks = 6
        ) +
        annotation_logticks(sides = "l") +
        labs(
                title = "Numerical evolution of rural fires by Class of Burned Area",
                x = "Year",
                y = "Num of rural fires (scale log10)"
        ) +
        theme_minimal() +
        theme(
                legend.position = "none",
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5)  # Centraliza o título
        ) +
        geom_text(
                data = df_labels,
                aes(label = ClasseArea),
                hjust = 0,
                vjust = 0.5,
                size = 3.5,
                fontface = "bold",
                nudge_x = 0.5,  # Ajuste fino para evitar cortes
                check_overlap = TRUE
        ) +
        coord_cartesian(
                xlim = c(min(df_count$Ano), max(df_count$Ano) + 3),  # Espaço para labels
                ylim = c(1, 100000),  # Define o valor máximo de Y como 100.000
                clip = "off"
        ) +
        scale_x_continuous(breaks = seq(min(df_count$Ano), max(df_count$Ano), by = 2))  # Definir intervalos de 1 no eixo X
