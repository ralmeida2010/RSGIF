#--------------------------------------------------------------
# Optional: Save the dataset to a CSV file (currently commented)
# write.csv(fogos, file = "data/fogos.csv", quote = TRUE, col.names = TRUE,  fileEncoding = "UTF-8", na = "")

#--------------------------------------------------------------
# Load required libraries
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(scales)    # For formatting axis labels as percentages

#--------------------------------------------------------------
# ZONE 1: Categorize fire events by total burned area

fogos <- fogos %>%
        mutate(ClasseArea = case_when(
                AreaTotal <= 1 ~ "[0-1ha]",                    # Fires up to 1 hectare
                AreaTotal > 1 & AreaTotal <= 10 ~ "]1-10ha]",  # Fires between 1 and 10 ha
                AreaTotal > 10 & AreaTotal <= 100 ~ "]10-100ha]", # Fires between 10 and 100 ha
                AreaTotal > 100 & AreaTotal <= 1000 ~ "]100-1000ha]", # Fires between 100 and 1000 ha
                AreaTotal > 1000 ~ "]1000-+1000ha]"            # Fires greater than 1000 ha
        ))

# Alternative classification (commented out)
# fogos <- fogos %>%
#   mutate(ClasseArea = case_when(
#     AreaTotal <= 10000 ~ "[0-10000ha]",
#     AreaTotal > 10000 ~ "]10000-+10000ha]"
#   ))

#--------------------------------------------------------------
# ZONE 2: Calculate the percentage of fires in each area class per year

df_percent <- fogos %>%
        group_by(Ano, ClasseArea) %>%
        summarise(n = n(), .groups = "drop") %>%        # Count number of fires
        group_by(Ano) %>%
        mutate(percent = (n / sum(n)) * 100)            # Convert to percentage

# Extract label positions for latest year for plotting
df_labels <- df_percent %>%
        group_by(ClasseArea) %>%
        filter(Ano == max(Ano))                         # Keep only last year

#--------------------------------------------------------------
# ZONE 3: Plot percentage of fires by area class over time

ggplot(df_percent, aes(x = Ano, y = percent, color = ClasseArea, group = ClasseArea)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.8) +  # Linear trend
        scale_color_grey(start = 0.2, end = 0.8) +                                  # Greyscale palette
        scale_y_log10(labels = label_percent(scale = 1)) +                         # Log-scale Y-axis
        annotation_logticks(sides = "l") +                                         # Log ticks on Y-axis
        labs(
                title = "Proporção de incêndios por Classe de Área ao longo dos anos",
                x = "Ano",
                y = "Percentagem (%) (escala log10)"
        ) +
        theme_minimal() +
        theme(legend.position = "none") +  # Hide legend
        geom_text(data = df_labels, 
                  aes(x = Ano, y = percent, label = ClasseArea), 
                  hjust = 0, vjust = 0.5, size = 3, fontface = "bold", nudge_x = 1)

#--------------------------------------------------------------
# ZONE 4: Count number of fires by year and area class

df_count <- fogos %>%
        group_by(Ano, ClasseArea) %>%
        summarise(n = n(), .groups = "drop")

#--------------------------------------------------------------
# ZONE 5: Calculate trend slope using a 10-year moving window

# Define function to compute slope over rolling window
calc_slope <- function(df, window = 10) {
        df %>%
                arrange(Ano) %>%
                mutate(slope = map_dbl(seq_len(n()), ~ {
                        if (.x > window) {
                                modelo <- lm(n ~ Ano, data = df[(.x - window + 1):.x, ])
                                coef(modelo)[2]
                        } else {
                                NA_real_
                        }
                })) %>%
                filter(!is.na(slope))
}

# Apply slope calculation by area class
df_slope <- df_count %>%
        group_by(ClasseArea) %>%
        do(calc_slope(.)) %>%
        ungroup()

#--------------------------------------------------------------
# ZONE 6: Plot trend slopes over time by area class

ggplot(df_slope, aes(x = Ano, y = slope, color = ClasseArea, group = ClasseArea)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_viridis_d(option = "D", begin = 0.2, end = 0.8) +
        labs(
                title = "Trend slope of the moving regression lines (10-year window) for the number of fires by Burned Area Class",
                x = "Year",
                y = "slope of the moving regression lines (10-year window)"
        ) +
        theme_minimal() +
        theme(
                legend.position = "none",
                strip.text = element_text(size = 10),
                plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                axis.title = element_text(size = 20),
                axis.text = element_text(angle = 45, hjust = 1, size = 17)
        ) +
        facet_wrap(~ClasseArea, scales = "free_y") +
        scale_x_continuous(breaks = seq(min(df_slope$Ano), max(df_slope$Ano), by = 2))

#--------------------------------------------------------------
# ZONE 7: Calculate both slope and RMSE per year using moving window

library(purrr)  # For map_dbl()

# Define function to compute both slope and RMSE
calc_slope_rmse <- function(df, window = 10) {
        df %>%
                arrange(Ano) %>%
                mutate(
                        slope = map_dbl(seq_len(n()), ~ {
                                if (.x > window) {
                                        modelo <- lm(n ~ Ano, data = df[(.x - window + 1):.x, ])
                                        coef(modelo)[2]
                                } else {
                                        NA_real_
                                }
                        }),
                        rmse = map_dbl(seq_len(n()), ~ {
                                if (.x > window) {
                                        modelo <- lm(n ~ Ano, data = df[(.x - window + 1):.x, ])
                                        sqrt(mean(residuals(modelo)^2))
                                } else {
                                        NA_real_
                                }
                        })
                ) %>%
                filter(!is.na(slope) & !is.na(rmse))
}

# Apply function
df_slope_rmse <- df_count %>%
        group_by(ClasseArea) %>%
        do(calc_slope_rmse(.)) %>%
        ungroup()

#--------------------------------------------------------------
# ZONE 8: Table of RMSE values by year and area class

tabela_rmse <- df_slope_rmse %>%
        select(Ano, ClasseArea, rmse) %>%
        arrange(ClasseArea, Ano)
print(tabela_rmse)

#--------------------------------------------------------------
# ZONE 9: Plot RMSE per year by area class

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

#--------------------------------------------------------------
# ZONE 10: Correlation matrix between area classes for different time periods

library(tidyverse)
library(ggcorrplot)

# Add period classification to each year
df_slope <- df_slope %>%
        mutate(Periodo = case_when(
                Ano >= 1990 & Ano <= 1999 ~ "1990-1999",
                Ano >= 2000 & Ano <= 2009 ~ "2000-2009",
                Ano >= 2010 & Ano <= 2019 ~ "2010-2019",
                Ano >= 2020 & Ano <= 2024 ~ "2020-2024"
        ))

# List unique periods
periodos <- unique(na.omit(df_slope$Periodo))

# Function to create correlation plot for a given period
plot_ggcorr <- function(df, periodo) {
        mat_corr <- df %>%
                filter(Periodo == periodo) %>%
                select(Ano, ClasseArea, slope) %>%
                pivot_wider(names_from = ClasseArea, values_from = slope) %>%
                select(-Ano) %>%
                cor(use = "pairwise.complete.obs")
        
        ggcorrplot(
                mat_corr,
                method = "square",
                lab = TRUE,
                lab_size = 4,
                type = "upper",
                title = paste("Trends in the period", periodo)
        ) +
                theme_minimal() +
                theme(
                        plot.title = element_text(size = 16, face = "bold"),
                        axis.title = element_blank(),
                        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                        axis.text.y = element_text(size = 12),
                        axis.ticks = element_blank()
                )
}

# Create list of correlation plots by period
graficos <- lapply(periodos, function(p) plot_ggcorr(df_slope, p))

# Display all correlation plots
library(patchwork)
wrap_plots(graficos, ncol = 2)

#--------------------------------------------------------------
# ZONE 11: Final plot of number of fires per year (log scale)

library(viridis)

ggplot(df_count, aes(x = Ano, y = n, color = ClasseArea, group = ClasseArea)) +
        geom_line(size = 1, alpha = 0.7) +
        geom_point(size = 2, alpha = 0.7) +
        geom_smooth(method = "loess", se = TRUE, linetype = "dashed", size = 0.8) +
        scale_color_viridis_d(option = "D", begin = 0.2, end = 0.8) +
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
        theme_minimal(base_size = 14) +  # Aumenta fonte base
        theme(
                legend.position = "none",
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 14)
        ) +
        geom_text(
                data = df_labels,
                aes(label = ClasseArea),
                hjust = 0,
                vjust = 0.5,
                size = 5,  # Aumentado
                fontface = "bold",
                nudge_x = 0.5,
                check_overlap = TRUE
        ) +
        coord_cartesian(
                xlim = c(min(df_count$Ano), max(df_count$Ano) + 3),
                ylim = c(1, 100000),
                clip = "off"
        ) +
        scale_x_continuous(breaks = seq(min(df_count$Ano), max(df_count$Ano), by = 2))

