library(ggplot2)



# Select specific columns
rd2 <- dailydistrict_fires %>%
        select(Data, Distrito,  NInc, AreaTotalEstimado, Mean_DuracaoEstimado, Mean_HaHoraEstimado, meanFWI, Mean_fwi, meanhFWI)




rd2 <- rd2 %>%
        mutate(NInc = ifelse(is.na(NInc), 0, NInc),
               AreaTotalEstimado = ifelse(is.na(AreaTotalEstimado), 0, AreaTotalEstimado),
               Mean_DuracaoEstimado = ifelse(is.na(Mean_DuracaoEstimado), 0, Mean_DuracaoEstimado),
               Mean_HaHoraEstimado = ifelse(is.na(Mean_HaHoraEstimado), 0, Mean_HaHoraEstimado),
               Mean_fwi = ifelse(is.na(Mean_fwi), meanFWI, Mean_fwi)
               
               )

head(daily_fires)

#---------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(purrr)



# Criar a coluna do ano
daily_fires <- daily_fires %>%
        mutate(Ano = format(as.Date(Data), "%Y"))

# Função para ajustar o modelo de potência e calcular métricas de ajuste
ajustar_potencia <- function(df) {
        tryCatch({
                if (n_distinct(df$Freq) < 2) {
                        message("Ano ", unique(df$Ano), " tem valores constantes de Freq. Pulando...")
                        return(tibble(Ano = unique(df$Ano), a = NA, b = NA, RMSE = NA, MAE = NA, r = NA))
                }
                
                # Ajuste do modelo
                modelo <- nls(Freq ~ a * NInc^b, data = df, start = list(a = 1, b = -1))
                coeficientes <- coef(modelo)
                
                # Previsões do modelo
                df$pred <- predict(modelo, newdata = df)
                
                # Cálculo dos indicadores de ajuste
                rmse <- sqrt(mean((df$Freq - df$pred)^2))
                mae <- mean(abs(df$Freq - df$pred))
                r <- cor(df$Freq, df$pred, use = "complete.obs")
                
                tibble(Ano = unique(df$Ano), a = coeficientes["a"], b = coeficientes["b"], RMSE = rmse, MAE = mae, r = r)
        }, error = function(e) {
                message("Erro no ajuste para o ano ", unique(df$Ano), ": ", e$message)
                tibble(Ano = unique(df$Ano), a = NA, b = NA, RMSE = NA, MAE = NA, r = NA)
        })
}

# Criar tabela de frequências para cada ano
tabela_freq <- daily_fires %>%
        count(Ano, NInc, name = "Freq")

# Aplicar a função para cada ano e armazenar os coeficientes
parametros_potencia <- tabela_freq %>%
        group_split(Ano) %>%
        map_dfr(ajustar_potencia)

# Exibir os parâmetros ajustados
print(parametros_potencia)



library(viridis)

# Criar um dataframe com previsões para cada ano
gerar_curvas <- function(parametros) {
        if (any(is.na(parametros$a))) return(NULL)  # Ignorar anos sem ajuste válido
        
        tibble(
                Ano = parametros$Ano,
                NInc = seq(1, max(tabela_freq$NInc, na.rm = TRUE), length.out = 100),
                Freq_Pred = parametros$a * seq(1, max(tabela_freq$NInc, na.rm = TRUE), length.out = 100)^parametros$b
        )
}

# Converter Ano para numérico antes do gráfico
curvas_ajustadas <- curvas_ajustadas %>%
        mutate(Ano = as.numeric(Ano))

# Gerar o gráfico com cores em gradiente corrigido
ggplot(curvas_ajustadas, aes(x = NInc, y = Freq_Pred, color = Ano, group = Ano)) +
        geom_line(size = 1) +
        scale_color_viridis_c(option = "plasma", direction = -1) +  # Gradiente de cores por ano
        labs(
                title = "Curvas Ajustadas da Equação Potência ao Longo dos Anos",
                x = "Número de Incêndios Diários (NInc)",
                y = "Frequência Estimada",
                color = "Ano"
        ) +
        theme_minimal()





#-----------------------------------------------------------------------------------------------------





# Create histograms of NInc by Distrito
ggplot(rd2, aes(x = NInc)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +  # Histogram with binwidth of 1
        facet_wrap(~ Distrito, scales = "free") +  # Create a histogram for each Distrito
        labs(title = "Histograms of NInc by Distrito", x = "NInc", y = "Count") +  # Labels
        theme_minimal()  # Clean theme




# Create scatter plot with a trend line
ggplot(result, aes(x = NInc, y = FWI)) +
        geom_point(color = "blue", alpha = 0.6) +  # Scatter plot of points
        geom_smooth(method = "lm", color = "red", se = TRUE) +  # Trend line without confidence interval
        labs(title = "NInc vs Mean FWI Distribution with Trend Line",
             x = "NInc",
             y = "Mean FWI") +
        theme_minimal()  # Use a clean theme

model <- lm(Mean_fwi ~ FWI, data = result)
result$Predicted_Mean_fwi <- predict(model, newdata = result2)



# Create bins for NInc to categorize them
result$NInc_Bin <- cut(result$NInc, breaks = c(0, 5, 10, 20, 50, 100, 200, 500, Inf), 
                        labels = c("0-5", "6-10", "11-20", "21-50", "51-100", "101-200", "201-500", "500+"))

# Plot the histogram faceted by NInc_Bin
ggplot(result, aes(x = FWI)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
        facet_wrap(~ NInc_Bin, scales = "free_y") +
        labs(title = "Distribution of FWI by NInc Categories", x = "FWI", y = "Count") +
        theme_minimal()
ggplot(result, aes(x = NInc_Bin, y = FFMC)) +
        geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
        labs(title = "Boxplot of FWI by NInc Categories", x = "NInc Categories", y = "FFMC") +
        theme_minimal()
ggplot(result, aes(x = NInc_Bin, y = FWI)) +
        geom_violin(fill = "purple", color = "black", alpha = 0.7) +
        labs(title = "Violin Plot of FWI by NInc Categories", x = "NInc Categories", y = "FWI") +
        theme_minimal()
ggplot(result, aes(x = NInc, y = FWI)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", color = "blue", se = FALSE) +
        labs(title = "Scatter Plot of FWI vs. NInc", x = "NInc", y = "FWI") +
        theme_minimal()
ggplot(result, aes(x = FWI, fill = NInc_Bin)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot of FWI by NInc Categories", x = "FWI", y = "Density") +
        theme_minimal()


# Create bins for AreaTotal to categorize them
result$AreaTotal_Bin <- cut(result$AreaTotalEstimado, 
                             breaks = c(0, 10, 50, 100, 500, 1000, 2000, 5000, 7000, Inf), 
                             labels = c("0-10", "11-50", "51-100", "101-500", 
                                        "501-1000", "1001-2000", "2001-5000", "5001-7000", "7000+"))

# Transform AreaTotalEstimado to log10(AreaTotalEstimado)
result$log10_AreaTotal <- log10(result$AreaTotalEstimado)

# Create bins for log10_AreaTotal to categorize them
result$AreaTotal_Bin <- cut(result$log10_AreaTotal, 
                             breaks = c(-Inf, log10(10), log10(50), log10(100), log10(500), 
                                        log10(1000), log10(2000), log10(5000), log10(7000), Inf), 
                             labels = c("0-10", "11-50", "51-100", "101-500", 
                                        "501-1000", "1001-2000", "2001-5000", "5001-7000", "7000+"))

ggplot(result, aes(x = FWI)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
        facet_wrap(~ AreaTotal_Bin, scales = "free_y") +
        labs(title = "Distribution of FWI by AreaTotal Categories", x = "FWI", y = "Count") +
        theme_minimal()

ggplot(result, aes(x = AreaTotal_Bin, y = FWI)) +
        geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
        labs(title = "Boxplot of FWI by AreaTotal Categories", x = "AreaTotal Categories", y = "FWI") +
        theme_minimal()

ggplot(result2, aes(x = AreaTotal_Bin, y = FWI)) +
        geom_violin(fill = "purple", color = "black", alpha = 0.7) +
        labs(title = "Violin Plot of FWI by AreaTotal Categories", x = "AreaTotal Categories", y = "FWI") +
        theme_minimal()

ggplot(result, aes(x = AreaTotalEstimado, y = FWI)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", color = "blue", se = FALSE) +
        labs(title = "Scatter Plot of FWI vs. AreaTotal", x = "AreaTotal", y = "FWI") +
        theme_minimal()

ggplot(result, aes(x = FWI, fill = AreaTotal_Bin)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot of FWI by AreaTotal Categories", x = "FWI", y = "Density") +
        theme_minimal()

#-----------time series analysis


library(xts)


rd <- result[, c("Data", "NInc", "FWI")]
rd[is.na(rd)] <- 0

# Convert to a time series object for `NInc` and `FWI`


# Create a time series object using xts (Indexing by Date)
NInc_ts <- xts(rd$NInc, order.by = rd$Data)
FWI_ts <- xts(rd$FWI, order.by = rd$Data)

# Plot the time series
plot(NInc_ts, main = "Number of Fires Over Time", ylab = "NInc", xlab = "Date")
plot(FWI_ts, main = "Fire Weather Index Over Time", ylab = "FWI", xlab = "Date")



# Plot both time series together
plot(NInc_ts, type = "l", col = "blue", lwd = 2, ylab = "Value", xlab = "Time", main = "Time Series Comparison")
lines(FWI_ts, col = "red", lwd = 2)

# Add a legend for clarity
legend("topright", legend = c("NInc", "FWI"), col = c("blue", "red"), lwd = 2)

# Perform Pearson correlation test between NInc and FWI
cor_test <- cor.test(NInc_ts, FWI_ts, use = "complete.obs")

# Extract the correlation coefficient
correlation <- cor_test$estimate

# Extract the p-value
p_value <- cor_test$p.value

# Print results
print(paste("Correlation between NInc and FWI:", correlation))
print(paste("P-value for the correlation test:", p_value))


# Convert your data to time series if it's not already
NInc_ts <- ts(rd$NInc, frequency = 365)  # Example, daily data with yearly frequency
FWI_ts <- ts(rd$FWI, frequency = 365)    # Example, daily data with yearly frequency
# Cross-correlation between NInc and FWI
ccf(NInc_ts, FWI_ts, main = "Cross-Correlation between NInc and FWI")


# Load the urca package
# install.packages("urca")  # Uncomment if not installed
library(urca)

# Perform cointegration test using the Engle-Granger method
coint_test <- ca.jo(cbind(NInc_ts, FWI_ts), type = "trace", ecdet = "none", K = 2)
summary(coint_test)

# Load necessary package
# install.packages("lmtest")  # Uncomment if not installed
library(lmtest)

# Perform Granger Causality test
grangertest(NInc_ts ~ FWI_ts, order = 1)  # Test if FWI Granger-causes NInc
grangertest(FWI_ts ~ NInc_ts, order = 1)  # Test if NInc Granger-causes FWI








# Use the ts object for decomposition
NInc_decomposed <- decompose(ts(rd$NInc, frequency = 365), type = "additive")
plot(NInc_decomposed)


FWI_decomposed <- decompose(ts(rd$FWI, frequency = 365), type = "additive")
plot(FWI_decomposed)


# Extract trend components
NInc_trend <- NInc_decomposed$trend
FWI_trend <- FWI_decomposed$trend



# Plot the trends on the same plot
plot(NInc_trend, type = "l", col = "blue", lwd = 2, 
     ylab = "Trend", xlab = "Time", main = "Trend Comparison of NInc and FWI")

# Add the FWI trend to the same plot
lines(FWI_trend, col = "red", lwd = 2)

# Add a legend for clarity
legend("topright", legend = c("NInc Trend", "FWI Trend"), col = c("blue", "red"), lwd = 2)

# Normalize the trends (if on different scales)
NInc_trend_norm <- (NInc_trend - min(NInc_trend, na.rm = TRUE)) / (max(NInc_trend, na.rm = TRUE) - min(NInc_trend, na.rm = TRUE))
FWI_trend_norm <- (FWI_trend - min(FWI_trend, na.rm = TRUE)) / (max(FWI_trend, na.rm = TRUE) - min(FWI_trend, na.rm = TRUE))

# Plot the normalized trends
plot(NInc_trend_norm, type = "l", col = "blue", lwd = 2, 
     ylab = "Normalized Trend", xlab = "Time", main = "Normalized Trend Comparison of NInc and FWI")
lines(FWI_trend_norm, col = "red", lwd = 2)
legend("topright", legend = c("NInc Trend", "FWI Trend"), col = c("blue", "red"), lwd = 2)

# Create a data frame with the normalized trends
trend_data <- data.frame(Date = time(NInc_trend), 
                         NInc_trend_norm = NInc_trend_norm, 
                         FWI_trend_norm = FWI_trend_norm)


# Create scatter plot with a trend line
ggplot(trend_data, aes(x = NInc_trend_norm, y = FWI_trend_norm)) +
        geom_point(color = "blue", alpha = 0.6) +  # Scatter plot of points
        geom_smooth(method = "lm", color = "red", se = TRUE) +  # Trend line without confidence interval
        labs(title = "NInc vs Mean FWI Distribution with Trend Line",
             x = "NInc",
             y = "FWI") +
        theme_minimal()  # Use a clean theme


# Write the data frame to a CSV file
write.csv(trend_data, "dailytrend.csv", row.names = FALSE)


#--------------------analise de dias criticos-----------------

# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(lubridate)

# Assuming the dataset is 'df' with columns: Date, Number_of_Fires, Burnt_Area

# Convert the Date column (if it's not in Date format already)
df <- daily_fires %>%
        mutate(Data = as.Date(Data))

df <- df %>%
        mutate(Ano = year(Data),   # Extract year
               Mes = month(Data)) %>%  # Extract month
        filter(Ano <= 2025)  # Keep only years up to 2024

# Definir um threshold para dias críticos (exemplo: acima do percentil 95%)
threshold_fires <- quantile(df$NInc, 0.95, na.rm = TRUE)
threshold_area <- quantile(df$AreaTotalEstimado, 0.95, na.rm = TRUE)
threshold_fwiInc <- quantile(df$FWI, 0.95, na.rm = TRUE)
threshold_fwi <- quantile(df$mean_fwi, 0.95, na.rm = TRUE)
# Definir um threshold para os dias menos críticos (exemplo: abaixo do percentil 5%)
thresholdmin_fires <- quantile(df$NInc, 0.32, na.rm = TRUE)
thresholdmin_area <- quantile(df$AreaTotalEstimado, 0.32, na.rm = TRUE)
thresholdmin_fwi <- quantile(df$FWI, 0.32, na.rm = TRUE)



# Identificar dias críticos
df <- df %>%
        mutate(CriticoInc = ifelse(NInc >= threshold_fires , 1, 0),
               CriticoAreaTotal = ifelse( AreaTotalEstimado >= threshold_area,1,0),
               CriticoFWi = ifelse( FWI >= threshold_fwi,1,0),
               FavoravelInc = ifelse(NInc <= thresholdmin_fires , 1, 0),  # Menor nº de incêndios
               FavoravelAreaTotal = ifelse(AreaTotalEstimado <= thresholdmin_area,1,0),  # Menor área queimada
               FavoravelFWi = ifelse(FWI <= thresholdmin_fwi,1,0)  # Menores valores de FWI
               )

# Contar a frequência de dias críticos por ano, removendo NA
dias_criticos_por_ano <- df %>%
       
        group_by(Ano) %>%
        summarise(
                CriticoInc = sum(CriticoInc == 1, na.rm = TRUE),         # Count the number of critical days (Incêndios)
                CriticoAreaTotal = sum(CriticoAreaTotal == 1, na.rm = TRUE),  # Sum of the burnt area on critical days
                Criticofwi = sum(CriticoFWi == 1, na.rm = TRUE),  # Sum of the burnt area on critical days
                NRF=sum(NInc, na.rm = TRUE),
                BA=sum(AreaTotalEstimado, na.rm = TRUE),
                FavoravelInc = sum(FavoravelInc == 1, na.rm = TRUE),         
                FavoravelAreaTotal = sum(FavoravelAreaTotal == 1, na.rm = TRUE),  
                Favoravelfwi = sum(FavoravelFWi == 1, na.rm = TRUE)  
                
                
        )


# Criar gráfico com ggplot
ggplot(dias_criticos_por_ano, aes(x = Ano)) + 
        geom_smooth(aes(y = CriticoInc, color = "Critical days in DNRF"), method = "loess", se = FALSE, span = 0.1, size = 1) +  
        geom_smooth(aes(y = CriticoAreaTotal, color = "Critical days in DBA"), method = "loess", se = FALSE, span = 0.1, size = 1) +  
        geom_smooth(aes(y = Criticofwi, color = "Critical days in FWI"), method = "loess", se = FALSE, span = 0.1, size = 1, linetype = "dotted") +  
        labs(title = "Frequency of Critical Days for Daily Number of Fires, Burned Area and FWI", 
             x = "Year", 
             y = "Nº of Days per Year", 
             color = "Legend:") +  
        scale_color_manual(values = c("Critical days in DNRF" = "gray10", "Critical days in DBA" = "gray60", "Critical days in FWI" = "gray0")) +  
        scale_x_continuous(breaks = seq(min(dias_criticos_por_ano$Ano), max(dias_criticos_por_ano$Ano), by = 2)) +  # X-axis every 2 years
        theme_minimal() +  
        theme(plot.title = element_text(hjust = 0.5))  # Center title





# Criar gráfico com ggplot
ggplot(dias_criticos_por_ano, aes(x = Ano)) + 
        geom_smooth(aes(y = FavoravelInc, color = "Favorable days in DNRF"), method = "loess", se = FALSE, span = 0.1, size = 1) +  
        geom_smooth(aes(y = FavoravelAreaTotal, color = "Favorable days in DBA"), method = "loess", se = FALSE, span = 0.1, size = 1) +  
        geom_smooth(aes(y = Favoravelfwi, color = "Favorable days in FWI"), method = "loess", se = FALSE, span = 0.1, size = 1, linetype = "dotted") +  
        labs(title = "Frequency of Favorable Days for Daily Number of Fires, Burned Area and FWI", 
             x = "Year", 
             y = "Nº of Days per Year", 
             color = "Legend:") +  
        scale_color_manual(values = c("Favorable days in DNRF" = "gray10", "Favorable days in DBA" = "gray60", "Favorable days in FWI" = "gray0")) +  
        scale_x_continuous(breaks = seq(min(dias_menos_criticos_por_ano$Ano), max(dias_menos_criticos_por_ano$Ano), by = 2)) +  # X-axis every 2 years
        theme_minimal() +  
        theme(plot.title = element_text(hjust = 0.5))  # Center title


