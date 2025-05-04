if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

# Select specific columns from the dataset
rd2 <- dailydistrict_fires %>%
        select(Data, Distrito,  DNRF, DBA,  median_fwi, median_hFWI)

# Replace NA values with 0 in DNRF and DBA columns
rd2 <- rd2 %>%
        mutate(DNRF = ifelse(is.na(DNRF), 0, DNRF),
               DBA = ifelse(is.na(DBA), 0, DBA))


#-------Exploratory analysis--------------------------------------------------------------------------------------------

# Create histograms of DNRF by District
ggplot(rd2, aes(x = DNRF)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +  # Histogram with binwidth of 1
        facet_wrap(~ Distrito, scales = "free") +  # Create a histogram for each District
        labs(title = "Histograms of DNRF by District", x = "DNRF", y = "Count") +  # Labels
        theme_minimal()  # Clean theme

# Create scatter plot with a trend line
ggplot(rd2, aes(x = DNRF, y = median_fwi)) +
        geom_point(color = "blue", alpha = 0.6) +  # Scatter plot of points
        geom_smooth(method = "lm", color = "red", se = TRUE) +  # Trend line with confidence interval
        labs(title = "DNRF vs Median FWI Distribution with Trend Line",
             x = "DNRF",
             y = "Median FWI") +
        theme_minimal()  # Use a clean theme

# Fit linear model and predict values
model <- lm(median_fwi ~ DNRF, data = rd2)
rd2$Predicted_Median_fwi <- predict(model, newdata = rd2)

# Create bins for DNRF to categorize them
rd2$DNRF_Bin <- cut(rd2$DNRF, breaks = c(0, 5, 10, 20, 50, 100, 200, 500, Inf), 
                       labels = c("0-5", "6-10", "11-20", "21-50", "51-100", "101-200", "201-500", "500+"))

# Plot the histogram faceted by DNRF_Bin
ggplot(rd2, aes(x = median_fwi)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
        facet_wrap(~ DNRF_Bin, scales = "free_y") +
        labs(title = "Distribution of FWI by DNRF Categories", x = "FWI", y = "Count") +
        theme_minimal()

# Boxplot of FFMC by DNRF categories
ggplot(rd2, aes(x = DNRF_Bin, y = median_hFWI)) +
        geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
        labs(title = "Boxplot of hFWI by DNRF Categories", x = "DNRF Categories", y = "median_hFWI") +
        theme_minimal()

# Violin plot of FWI by DNRF categories
ggplot(rd2, aes(x = DNRF_Bin, y = median_fwi)) +
        geom_violin(fill = "purple", color = "black", alpha = 0.7) +
        labs(title = "Violin Plot of FWI by DNRF Categories", x = "DNRF Categories", y = "FWI") +
        theme_minimal()

# Scatter plot with loess smooth
ggplot(rd2, aes(x = DNRF, y = median_fwi)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", color = "blue", se = FALSE) +
        labs(title = "Scatter Plot of FWI vs. DNRF", x = "DNRF", y = "FWI") +
        theme_minimal()

# Density plot of FWI by DNRF categories
ggplot(rd2, aes(x = median_fwi, fill = DNRF_Bin)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot of FWI by DNRF Categories", x = "FWI", y = "Density") +
        theme_minimal()

# Create bins for Burned Area to categorize them
rd2$DBA_Bin <- cut(rd2$DBA, 
                            breaks = c(0, 10, 50, 100, 500, 1000, 2000, 5000, 7000, Inf), 
                            labels = c("0-10", "11-50", "51-100", "101-500", 
                                       "501-1000", "1001-2000", "2001-5000", "5001-7000", "7000+"))

# Transform AreaTotalEstimado to log10 scale
rd2$log10_DBA <- log10(rd2$DBA)

# Create bins for log10_AreaTotal
rd2$DBA_Bin <- cut(rd2$log10_DBA, 
                            breaks = c(-Inf, log10(10), log10(50), log10(100), log10(500), 
                                       log10(1000), log10(2000), log10(5000), log10(7000), Inf), 
                            labels = c("0-10", "11-50", "51-100", "101-500", 
                                       "501-1000", "1001-2000", "2001-5000", "5001-7000", "7000+"))

# Plot FWI distribution by area categories
ggplot(rd2, aes(x = median_fwi)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
        facet_wrap(~ DBA_Bin, scales = "free_y") +
        labs(title = "Distribution of FWI by  daily burned area Categories", x = "Median FWI", y = "Count") +
        theme_minimal()

# Boxplot of FWI by area categories
ggplot(rd2, aes(x = DBA_Bin, y = median_fwi)) +
        geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
        labs(title = "Boxplot of FWI by daily burned area Categories", x = "Daily burned area Categories", y = "median FWI") +
        theme_minimal()

# Violin plot of FWI by area categories
ggplot(rd2, aes(x = DBA_Bin, y = median_fwi)) +
        geom_violin(fill = "purple", color = "black", alpha = 0.7) +
        labs(title = "Violin Plot of FWI by  daily burned area Categories", x = " daily burned area Categories", y = "median FWI") +
        theme_minimal()

# Scatter plot of FWI vs area with loess smooth
ggplot(rd2, aes(x = DBA, y = median_fwi)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "loess", color = "blue", se = FALSE) +
        labs(title = "Scatter Plot of FWI vs. daily burned area", x = "daily burned area", y = "median FWI") +
        theme_minimal()

# Density plot of FWI by area categories
ggplot(rd2, aes(x = median_fwi, fill = DBA_Bin)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot of FWI by  daily burned area Categories", x = "median FWI", y = "Density") +
        theme_minimal()


#-----------Time series analysis-------------------------------------

library(xts)

# Prepare data for time series analysis
rd <- rd2[, c("Data", "DNRF", "median_fwi")]
rd[is.na(rd)] <- 0

# Create time series objects using xts (Indexing by Date)
DNRF_ts <- xts(rd$DNRF, order.by = rd$Data)
median_fwi_ts <- xts(rd$median_fwi, order.by = rd$Data)

# Plot the time series
# Reset graphics parameters and try a smaller margin
dev.off()  # This closes any open graphics devices
par(mar = c(3, 3, 2, 1))  # Smaller margins (bottom, left, top, right)
plot(DNRF_ts, main = "Number of Fires Over Time", ylab = "DNRF", xlab = "Date")
plot(median_fwi_ts, main = "Fire Weather Index Over Time", ylab = "median_fwi", xlab = "Date")

# Plot both time series together
plot(DNRF_ts, type = "l", col = "blue", lwd = 2, ylab = "Value", xlab = "Time", main = "Time Series Comparison")
lines(median_fwi_ts, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("DNRF", "median_fwi"), col = c("blue", "red"), lwd = 2)

# Perform Pearson correlation test between DNRF and median_fwi
cor_test <- cor.test(DNRF_ts, median_fwi_ts, use = "complete.obs")

# Extract the correlation coefficient and p-value
correlation <- cor_test$estimate
p_value <- cor_test$p.value

# Print results
print(paste("Correlation between DNRF and median_fwi:", correlation))
print(paste("P-value for the correlation test:", p_value))

# Convert data to time series format
DNRF_ts <- ts(rd$DNRF, frequency = 365)  # Daily data with yearly frequency
median_fwi_ts <- ts(rd$median_fwi, frequency = 365)    # Daily data with yearly frequency

# Cross-correlation between DNRF and median_fwi
ccf(DNRF_ts, median_fwi_ts, main = "Cross-Correlation between DNRF and median_fwi")

# Load the urca package for cointegration test
library(urca)

# Perform cointegration test using the Engle-Granger method
coint_test <- ca.jo(cbind(DNRF_ts, median_fwi_ts), type = "trace", ecdet = "none", K = 2)
summary(coint_test)

# Load package for Granger causality test
library(lmtest)

# Perform Granger Causality test
grangertest(DNRF_ts ~ median_fwi_ts, order = 1)  # Test if median_fwi Granger-causes DNRF
grangertest(median_fwi_ts ~ DNRF_ts, order = 1)  # Test if DNRF Granger-causes median_fwi

# Decompose the time series into components
DNRF_decomposed <- decompose(ts(rd$DNRF, frequency = 365), type = "additive")
plot(DNRF_decomposed)

median_fwi_decomposed <- decompose(ts(rd$median_fwi, frequency = 365), type = "additive")
plot(median_fwi_decomposed)

# Extract trend components
DNRF_trend <- DNRF_decomposed$trend
median_fwi_trend <- median_fwi_decomposed$trend

# Plot the trends on the same plot
plot(DNRF_trend, type = "l", col = "blue", lwd = 2, 
     ylab = "Trend", xlab = "Time", main = "Trend Comparison of DNRF and median_fwi")
lines(median_fwi_trend, col = "red", lwd = 2)
legend("topright", legend = c("DNRF Trend", "median_fwi Trend"), col = c("blue", "red"), lwd = 2)

# Normalize the trends (if on different scales)
DNRF_trend_norm <- (DNRF_trend - min(DNRF_trend, na.rm = TRUE)) / (max(DNRF_trend, na.rm = TRUE) - min(DNRF_trend, na.rm = TRUE))
median_fwi_trend_norm <- (median_fwi_trend - min(median_fwi_trend, na.rm = TRUE)) / (max(median_fwi_trend, na.rm = TRUE) - min(median_fwi_trend, na.rm = TRUE))

# Plot the normalized trends
plot(DNRF_trend_norm, type = "l", col = "blue", lwd = 2, 
     ylab = "Normalized Trend", xlab = "Time", main = "Normalized Trend Comparison of DNRF and median_fwi")
lines(median_fwi_trend_norm, col = "red", lwd = 2)
legend("topright", legend = c("DNRF Trend", "median_fwi Trend"), col = c("blue", "red"), lwd = 2)

# Create a data frame with the normalized trends
trend_data <- data.frame(Date = time(DNRF_trend), 
                         DNRF_trend_norm = DNRF_trend_norm, 
                         median_fwi_trend_norm = median_fwi_trend_norm)

# Scatter plot with trend line for normalized trends
ggplot(trend_data, aes(x = DNRF_trend_norm, y = median_fwi_trend_norm)) +
        geom_point(color = "blue", alpha = 0.6) +
        geom_smooth(method = "lm", color = "red", se = TRUE) +
        labs(title = "DNRF vs Mean median_fwi Distribution with Trend Line",
             x = "DNRF",
             y = "median_fwi") +
        theme_minimal()




#--------------------critical days analysis-----------------
library(dplyr)
library(ggplot2)
library(lubridate)

# Prepare the data - convert Date and extract Year, Month
df <- daily_fires %>%
        mutate(Data = as.Date(Data))

df <- df %>%
        mutate(Ano = year(Data),   # Extract year
               Mes = month(Data)) %>%  # Extract month
        filter(Ano <= 2025 & Ano>=1980)  # Filter years between 1980-2025

# Define thresholds for critical days (95th percentile)
threshold_fires <- quantile(df$DNRF, 0.95, na.rm = TRUE)
threshold_area <- quantile(df$DBA, 0.95, na.rm = TRUE)
threshold_fwiInc <- quantile(df$FWI, 0.95, na.rm = TRUE)
threshold_fwi <- quantile(df$median_fwi, 0.95, na.rm = TRUE)

# Define thresholds for favorable days (5th percentile)
thresholdmin_fires <- quantile(df$DNRF, 0.05, na.rm = TRUE)
thresholdmin_area <- quantile(df$DBA, 0.05, na.rm = TRUE)
thresholdmin_fwiInc <- quantile(df$median_fwi, 0.32, na.rm = TRUE)
thresholdmin_fwi <- quantile(df$FWI, 0.05, na.rm = TRUE)

print(threshold_fires)
print(threshold_area)
print(threshold_fwi)
print(thresholdmin_fires)
print(thresholdmin_area)
print(thresholdmin_fwi)

# Identify critical and favorable days
df <- df %>%
        mutate(CriticalDNRF = ifelse(DNRF >= threshold_fires , 1, 0),
               CriticalDBA = ifelse(DBA >= threshold_area,1,0),
               CriticalFWI = ifelse(FWI >= threshold_fwi,1,0),
               FavorableDNRF = ifelse(DNRF <= thresholdmin_fires , 1, 0),  # Fewer fires
               FavorableDBA = ifelse(DBA <= thresholdmin_area,1,0),  # Less burned area
               FavorableFWI = ifelse(FWI <= thresholdmin_fwi,1,0)  # Lower FWI values
        )

# Count frequency of critical days by year
dias_criticos_por_ano <- df %>%
        group_by(Ano) %>%
        summarise(
                CriticalDNRF = sum(CriticalDNRF == 1, na.rm = TRUE),         # Count critical days (Fires)
                CriticalDBA = sum(CriticalDBA == 1, na.rm = TRUE),  # Count critical days (Burned area)
                CriticalFWI = sum(CriticalFWI == 1, na.rm = TRUE),  # Count critical days (FWI)
                NRF=sum(DNRF, na.rm = TRUE),
                BA=sum(DBA, na.rm = TRUE),
                FavorableDNRF = sum(FavorableDNRF == 1, na.rm = TRUE),         
                FavorableDBA = sum(FavorableDBA == 1, na.rm = TRUE),  
                FavorableFWI = sum(FavorableFWI == 1, na.rm = TRUE)  
        )

# Plot critical days trends
ggplot(dias_criticos_por_ano, aes(x = Ano)) + 
        # Actual points
        geom_point(aes(y = CriticalDNRF, color = "Critical days in DNRF", shape = "Critical days in DNRF"), size = 3) +
        geom_point(aes(y = CriticalDBA, color = "Critical days in DBA", shape = "Critical days in DBA"), size = 2.5) +
        geom_point(aes(y = CriticalFWI, color = "Critical days in FWI", shape = "Critical days in FWI"), size = 2.2) +
        
        # Trend lines
        geom_smooth(aes(y = CriticalDNRF, color = "Critical days in DNRF"), method = "loess", se = TRUE, fill = "salmon", span = 0.5, size = 1, linetype = "solid") +  
        geom_smooth(aes(y = CriticalDBA, color = "Critical days in DBA"), method = "loess", se = TRUE, fill = "lightblue", span = 0.5, size = 1, linetype = "solid") +  
        geom_smooth(aes(y = CriticalFWI, color = "Critical days in FWI"), method = "loess", se = TRUE, fill = "lightgreen", span = 0.5, size = 1, linetype = "solid") +  
        
        # Scales and legend
        scale_color_manual(values = c(
                "Critical days in DNRF" = "red", 
                "Critical days in DBA" = "blue", 
                "Critical days in FWI" = "darkgreen"
        )) +
        scale_shape_manual(values = c(
                "Critical days in DNRF" = 18,  # Diamond
                "Critical days in DBA" = 16,   # Circle
                "Critical days in FWI" = 17    # Triangle
        )) +
        
        # Axes and theme
        labs(
                title = "Smooth line of Frequency of Critical Days for Daily Number of Fires, Burned Area and FWI", 
                x = "Year", 
                y = "Nº of Critical Days per Year", 
                color = "Legend:", 
                shape = "Legend:"
        ) +
        scale_x_continuous(breaks = seq(min(dias_criticos_por_ano$Ano), max(dias_criticos_por_ano$Ano), by = 2)) +
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                legend.key.size = unit(1.2, "cm")
        )

# Plot favorable days trends
ggplot(dias_criticos_por_ano, aes(x = Ano)) + 
        # Actual points
        geom_point(aes(y = FavorableDNRF, color = "Favorable days in DNRF", shape = "Favorable days in DNRF"), size = 3) +
        geom_point(aes(y = FavorableDBA, color = "Favorable days in DBA", shape = "Favorable days in DBA"), size = 2.5) +
        geom_point(aes(y = FavorableFWI, color = "Favorable days in FWI", shape = "Favorable days in FWI"), size = 2.2) +
        
        # Trend lines
        geom_smooth(aes(y = FavorableDNRF, color = "Favorable days in DNRF"), method = "loess", se = TRUE, fill = "salmon", span = 0.5, size = 1, linetype = "solid") +  
        geom_smooth(aes(y = FavorableDBA, color = "Favorable days in DBA"), method = "loess", se = TRUE, fill = "lightblue", span = 0.5, size = 1, linetype = "solid") +  
        geom_smooth(aes(y = FavorableFWI, color = "Favorable days in FWI"), method = "loess", se = TRUE, fill = "lightgreen", span = 0.5, size = 1, linetype = "solid") +  
        
        # Scales and legend
        scale_color_manual(values = c(
                "Favorable days in DNRF" = "red", 
                "Favorable days in DBA" = "blue", 
                "Favorable days in FWI" = "darkgreen"
        )) +
        scale_shape_manual(values = c(
                "Favorable days in DNRF" = 18,  # Diamond
                "Favorable days in DBA" = 16,   # Circle
                "Favorable days in FWI" = 17    # Triangle
        )) +
        
        # Axes and theme
        labs(
                title = "Smooth line of Frequency of Favorable Days for Daily Number of Fires, Burned Area and FWI", 
                x = "Year", 
                y = "Nº of Favorable Days per Year", 
                color = "Legend:", 
                shape = "Legend:"
        ) +
        scale_x_continuous(breaks = seq(min(dias_criticos_por_ano$Ano), max(dias_criticos_por_ano$Ano), by = 2)) +
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                legend.key.size = unit(1.2, "cm")
        )


