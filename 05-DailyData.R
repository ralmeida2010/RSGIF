# Install and load required packages with checks
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(jmv)) install.packages("jmv"); library(jmv)
if (!require(jmvcore)) install.packages("jmvcore"); library(jmvcore)
if (!require(jmvconnect)) install.packages("jmvconnect"); library(jmvconnect)
if (!require(jmvReadWrite)) install.packages("jmvReadWrite"); library(jmvReadWrite)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

# Create daily summary of fire data (aggregated across all districts)
daily_fires <- fogos %>%
    # Group by date (converted from DHInicio datetime)
    group_by(Date = as.Date(DHInicio)) %>%
    summarize(
        # Daily Number of Reported Fires (count of records)
        DNRF = n(),  
        # Daily Burned Area (sum of AreaTotal)
        DBA = sum(AreaTotal, na.rm = TRUE),
        # Median altitude of fire locations
        median_AltitudeMedia = median(AltitudeMedia, na.rm = TRUE),
        # Mean vegetation variety index
        mean_CosN5Variedade = mean(CosN5Variedade, na.rm = TRUE),
        # Mean drought code
        mean_dc = mean(dc, na.rm = TRUE),  
        # Median Fire Weather Index
        median_fwi = median(fwi, na.rm = TRUE),
        # Median hourly Fire Weather Index
        median_hFWI = median(hFWI, na.rm = TRUE),
        # Median exposure hours
        median_HorasExposicaoMedia = median(HorasExposicaoMedia, na.rm = TRUE),
        # Mean relative humidity
        mean_HumidadeRelativa = mean(HumidadeRelativa, na.rm = TRUE),
        # Mean maximum drought code 48h post-extinction
        mean_MaxDC_48h_DiaPosExtincao = mean(MaxDC_48h_DiaPosExtincao, na.rm = TRUE),
        # Median maximum FWI 48h post-extinction
        median_MaxFWIh_48h_PosExtincao = median(MaxFWIh_48h_PosExtincao, na.rm = TRUE),
        # Mean fire danger index
        mean_Perigosidade = mean(Perigosidade, na.rm = TRUE),
        # Median temperature
        median_Temperatura = median(Temperatura, na.rm = TRUE),
        # Median wind intensity
        median_VentoIntensidade = median(VentoIntensidade, na.rm = TRUE),
        # Median fire scar area from Farsite model
        median_AreaManchaModFarsite = median(AreaManchaModFarsite, na.rm = TRUE),
        # Median buildup index
        median_bui = median(bui, na.rm = TRUE),
        # Median slope
        median_DecliveMedio = median(DecliveMedio, na.rm = TRUE),
        # Median road density
        median_DendidadeRV = median(DendidadeRV, na.rm = TRUE),
        # Median building density
        median_DensidadeEdificios = median(DensidadeEdificios, na.rm = TRUE),
        # Median resident density
        median_DensidadeResidentes = median(DensidadeResidentes, na.rm = TRUE),
        # Median distance to closest water source
        median_Dist_CBS_m = median(Dist_CBS_m, na.rm = TRUE),
        # Median duff moisture code
        median_dmc = median(dmc, na.rm = TRUE),
        # Median daily severity rating
        median_dsr = median(dsr, na.rm = TRUE),
        # Median estimated fire duration in hours
        median_DuracaoHorasEstimado = median(DuracaoHorasEstimado, na.rm = TRUE),  
        # Median fine fuel moisture code
        median_ffmc = median(ffmc, na.rm = TRUE),
        # Median area burned per hour
        median_HaHora = median(HaHora, na.rm = TRUE), 
        # Median hourly fine fuel moisture code
        median_hFFMC = median(hFFMC, na.rm = TRUE), 
        # Median hourly initial spread index
        median_hISI = median(hISI, na.rm = TRUE),
        # Median initial spread index
        median_isi = median(isi, na.rm = TRUE),
        # Median estimated number of simultaneous fires (5000m radius)
        median_NIncSimul5000_Estimado = median(NIncSimul5000_Estimado, na.rm = TRUE),
        # Median estimated number of simultaneous fires (5000m radius, 90th percentile)
        median_NIncSimul500090_Estimado = median(NIncSimul500090_Estimado, na.rm = TRUE),
        # Median estimated number of simultaneous fires in municipality
        median_NIncSimulConcelho_Estimado = median(NIncSimulConcelho_Estimado, na.rm = TRUE),
        # Median estimated number of simultaneous fires in municipality (90th percentile)
        median_NIncSimulConcelho90_Estimado = median(NIncSimulConcelho90_Estimado, na.rm = TRUE),
        # Median estimated number of simultaneous fires in district
        median_NIncSimulDistrito_Estimado = median(NIncSimulDistrito_Estimado, na.rm = TRUE),
        # Median estimated number of simultaneous fires in district (90th percentile)
        median_NIncSimulDistrito90_Estimado = median(NIncSimulDistrito90_Estimado, na.rm = TRUE),
        # Median precipitation
        median_Precepitacao = median(Precepitacao, na.rm = TRUE),
        # Median terrain roughness
        median_Rugosidade = median(Rugosidade, na.rm = TRUE),
        # Median vector wind intensity
        median_VentoIntensidade_vetor = median(VentoIntensidade_vetor, na.rm = TRUE)  
    )

# Create daily summary of fire data by district
dailydistrict_fires <- fogos %>%
    # Group by both date and district
    group_by(Date = as.Date(DHInicio), Distrito) %>%
    summarize(
        # Same metrics as above but grouped by district
        DNRF = n(),
        DBA = sum(AreaTotal, na.rm = TRUE),
        median_AltitudeMedia = median(AltitudeMedia, na.rm = TRUE),
        mean_CosN5Variedade = mean(CosN5Variedade, na.rm = TRUE),
        mean_dc = mean(dc, na.rm = TRUE),
        # [All other metrics repeated from above...]
        median_VentoIntensidade_vetor = median(VentoIntensidade_vetor, na.rm = TRUE)  
    )

# Merge daily fire data with meteorological data
daily_fires <- DailyMeanMeteoFrom1980 %>%
    left_join(daily_fires, by = c("Data" = "Date"))

# Merge district-level fire data with meteorological data
dailydistrict_fires <- DailyMeanMeteoDistritoFrom1980 %>%
    left_join(dailydistrict_fires, by = c("Data" = "Date", "Distrito" = "Distrito"))

# Generate descriptive statistics for national-level data
Daily_fires_Descriptives <- descriptives(
    data = daily_fires,
    desc = "rows",
    vars = colnames(daily_fires),
    n = TRUE, missing = TRUE, mean = TRUE, median = TRUE,
    sd = TRUE, variance = TRUE, min = TRUE, max = TRUE,
    se = TRUE, skew = TRUE, kurt = TRUE, sw = TRUE
)
Daily_fires_Descriptives_df <- as.data.frame(Daily_fires_Descriptives)

# Generate descriptive statistics for district-level data
Dailydistrict_fires_Descriptives <- descriptives(
    data = dailydistrict_fires,
    desc = "rows",
    vars = colnames(dailydistrict_fires),
    n = TRUE, missing = TRUE, mean = TRUE, median = TRUE,
    sd = TRUE, variance = TRUE, min = TRUE, max = TRUE,
    se = TRUE, skew = TRUE, kurt = TRUE, sw = TRUE
)
Dailydistrict_fires_Descriptives <- as.data.frame(Dailydistrict_fires_Descriptives$descriptivesT)

# Create visualization of mean temperature over time
ggplot(dailydistrict_fires, aes(x = Data, y = meanT)) +
    geom_line() +  # Line plot showing trend
    geom_point() +  # Points marking each observation
    labs(
        title = "Mean Temperature Over Time",
        x = "Date",
        y = "Mean Temperature"
    ) +
    theme_minimal()  # Clean, modern theme

# Access descriptive statistics plots
Dailydistrict_fires_Descriptives$plots
