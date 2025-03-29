
library(dplyr)






# Group by the new Date column and summarize the data as needed

daily_fires<- fogos %>%
        group_by(Date = as.Date(DHInicio)) %>%
        summarize(
                NInc = n(),  # Count of entries per day
                AreaTotal= sum(AreaTotal, na.rm = TRUE), # somatorio da areatotal por dia
                AreaTotalEstimado = sum(AreaTotalEstimado, na.rm = TRUE),
                Mean_DuracaoEstimado=mean(as.numeric(difftime(DHFimEstimado, DHInicio, units = "hours")), na.rm = TRUE),
                
                mean_AltitudeMedia=mean(AltitudeMedia, na.rm = TRUE),
                mean_CosN5Variedade=mean(CosN5Variedade, na.rm = TRUE),
                mean_dc=mean(dc, na.rm = TRUE),
                mean_fwi=mean(fwi, na.rm = TRUE),
                mean_hFWI=mean(hFWI, na.rm = TRUE),
                mean_HorasExposicaoMedia=mean(HorasExposicaoMedia, na.rm = TRUE),
                mean_HumidadeRelativa=mean(HumidadeRelativa, na.rm = TRUE),
                mean_MaxDC_48h_DiaPosExtincao=mean(MaxDC_48h_DiaPosExtincao, na.rm = TRUE),
                mean_MaxFWIh_48h_PosExtincao=mean(MaxFWIh_48h_PosExtincao, na.rm = TRUE),
                mean_Perigosidade=mean(Perigosidade, na.rm = TRUE),
                mean_Temperatura=mean(Temperatura, na.rm = TRUE),
                mean_VentoDirecao=mean(VentoDirecao, na.rm = TRUE),
                mean_VentoDirecao_vetor=mean(VentoDirecao_vetor, na.rm = TRUE),
                mean_VentoIntensidade=mean(VentoIntensidade, na.rm = TRUE),
                median_AreaManchaModFarsite=median(AreaManchaModFarsite, na.rm = TRUE),
                median_bui=median(bui, na.rm = TRUE),
                median_DecliveMedio=median(DecliveMedio, na.rm = TRUE),
                median_DendidadeRV=median(DendidadeRV, na.rm = TRUE),
                median_DensidadeEdificios=median(DensidadeEdificios, na.rm = TRUE),
                median_DensidadeResidentes=median(DensidadeResidentes, na.rm = TRUE),
                median_Dist_CBS_m=median(Dist_CBS_m, na.rm = TRUE),
                median_dmc=median(dmc, na.rm = TRUE),
                median_dsr=median(dsr, na.rm = TRUE),
                median_DuracaoHoras=median(DuracaoHoras, na.rm = TRUE),
                median_ffmc=median(ffmc, na.rm = TRUE),
                median_HaHora=median(HaHora, na.rm = TRUE),
                median_HaHoraEstimado=median(haHoraEstimado, na.rm = TRUE),
                median_hFFMC=median(hFFMC, na.rm = TRUE),
                median_hISI=median(hISI, na.rm = TRUE),
                median_isi=median(isi, na.rm = TRUE),
                median_MaxBUI_48h_PosExtincao=median(MaxBUI_48h_PosExtincao, na.rm = TRUE),
                median_MaxDMC_48h_PosExtincao=median(MaxDMC_48h_PosExtincao, na.rm = TRUE),
                median_MaxFFMCh_48h_PosExtincao=median(MaxFFMCh_48h_PosExtincao, na.rm = TRUE),
                median_MaxISIh_48h_PosExtincao=median(MaxISIh_48h_PosExtincao, na.rm = TRUE),
                median_NIncSimul5000_Estimado=median(NIncSimul5000_Estimado, na.rm = TRUE),
                median_NIncSimul500090_Estimado=median(NIncSimul500090_Estimado, na.rm = TRUE),
                median_NIncSimulConcelho_Estimado=median(NIncSimulConcelho_Estimado, na.rm = TRUE),
                median_NIncSimulConcelho90_Estimado=median(NIncSimulConcelho90_Estimado, na.rm = TRUE),
                median_NIncSimulDistrito_Estimado=median(NIncSimulDistrito_Estimado, na.rm = TRUE),
                median_NIncSimulDistrito90_Estimado=median(NIncSimulDistrito90_Estimado, na.rm = TRUE),
                median_Precepitacao=median(Precepitacao, na.rm = TRUE),
                median_Rugosidade=median(Rugosidade, na.rm = TRUE),
                median_VentoIntensidade_vetor=median(VentoIntensidade_vetor, na.rm = TRUE)
         )
 

dailydistrict_fires<- fogos %>%
        group_by(Date = as.Date(DHInicio), Distrito) %>%
        summarize(
                NInc = n(),  # Count of entries per day
                AreaTotal= sum(AreaTotal, na.rm = TRUE), # somatorio da areatotal por dia
               # AreaTotalEstimado = sum(AreaTotalEstimado),
                Mean_DuracaoEstimado=mean(as.numeric(difftime(DHFimEstimado, DHInicio, units = "hours")), na.rm = TRUE),
                mean_AltitudeMedia=mean(AltitudeMedia, na.rm = TRUE),
                mean_CosN5Variedade=mean(CosN5Variedade, na.rm = TRUE),
                mean_dc=mean(dc, na.rm = TRUE),
                mean_fwi=mean(fwi, na.rm = TRUE),
                mean_hFWI=mean(hFWI, na.rm = TRUE),
                mean_HorasExposicaoMedia=mean(HorasExposicaoMedia, na.rm = TRUE),
                mean_HumidadeRelativa=mean(HumidadeRelativa, na.rm = TRUE),
                mean_MaxDC_48h_DiaPosExtincao=mean(MaxDC_48h_DiaPosExtincao, na.rm = TRUE),
                mean_MaxFWIh_48h_PosExtincao=mean(MaxFWIh_48h_PosExtincao, na.rm = TRUE),
                mean_Perigosidade=mean(Perigosidade, na.rm = TRUE),
                mean_Temperatura=mean(Temperatura, na.rm = TRUE),
                mean_VentoDirecao=mean(VentoDirecao, na.rm = TRUE),
                mean_VentoDirecao_vetor=mean(VentoDirecao_vetor, na.rm = TRUE),
                mean_VentoIntensidade=mean(VentoIntensidade, na.rm = TRUE),
                median_AreaManchaModFarsite=median(AreaManchaModFarsite, na.rm = TRUE),
                median_bui=median(bui, na.rm = TRUE),
                median_DecliveMedio=median(DecliveMedio, na.rm = TRUE),
                median_DendidadeRV=median(DendidadeRV, na.rm = TRUE),
                median_DensidadeEdificios=median(DensidadeEdificios, na.rm = TRUE),
                median_DensidadeResidentes=median(DensidadeResidentes, na.rm = TRUE),
                median_Dist_CBS_m=median(Dist_CBS_m, na.rm = TRUE),
                median_dmc=median(dmc, na.rm = TRUE),
                median_dsr=median(dsr, na.rm = TRUE),
                median_DuracaoHoras=median(DuracaoHoras, na.rm = TRUE),
                median_ffmc=median(ffmc, na.rm = TRUE),
                median_HaHora=median(HaHora, na.rm = TRUE),
                #median_HaHoraEstimado=median(HaHoraEstimado),
                median_hFFMC=median(hFFMC, na.rm = TRUE),
                median_hISI=median(hISI, na.rm = TRUE),
                median_isi=median(isi, na.rm = TRUE),
                median_MaxBUI_48h_PosExtincao=median(MaxBUI_48h_PosExtincao, na.rm = TRUE),
                median_MaxDMC_48h_PosExtincao=median(MaxDMC_48h_PosExtincao, na.rm = TRUE),
                median_MaxFFMCh_48h_PosExtincao=median(MaxFFMCh_48h_PosExtincao, na.rm = TRUE),
                median_MaxISIh_48h_PosExtincao=median(MaxISIh_48h_PosExtincao, na.rm = TRUE),
                median_NIncSimul5000_Estimado=median(NIncSimul5000_Estimado, na.rm = TRUE),
                median_NIncSimul500090_Estimado=median(NIncSimul500090_Estimado, na.rm = TRUE),
                median_NIncSimulConcelho_Estimado=median(NIncSimulConcelho_Estimado, na.rm = TRUE),
                median_NIncSimulConcelho90_Estimado=median(NIncSimulConcelho90_Estimado, na.rm = TRUE),
                median_NIncSimulDistrito_Estimado=median(NIncSimulDistrito_Estimado, na.rm = TRUE),
                median_NIncSimulDistrito90_Estimado=median(NIncSimulDistrito90_Estimado, na.rm = TRUE),
                median_Precepitacao=median(Precepitacao, na.rm = TRUE),
                median_Rugosidade=median(Rugosidade, na.rm = TRUE),
                median_VentoIntensidade_vetor=median(VentoIntensidade_vetor, na.rm = TRUE)
    
        )






# Perform a left join by the 'date' column
daily_fires <- DailyMeanMeteo1980_2023 %>%
        left_join(daily_fires, by = c("Data" = "Date"))


# Perform a left join by the 'Data' and 'Distrito' columns
dailydistrict_fires <- DailyMeanMeteoDistrito1980_2023 %>%
        left_join(dailydistrict_fires, by = c("Data" = "Date", "Distrito" = "Distrito"))




# Generate descriptive statistics for both datasets
Daily_fires_Descriptives <- descriptives(daily_fires, desc = "rows", vars = colnames(daily_fires), n=TRUE, missing=TRUE, mean=TRUE, median=TRUE, sd=TRUE, variance=TRUE, min=TRUE, max=TRUE, se=TRUE, skew=TRUE, kurt=TRUE, sw=TRUE)
Daily_fires_Descriptives_df <- as.data.frame(Daily_fires_Descriptives)




# Generate descriptive statistics for both datasets
Dailydistrict_fires_Descriptives <- descriptives(dailydistrict_fires, desc = "rows", vars = colnames(dailydistrict_fires), n=TRUE, missing=TRUE, mean=TRUE, median=TRUE, sd=TRUE, variance=TRUE, min=TRUE, max=TRUE, se=TRUE, skew=TRUE, kurt=TRUE, sw=TRUE)
Dailydistrict_fires_Descriptives <- as.data.frame(Dailydistrict_fires_Descriptives)


# Write data to CSV+
write.csv(daily_fires, file = "daily_fires.csv", quote = TRUE, col.names = TRUE,  fileEncoding = "UTF-8", na = "")

# Write data to CSV+
write.csv(dailydistrict_fires, file = "dailydistrict_fires.csv", quote = TRUE, col.names = TRUE,  fileEncoding = "UTF-8", na = "")
# Write data to CSV+
write.csv(DailyMeanMeteoDistrito1980_2023, file = "DailyMeanMeteoDistrito1980_2023.csv", quote = TRUE, col.names = TRUE,  fileEncoding = "UTF-8", na = "")

