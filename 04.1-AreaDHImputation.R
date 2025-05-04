# Install and load required packages
if (!require(dplyr)) install.packages("dplyr")
if (!require(mice)) install.packages("mice")
if (!require(lubridate)) install.packages("lubridate")
if (!require(VIM)) install.packages("VIM")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(coda)) install.packages("coda")
if (!require(httr)) install.packages("httr")


# Load required libraries
library(dplyr)       # For data manipulation
library(mice)        # For multiple imputation
library(lubridate)   # For date-time operations
library(VIM)         # For visualizing missing data
library(ggplot2)     # For data visualization
library(coda)        # For MCMC diagnostics

# Select only necessary columns from the original dataset
dados <- fogos[, c("Codigo", "INE", "Distrito", "Mes", "Hora", "DHInicio", "DHFim", "DHFimEstimado", 
                   "AreaTotal", "AreaAgric", "AreaMato", "AreaPov", "fwi", "Observacoes")]

# Data cleaning and feature engineering
dados <- dados %>%
    mutate(
        # Estimate agricultural area (replace NA with 0)
        AreaAgricEstimado = ifelse(is.na(AreaAgric), 0, AreaAgric),
        # Set AreaTotalEstimado to NA if AreaTotal is <= 0
        AreaTotalEstimado = ifelse(AreaTotal <= 0, NA, AreaTotal),
        # Set populated area to NA if total area is 0
        AreaPovEstimado = ifelse(AreaTotal == 0, NA, AreaPov),
        # Set bush area to NA if total area is 0
        AreaMatoEstimado = ifelse(AreaTotal == 0, NA, AreaMato),
        # Set agricultural area to NA if total area is 0
        AreaAgricEstimado = ifelse(AreaTotal == 0, NA, AreaAgric)
    )

# Calculate fire duration and handle anomalies
dados <- dados %>%
    mutate(
        # Set DHFimEstimado to NA if end time is before start time
        DHFimEstimado = ifelse(DHFim < DHInicio, NA, DHFim),
        # Calculate duration in minutes
        duration = as.numeric(difftime(DHFim, DHInicio, units = "mins"))
    )

# Clean duration values based on observations
dados <- dados %>%
    mutate(
        duration = ifelse(
            duration <= 0 | 
                (grepl("anomaly", Observacoes, ignore.case = TRUE) & AreaTotal < 10) |
                grepl("estimated", Observacoes, ignore.case = TRUE), 
            NA, 
            duration
        )
    )

# Convert categorical variables to factors
dados <- dados %>%
    mutate(
        Distrito = as.factor(Distrito),
        Mes = as.factor(Mes),
        Hora = as.factor(Hora)
    )

# Select columns for imputation
dados_imp <- dados[, c("Distrito", "Mes", "Hora", "fwi", "AreaTotalEstimado", "duration")]

# Ensure factor types for imputation
dados_imp <- dados_imp %>%
    mutate(
        Distrito = as.factor(Distrito),
        Mes = as.factor(Mes),
        Hora = as.factor(Hora)
    )

# Perform multiple imputation using MICE (Predictive Mean Matching)
imputacao <- mice(
    dados_imp, 
    method = "pmm",  # Predictive Mean Matching
    m = 10,          # Number of imputed datasets
    maxit = 100,     # Maximum iterations
    seed = 1234      # For reproducibility
)

# Get the completed imputed dataset
dados_imputados <- complete(imputacao)

# Compare distributions before and after imputation
t.test(dados_imp$AreaTotalEstimado, dados_imputados$AreaTotalEstimado)
t.test(dados_imp$duration, dados_imputados$duration)

# Compare variances
var(dados_imp$AreaTotalEstimado, na.rm = TRUE)
var(dados_imputados$AreaTotalEstimado)

# MCMC diagnostics for imputation
# Get all imputed datasets
imp_data <- complete(imputacao, "all")

# Select only numeric columns for diagnostics
imp_data_numeric <- lapply(imp_data, function(df) df[, sapply(df, is.numeric)])

# Convert to MCMC format
imp_data_mcmc <- lapply(imp_data_numeric, as.mcmc)

# Create MCMC list object
imp_mcmc_list <- as.mcmc.list(imp_data_mcmc)

# Check convergence using Gelman-Rubin diagnostic
gelman.diag(imp_mcmc_list)

# Date-time handling
# Convert start time to POSIXct format
dados$DHInicio <- as.POSIXct(dados$DHInicio, tz = "UTC")

# Estimate end time based on duration (converting minutes to seconds)
dados$DHFimEstimado <- dados$DHInicio + lubridate::duration(dados$duration * 60)

# Ensure estimated end time is in POSIXct format
dados$DHFimEstimado <- as.POSIXct(dados$DHFimEstimado, tz = "UTC")

# Update original dataset with imputed values
fogos_actualizado <- fogos

# Transfer imputed values to original dataset
fogos_actualizado$AreaTotalEstimado <- dados_imp$AreaTotalEstimado
fogos_actualizado$duration <- dados_imp$duration

# Calculate estimated end time based on imputed duration
fogos_actualizado$DHFimEstimado <- fogos_actualizado$DHInicio + 
    lubridate::duration(fogos_actualizado$duration * 60)  # Convert minutes to seconds

# Ensure estimated end time is in POSIXct format
fogos_actualizado$DHFimEstimado <- as.POSIXct(fogos_actualizado$DHFimEstimado, tz = "UTC")

# Handle observation notes
# Ensure Observations column isn't NA
fogos_actualizado$Observacoes <- ifelse(
    is.na(fogos_actualizado$Observacoes), 
    "", 
    fogos_actualizado$Observacoes
)

# Update notes for estimated areas
fogos_actualizado$Observacoes <- ifelse(
    fogos_actualizado$AreaTotalEstimado != fogos_actualizado$AreaTotal & 
        !grepl("Area Estimated", fogos_actualizado$Observacoes),
    paste0(fogos_actualizado$Observacoes, ";Area Estimated"),
    fogos_actualizado$Observacoes
)

# Update notes for estimated end times
fogos_actualizado$Observacoes <- ifelse(
    fogos_actualizado$DHFimEstimado != fogos_actualizado$DHFim & 
        !grepl("DHFim Estimated", fogos_actualizado$Observacoes),
    paste0(fogos_actualizado$Observacoes, ";DHFim Estimated"),
    fogos_actualizado$Observacoes
)

# Clean up observation notes
fogos_actualizado$Observacoes <- trimws(fogos_actualizado$Observacoes, which = "left")

# Clean up environment
rm(dados, dados_imp, dados_imputados, dados_selecionados, fogos_atualizado, fogos_filtrado, fogosi, fogos)

# Replace original dataset with updated version
fogos <- fogos_actualizado
rm(fogos_actualizado)

# Calculate additional duration metrics
fogos <- fogos %>%
    mutate(
        # Calculate duration in hours
        DuracaoHorasEstimado = as.numeric(difftime(DHFimEstimado, DHInicio, units = "hours")),
        # Calculate burned area per hour
        haHoraEstimado = ifelse(DuracaoHorasEstimado > 0, AreaTotalEstimado / DuracaoHorasEstimado, NA)
    )

# Clean intervention date
fogos$DH1Intervencao <- as.Date(fogos$DH1Intervencao)
fogos$DH1Intervencao[fogos$DH1Intervencao == as.Date("1900-01-01")] <- NA

# Select and reorder columns
fogos <- fogos[, c("Codigo","CodigoSado", "Ano","Dia", "Mes", "Hora", "TipoFogo", "DHInicio","DH1Intervencao", 
                   "DHResolucao", "DHConclusao", "DHFim", "DHFimEstimado", "DuracaoHoras", "duration", 
                   "DuracaoHorasEstimado","Tempo1Intervencao","TempoResolucao", "TempoRescaldo",
                   "NUTS2", "NUTS3", "Distrito", "Concelho", "Freguesia", "Freguesia2014", "Local", "INE", 
                   "DDCCFF2014", "QO", "x_20790", "y_20790", "x_3763", "y_3763", "Lat_4326", "Lon_4326",  
                   "AreaPov", "AreaMato", "AreaAgric", "AreaTotal","AreaTotalEstimado", "HaHora","haHoraEstimado" ,"ClasseArea", 
                   "CodCausa", "TipoCausa", "GrupoCausa", "DescricaoCausa", "Reacendimento", "Reacendimento_IncendioPai", 
                   "OriginouReacendimento", "Reacendimentos", "FonteAlerta", "AltitudeMedia", "DecliveMedio", 
                   "HorasExposicaoMedia", "Rugosidade", "Perimetro", "APS" ,"ModFarsite", "AreaManchaModFarsite",
                   "RCM", "DendidadeRV", "CosN5Variedade", "Perigosidade", "Dist_CBS_m", "CBS", "DensidadeResidentes", 
                   "DensidadeEdificios", "AreaTotalIncSimul5000", "NIncSimulDistrito", "NIncSimulConcelho", 
                   "NIncSimulDistrito90", "NIncSimulConcelho90", "NIncSimul500090", "DistIncSimul500090", 
                   "AreaTotalIncSimul500090", "EstadoRegisto", "Temperatura", "HumidadeRelativa", "VentoIntensidade", 
                   "VentoIntensidade_vetor", "VentoDirecao_vetor", "Precepitacao", "VentoDirecao", "fwi", "dsr", 
                   "isi", "dc", "dmc", "ffmc", "bui", "hFWI", "hFFMC", "hISI", "MaxFWIh_48h_PosExtincao", 
                   "MaxFFMCh_48h_PosExtincao", "MaxISIh_48h_PosExtincao", "MaxDC_48h_DiaPosExtincao", 
                   "MaxDMC_48h_PosExtincao", "MaxBUI_48h_PosExtincao", "NIncSimul5000", "DistIncSimul5000", 
                   "ClassificacaoRegisto","Observacoes")]

# Rename duration column
fogos <- fogos %>%
    select(-DuracaoHoras) %>%
    rename(DuracaoMinutos = duration)

# Data validation checks
# Check 1: Estimated end time should be after start time
fogos$check_1 <- with(fogos, is.na(DHFimEstimado) | is.na(DHInicio) | DHFimEstimado >= DHInicio)

# Check 2: First intervention time should be between start and estimated end time
fogos$check_2 <- with(fogos, is.na(DH1Intervencao) | is.na(DHInicio) | is.na(DHFimEstimado) |
                          (DH1Intervencao >= DHInicio & DH1Intervencao <= DHFimEstimado))

# Check 3: Resolution time should be between start and estimated end time
fogos$check_3 <- with(fogos, is.na(DHResolucao) | is.na(DHInicio) | is.na(DHFimEstimado) |
                          (DHResolucao >= DHInicio & DHResolucao <= DHFimEstimado))

# Check 4: Conclusion time should be between start and estimated end time
fogos$check_4 <- with(fogos, is.na(DHConclusao) | is.na(DHInicio) | is.na(DHFimEstimado) |
                          (DHConclusao >= DHInicio & DHConclusao <= DHFimEstimado))

# Check 5: Time sequence should be: intervention <= resolution <= conclusion
fogos$check_5 <- with(fogos, is.na(DH1Intervencao) | is.na(DHResolucao) | is.na(DHConclusao) |
                          (DH1Intervencao <= DHResolucao & DHResolucao <= DHConclusao))

# Remove validation check columns (keeping just the clean data)
fogos <- fogos %>%
    select(-check_1, -check_2, -check_3, -check_4, -check_5)
