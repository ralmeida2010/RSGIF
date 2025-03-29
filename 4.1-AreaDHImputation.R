library(dplyr)
library(mice)
library(lubridate)
library(VIM)
library(ggplot2)
library(coda)

# Filtrando apenas as colunas necessárias
dados <- fogos[, c("Codigo", "INE", "Distrito", "Mes", "Hora", "DHInicio", "DHFim", "DHFimEstimado", 
                   "AreaTotal", "AreaAgric", "AreaMato", "AreaPov", "fwi", "Observacoes")]


dados <- dados %>%
        mutate(
                AreaAgricEstimado = ifelse(is.na(AreaAgric), 0, AreaAgric),
                AreaTotalEstimado = ifelse(AreaTotal<= 0, NA, AreaTotal),
                AreaPovEstimado = ifelse(AreaTotal == 0, NA, AreaPov),
                AreaMatoEstimado = ifelse(AreaTotal == 0, NA, AreaMato),
                AreaAgricEstimado = ifelse(AreaTotal == 0, NA, AreaAgric)
        )
dados <- dados %>%
        mutate(DHFimEstimado = ifelse(DHFim < DHInicio, NA, DHFim),
               duration = as.numeric(difftime(DHFim, DHInicio, units = "mins")))
dados <- dados %>%
        mutate(duration = ifelse(duration <= 0 | 
                                         (grepl("anomaly", Observacoes, ignore.case = TRUE) & AreaTotal < 10) |
                                         grepl("estimated", Observacoes, ignore.case = TRUE), 
                                 NA, duration))

# Convertendo variáveis categóricas para fatores
dados <- dados %>%
        mutate(Distrito = as.factor(Distrito),
               Mes = as.factor(Mes),
               Hora= as.factor(Hora))

dados_imp <- dados[, c("Distrito", "Mes", "Hora",  "fwi", "AreaTotalEstimado", "duration")]

#aggr(dados_imp)
#md.pattern(dados_imp)
dados_imp <- dados_imp %>%
mutate(Distrito = as.factor(Distrito),
       Mes = as.factor(Mes),
       Hora= as.factor(Hora))
       
       

# Aplicando o MICE para imputação
imputacao <- mice(dados_imp, method = "pmm", m = 10, maxit = 100, seed = 1234)



# Extraindo os dados imputados
dados_imputados <- complete(imputacao)
#aggr(dados_imputados)
#plot(imputacao)
#stripplot(imputacao)
#methods(mice)

#par(mfrow=c(2,2))
#boxplot(dados_imp$AreaTotalEstimado, main="AreaTotalEstimado with NA")
#boxplot(dados_imputados$AreaTotalEstimado, main="AreaTotalEstimado without NA")

t.test(dados_imp$AreaTotalEstimado, dados_imputados$AreaTotalEstimado)
t.test(dados_imp$duration, dados_imputados$duration)

var(dados_imp$AreaTotalEstimado, na.rm = TRUE)
var(dados_imputados$AreaTotalEstimado)


library(coda)
library(mice)
library(coda)
# Extract all imputed datasets
imp_data <- complete(imputacao, "all")  # Get all imputed datasets

# Select only numeric columns
imp_data_numeric <- lapply(imp_data, function(df) df[, sapply(df, is.numeric)])

# Convert each imputed dataset to MCMC format
imp_data_mcmc <- lapply(imp_data_numeric, as.mcmc)

# Combine into a single mcmc.list object
imp_mcmc_list <- as.mcmc.list(imp_data_mcmc)

# Check convergence using Gelman-Rubin diagnostic
library(coda)
gelman.diag(imp_mcmc_list)


#plot(density(dados_imp$duration, na.rm=TRUE), main="Data with NA")
#line(density(dados_imputados$duration, na.rm=TRUE), col="red", lty=3)



# Ensure duration keeps imputed values and replace only remaining NAs with 0
#dados$duration[is.na(dados$duration)] <- 0

# Ensure DHInicio is in POSIXct format
dados$DHInicio <- as.POSIXct(dados$DHInicio, tz = "UTC")

dados$DHFimEstimado <- dados$DHInicio + 
        lubridate::duration(dados$duration * 60)  # Convert minutes to seconds

# Ensure DHFimEstimado is POSIXct
dados$DHFimEstimado <- as.POSIXct(dados$DHFimEstimado, tz = "UTC")

    

# Aplicando o MICE para imputação
imputacao <- mice(dados_imp, method = "pmm", m = 10, maxit = 100, seed = 1234)



# Extraindo os dados imputados
dados_imputados <- complete(imputacao)
#aggr(dados_imputados)
#plot(imputacao)
#stripplot(imputacao)
#methods(mice)

#par(mfrow=c(2,2))
#boxplot(dados_imp$AreaTotalEstimado, main="AreaTotalEstimado with NA")
#boxplot(dados_imputados$AreaTotalEstimado, main="AreaTotalEstimado without NA")

t.test(dados_imp$AreaTotalEstimado, dados_imputados$AreaTotalEstimado)
t.test(dados_imp$duration, dados_imputados$duration)

var(dados_imp$AreaTotalEstimado, na.rm = TRUE)
var(dados_imputados$AreaTotalEstimado)


# Extract all imputed datasets
imp_data <- complete(imputacao, "all")  # Get all imputed datasets

# Select only numeric columns
imp_data_numeric <- lapply(imp_data, function(df) df[, sapply(df, is.numeric)])

# Convert each imputed dataset to MCMC format
imp_data_mcmc <- lapply(imp_data_numeric, as.mcmc)

# Combine into a single mcmc.list object
imp_mcmc_list <- as.mcmc.list(imp_data_mcmc)

# Check convergence using Gelman-Rubin diagnostic
library(coda)
gelman.diag(imp_mcmc_list)


#plot(density(dados_imp$duration, na.rm=TRUE), main="Data with NA")
#line(density(dados_imputados$duration, na.rm=TRUE), col="red", lty=3)



# Ensure duration keeps imputed values and replace only remaining NAs with 0
#dados$duration[is.na(dados$duration)] <- 0

# Ensure DHInicio is in POSIXct format
dados$DHInicio <- as.POSIXct(dados$DHInicio, tz = "UTC")

dados$DHFimEstimado <- dados$DHInicio + 
        lubridate::duration(dados$duration * 60)  # Convert minutes to seconds

# Ensure DHFimEstimado is POSIXct
dados$DHFimEstimado <- as.POSIXct(dados$DHFimEstimado, tz = "UTC")


# Criar cópia da estrutura original de fogos
fogos_actualizado <- fogos

# Atualizar as colunas imputadas no dataset original
fogos_actualizado$AreaTotalEstimado <- dados_imp$AreaTotalEstimado
fogos_actualizado$duration <- dados_imp$duration
fogos_actualizado$DHFimEstimado <- fogos_actualizado$DHInicio + 
        lubridate::duration(fogos_actualizado$duration * 60)  # Converter minutos para segundos

fogos_actualizado$DHFimEstimado <- as.POSIXct(fogos_actualizado$DHFimEstimado, tz = "UTC")

# Garantir que Observacoes não é NA (para evitar erros de concatenação)
fogos_actualizado$Observacoes <- ifelse(is.na(fogos_actualizado$Observacoes), "", fogos_actualizado$Observacoes)

# Atualizar Observacoes para AreaTotalEstimado diferente de AreaTotal, sem duplicar "Area Estimated"
fogos_actualizado$Observacoes <- ifelse(
        fogos_actualizado$AreaTotalEstimado != fogos_actualizado$AreaTotal & 
                !grepl("Area Estimated", fogos_actualizado$Observacoes),
        paste0(fogos_actualizado$Observacoes, ";Area Estimated"),
        fogos_actualizado$Observacoes
)

# Atualizar Observacoes para DHFimEstimado diferente de DHFim, sem duplicar "DHFim Estimated"
fogos_actualizado$Observacoes <- ifelse(
        fogos_actualizado$DHFimEstimado != fogos_actualizado$DHFim & 
                !grepl("DHFim Estimated", fogos_actualizado$Observacoes),
        paste0(fogos_actualizado$Observacoes, ";DHFim Estimated"),
        fogos_actualizado$Observacoes
)

# Remover espaços extras no início da string
fogos_actualizado$Observacoes <- trimws(fogos_actualizado$Observacoes, which = "left")



rm(dados, dados_imp, dados_imputados, dados_selecionados,fogos_atualizado, fogos_filtrado, fogosi, fogos)

fogos <- fogos_actualizado
rm (fogos_actualizado)

fogos <- fogos %>%
        mutate(
                DuracaoHorasEstimado = as.numeric(difftime(DHFimEstimado, DHInicio, units = "hours")),
                haHoraEstimado = ifelse(DuracaoHorasEstimado > 0, AreaTotalEstimado / DuracaoHorasEstimado, NA)
        )


