# Load required package
if (!require(mice)) install.packages("mice")
if (!require(jmv)) install.packages("jmv")
if (!require(jmvcore)) install.packages("jmvcore")
if (!require(jmvconnect)) install.packages("jmvconnect")
if (!require(jmvReadWrite)) install.packages("jmvReadWrite")
if (!require(sf)) install.packages("sf")
if (!require(sf)) install.packages("sf")
if (!require(purrr)) install.packages("purrr")
if (!require(pbapply)) install.packages("pbapply")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(sf)) install.packages("sf")
library(mice)
library(jmv)
library(jmvcore)
library(jmvconnect)
library(jmvReadWrite)
library(sf)
library(utils)
library(dplyr)
library(purrr)
library(pbapply)
library(ggplot2)
library(sf)   # For handling shapefiles and spatial data

#-------------------1- correction of INE----

# Load the shapefile
url <- "https://fogos.icnf.pt/download/ExportarDadosSGIF/CAOP2012.zip"
download.file(url, destfile = "CAOP2012.zip", mode = "wb")
unzip("CAOP2012.zip", exdir = "CAOP2012")

shapefile_path <- list.files("CAOP2012", pattern = "\\.shp$", full.names = TRUE)
shp_data <- st_read(shapefile_path)

# Filter rows where INE is NA
fires_na<- fogos %>%
        filter(is.na(INE))

# Select only the desired columns
fires_na <- fires_na %>%
        dplyr::select(Codigo, INE, x_3763, y_3763)

# Convert to an sf object
fires_na_geo <- st_as_sf(fires_na, coords = c("x_3763", "y_3763"), crs = 3763)

# Encontrar o polígono mais próximo para cada ponto
nearest_polygon_indices <- st_nearest_feature(fires_na_geo, shp_data)

# Obter os dados do polígono mais próximo
fires_na$INE <- shp_data$Dicofre[nearest_polygon_indices]

# Loop para percorrer cada linha de fogos_na_ine_sf_df
for (i in 1:nrow(fires_na)) {
        
        # Extrair o Codigo e o INE atual do fogos_na_ine_sf_df
        codigo_atual <- fires_na$Codigo[i]
        ine_atualizado <- fires_na$INE[i]
        
        # Verificar quais linhas em 'fogos' têm o mesmo 'Codigo'
        idx <- which(fogos$Codigo == codigo_atual)
        
        # Se encontrarmos correspondências, atualizamos o INE
        if (length(idx) > 0) {
                # Atualizar o INE apenas onde ele é nulo
               fogos$Observacoes[idx] <- ifelse(is.na(fogos$INE[idx]),
                                                    paste(fogos$Observacoes, ";", "ine atualizado"),
                                                     fogos$updated_INE_log[idx])  # Atualiza apenas quando INE for nulo
                
                # Atualizar a coluna INE em fogos, caso seja nulo
                fogos$INE[idx] <- ifelse(is.na(fogos$INE[idx]), ine_atualizado, fogos$INE[idx])
       }
}

rm(fires_na, fires_na_geo, shp_data, codigo_atual, i, idx, ine_atualizado, nearest_polygon_indices, shapefile_path, url)


#----------------atualização de info administrativa------------------
fogos <- fogos %>%
  # Ensure INE column in 'fogos' is numeric
  mutate(INE = as.numeric(INE)) %>%
  
  # Perform the left join with 'AdministrativeRelations' where INE is also numeric
  left_join(AdministrativeRelations %>% mutate(INE = as.numeric(INE)), by = "INE") %>%
  
  # Mutate and update the relevant columns
  mutate(
    Distrito = coalesce(Distrito.y, Distrito.x),
    Concelho = coalesce(Concelho.y, Concelho.x),
    Freguesia = coalesce(Freguesia.y, Freguesia.x),
    Freguesia2014 = coalesce(Freguesia_pos2012, Freguesia2014),
    DDCCFF2014 = coalesce(DTCCFG_Pos2012, DDCCFF2014),
    NUTS2 = coalesce(NUTSII_DSG, NUTS2),
    NUTS3 = coalesce(NUTSIII_DSG, NUTS3)
  ) %>%
  
  # Select only the columns from the original 'fogos' table
  select(names(fogos))

#----------------seleção de subset para análise de falhas 


#--------------------------------------------------------------------


# Subset and mutate the data
rdbase <- subset(fogos, ClassificacaoRegisto=="Closed" , 
                 select = c(Codigo, INE, DHInicio, DHFim, AreaTotal, fwi))

rdbase <- rdbase %>%
  mutate(AreaTotal = ifelse(AreaTotal == 0, NA, AreaTotal))

rdbase <- rdbase %>%
  mutate(DHFim = ifelse(DHFim < DHInicio, NA, DHFim),
         duration = as.numeric(difftime(DHFim, DHInicio, units = "hours")))

#descriptives(rdbase, desc = "rows", vars = colnames(rdbase), n=TRUE, missing=TRUE, mean=TRUE, median=TRUE, sd=TRUE, variance=TRUE, min=TRUE, max=TRUE, se=TRUE, skew=TRUE, kurt=TRUE, sw=TRUE)



#----------------correção de falhas de DHFim
rdbase <- subset(rdbase, select = -DHFim)

md.pattern(rdbase)
#methods(mice)

#impute_pmm <- mice(rdbase, method = "pmm", m = 5)
#impute_norm <- mice(rdbase, method = "2l.norm", m = 5)
#impute_cart <- mice(rdbase, method = "cart", m = 5)
impute_rf <- mice(rdbase, method = "rf", m = 5)
impute_mean <- mice(rdbase, method = "mean", m = 5)
impute_sample <- mice(rdbase, method = "sample", m = 5)

plot(impute_sample)
summary(impute_rf)
imputed_data <- complete(impute_rf, action = 1)
imputed_data_long <- complete(impute_rf, action = "long")
#------------------------------------
fogos <- fogos %>%
  mutate(DHFim = ifelse(DHFim < DHInicio, NA, DHFim),
         duration = as.numeric(difftime(DHFim, DHInicio, units = "hours")))
impute_rf_completed <- complete(impute_rf)

fogos <- fogos %>%
  mutate(
    DHFim = ifelse(DHFim < DHInicio, NA, DHFim),
    duration = as.numeric(difftime(DHFim, DHInicio, units = "hours"))
  )

impute_rf_completed <- complete(impute_rf)

fogos <- fogos %>%
  left_join(impute_rf_completed, by = "Codigo") %>%
  mutate(
    # Update AreaTotal, fwi, and duration using coalesce
    AreaTotal = coalesce(AreaTotal.x, AreaTotal.y),
    fwi = coalesce(fwi.x, fwi.y),
    duration = coalesce(duration.x, duration.y),
    
    # Append to Observacoes if AreaTotal is updated
    Observacoes = ifelse(is.na(AreaTotal.x) & !is.na(AreaTotal.y),
                         paste(Observacoes, 'Atualização do valor null de AreaTotal'),
                         Observacoes),
    
    # Append to Observacoes if fwi is updated
    Observacoes = ifelse(is.na(fwi.x) & !is.na(fwi.y),
                         paste(Observacoes, 'Atualização do valor null de fwi'),
                         Observacoes),
    
    # Append to Observacoes if duration is updated
    Observacoes = ifelse(is.na(duration.x) & !is.na(duration.y),
                         paste(Observacoes, 'Atualização do valor null de duration'),
                         Observacoes)
  ) %>%
  rename(
    DHInicio = DHInicio.x,
    INE = INE.x
  ) 
  #%>%
  # Select only the columns from the original 'fogos' dataframe
  # select(all_of(fogoscolumns))


# Step 1: Ensure DHInicio is in POSIXct or POSIXlt format
fogos$DHInicio <- as.POSIXct(fogos$DHInicio)

# Step 2: Check if duracao is numeric
fogos$duration <- as.numeric(fogos$duration)

# Step 3: Handle NAs in duracao (optional)
# Replace NAs in duracao with 0, if necessary
fogos$duration[is.na(fogos$duration)] <- 0

# Ensure duration is numeric and round/floor it to avoid non-integer values
#fogos$duration <- floor(as.numeric(fogos2$duration))

# Step 4: Fill DHFim where it's NA

fogos$DHFim[is.na(fogos$DHFim)] <- fogos$DHInicio[is.na(fogos$DHFim)] + lubridate::duration(fogos$duration[is.na(fogos$DHFim)], "hours")
fogos$DHFim <- as.POSIXct(fogos2$DHFim, origin = "1970-01-01", tz = "UTC")

fogos$DuracaoHoras <-as.numeric(difftime(fogos$DHFim, fogos$DHInicio, units = "hours"))

fogos$HaHora <- ifelse(fogos$DuracaoHoras == 0, 0, fogos$AreaTotal / fogos$DuracaoHoras)

# Select only the columns specified in 'fogoscolumns'
fogos <- fogos %>% select(all_of(fogoscolumns))
