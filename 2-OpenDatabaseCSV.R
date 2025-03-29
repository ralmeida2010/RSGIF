# Load required package

if (!require(jmv)) install.packages("jmv")
if (!require(jmvcore)) install.packages("jmvcore")
if (!require(jmvconnect)) install.packages("jmvconnect")
if (!require(jmvReadWrite)) install.packages("jmvReadWrite")
if (!require(sf)) install.packages("sf")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(ggExtra)) install.packages("ggExtra")
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(reshape2)) install.packages("reshape2")
if (!require(lubridate)) install.packages("lubridate")     
if (!require(purrr)) install.packages("purrr")
if (!require(pbapply)) install.packages("pbapply")
if (!require(utils)) install.packages("utils")

# Load necessary libraries
library(jmv)
library(jmvcore)
library(jmvconnect)
library(jmvReadWrite)
library(reshape2)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readr)            
library(sf)
library(utils)
library(dplyr)
library(purrr)

# Set the project directory
project_dir <- "C:/user/rui/Doutoramento/SGIF-Database struture/Data/"  # Replace with your desired project directory

bdown=function(url, file){
        library('RCurl')
        f = CFILE(file, mode="wb")
        a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
        close(f)
        return(a)
}

clean_text <- function(x) {
        x <- as.character(x)         # Ensure the input is character
        x <- gsub("[\r\n|;]", "", x) # Remove unwanted characters
        x[is.na(x)] <- ""            # Replace NA with empty string
        return(x)
}
# Define a function to replace commas with semicolons
replace_problems <- function(x) {
        # Replace commas with semicolons
        x <- gsub(",", ";", x)
        # Replace carriage returns (\r) with empty strings
        x <- gsub("\r", "", x)
        # Replace line feeds (\n) with empty strings
        x <- gsub("\n", "", x)
        return(x)
}




## ...and now just give remote and local paths     
ret = bdown("https://fogos.icnf.pt/download/ExportarDadosSGIF/Data2001_now.csv", "Data2001_now.csv")

## ...and now just give remote and local paths     
ret = bdown("https://fogos.icnf.pt/download/ExportarDadosSGIF/Data1980_2000.csv", "Data1980_2000.csv")

## ...and now just give remote and local paths     
ret = bdown("https://fogos.icnf.pt/download/ExportarDadosSGIF/Data1980_2000eliminados.csv", "Data1980_2000eliminados.csv")

## ...and now just give remote and local paths     
ret = bdown("https://fogos.icnf.pt/download/ExportarDadosSGIF/Data2001_noweliminados.csv", "Data2001_noweliminados.csv")

## ...and now just give remote and local paths     
ret = bdown("https://fogos.icnf.pt/download/ExportarDadosSGIF/DailyMeanMeteo1980_2023.csv", "DailyMeanMeteo1980_2023.csv")

## ...and now just give remote and local paths     
ret = bdown("https://fogos.icnf.pt/download/ExportarDadosSGIF/DailyMeanMeteoDistrito1980_2023.csv", "DailyMeanMeteoDistrito1980_2023.csv")

## ...and now just give remote and local paths     
ret = bdown("https://fogos.icnf.pt/download/ExportarDadosSGIF/Fire_TotalMeteo.csv", "Fires_TotalMeteo")

## ...and now just give remote and local paths     
ret = bdown("https://fogos.icnf.pt/download/ExportarDadosSGIF/AdministrativeRelations.csv", "AdministrativeRelations")


rm(ret)

# Read the CSV file with semicolon as the column separator and ISO-8859-1 encoding
Data1980_2000 <- read_delim(
        "Data1980_2000.csv",
        delim = ",",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE  # Suppress column types message
)

# Read the CSV file with semicolon as the column separator and ISO-8859-1 encoding
Data1980_2000eliminados <- read_delim(
        "Data1980_2000eliminados.csv",
        delim = ",",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE  # Suppress column types message
)

Data2001_now <- read_delim(
        "Data2001_now.csv",
        delim = ",",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE  # Suppress column types message
)

Data2001_noweliminados <- read_delim(
        "Data2001_noweliminados.csv",
        delim = ",",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE  # Suppress column types message
)

DailyMeanMeteoDistrito1980_2023 <- read_delim(
        "DailyMeanMeteoDistrito1980_2023.csv",
        delim = ",",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE  # Suppress column types message
)

DailyMeanMeteo1980_2023 <- read_delim(
        "DailyMeanMeteo1980_2023.csv",
        delim = ",",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE  # Suppress column types message
)


Fires_TotalMeteo <- read_delim(
        "Fires_TotalMeteo",
        delim = ",",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE  # Suppress column types message
)


AdministrativeRelations <- read_delim(
        "AdministrativeRelations",
        delim = ",",
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE  # Suppress column types message
)

# Filtrar as linhas onde x_3763 não é NA e manter a coluna codigo
Data1980_2000_valid <- Data1980_2000[!is.na(Data1980_2000$x_3763), c("Codigo", "x_3763", "y_3763")]

# Criar o sf object a partir do data frame filtrado
sf_points <- st_as_sf(Data1980_2000_valid, coords = c("x_3763", "y_3763"), crs = 3763)

# Transformar as coordenadas para EPSG:4326
sf_points_transformed <- st_transform(sf_points, crs = 4326)

# Extrair as coordenadas transformadas
coords_transformed <- st_coordinates(sf_points_transformed)

# Adicionar as coordenadas transformadas como novas colunas
Data1980_2000_valid$Lat_4326 <- coords_transformed[, 2]
Data1980_2000_valid$Lon_4326 <- coords_transformed[, 1]

# Atualizar as colunas Lat_4326 e Lon_4326 no data frame original
#Data1980_2000$Lat_4326[!is.na(Data1980_2000$x_3763)] <- Data1980_2000_valid$Lat_4326
#Data1980_2000$Lon_4326[!is.na(Data1980_2000$x_3763)] <- Data1980_2000_valid$Lon_4326
rm(sf_points)
rm(sf_points_transformed)
rm(coords_transformed)

# Mesclar as colunas transformadas de volta ao data frame original
Data1980_2000 <- merge(Data1980_2000, Data1980_2000_valid[, c("Codigo", "Lat_4326", "Lon_4326")], by = "Codigo", all.x = TRUE)
rm(Data1980_2000_valid)

fogos<- bind_rows(Data1980_2000, Data2001_now)
fogoseliminados<- bind_rows(Data1980_2000eliminados, Data2001_noweliminados)
fogos$Distrito[fogos$Distrito == "Viana Do Castelo"] <- "Viana do Castelo"

# Extract the column names of the fogos dataset
existing_columns <- colnames(fogos)

# List of columns you want to remove
columns_to_remove <- c("Nut2", "Nut3", "...1", "...2", "Fogacho", "Agricola")

# Keep only columns from 'columns_to_remove' that actually exist in the dataset
columns_to_remove_existing <- intersect(columns_to_remove, existing_columns)

# Remove only the existing columns
fogos <- fogos %>%
        dplyr::select(-all_of(columns_to_remove_existing))



# Rename column 'Tipo' to 'TipoFogo'
fogos <- fogos %>%
        rename(TipoFogo = Tipo)
fogoscolumns<-names(fogos)

Fires_TotalMeteo <- Fires_TotalMeteo %>%
        mutate(
                codigo = ifelse(year(DHMeteo) < 2001, 
                                paste0(year(DHMeteo), "_", codigo), 
                                codigo)
        )




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


#----------------atualização de info administrativa passar para o 2------------------
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

#write.csv(fogos, "fogos.csv", row.names = TRUE,  fileEncoding = "UTF-8", na = "")

#--------------------------------------------------------------------


# Generate descriptive statistics for both datasets
tt <- descriptives(fogos, desc = "rows", vars = colnames(fogos), n=TRUE, missing=TRUE, mean=TRUE, median=TRUE, sd=TRUE, variance=TRUE, min=TRUE, max=TRUE, se=TRUE, skew=TRUE, kurt=TRUE, sw=TRUE)
fogosDescriptives <- as.data.frame(tt$descriptivesT)
rm(tt)

rm ( columns_to_remove, columns_to_remove_existing, existing_columns, fogoscolumns)
# Selecionar colunas específicas
#Fires_Base<-  subset(fogos, select = c("Codigo", "TipoFogo", "FonteAlerta", "Ano", "Mes", "Dia", "Hora", "DHInicio", "DH1Intervencao", "DHResolucao", "DHConclusao", "DHFim", "NUTS3", "NUTS2", "Distrito", "Concelho", "Freguesia", "Local", "INE", "DDCCFF2014", "x_20790", "y_20790", "x_3763", "y_3763", "Lat_4326", "Lon_4326", "QO", "AreaAgric", "AreaMato", "AreaPov", "AreaTotal", "ClasseArea", "HaHora", "CodCausa", "TipoCausa" , "GrupoCausa", "DescricaoCausa","Reacendimento", "Reacendimento_IncendioPai", "OriginouReacendimento", "Fogacho" ) )
#Fires_LandUse<- subset(fogos, select = c("Codigo", "Perimetro","APS",  "ModFarsite", "AreaManchaModFarsite", "AltitudeMedia", "DecliveMedio", "HorasExposicaoMedia", "Rugosidade", "DendidadeRV", "CosN5Variedade", "Perigosidade", "Dist_CBS_m", "CBS", "DensidadeResidentes", "DensidadeEdificios" ))
#Fires_Meteo<- subset(fogos, select = c("Codigo", "Temperatura", "HumidadeRelativa", "VentoIntensidade", "VentoIntensidade_vetor", "VentoDirecao_vetor", "VentoDirecao", "Precepitacao", "fwi", "dsr", "isi", "dc", "dmc", "ffmc", "bui", "hFWI", "hFFMC", "hISI", "RCM", "MaxFWIh_48h_PosExtincao","MaxFFMCh_48h_PosExtincao", "MaxISIh_48h_PosExtincao", "MaxDC_48h_DiaPosExtincao", "MaxDMC_48h_PosExtincao", "MaxBUI_48h_PosExtincao" ))
#Fires_Simultaneity <- subset(fogos, select = c("Codigo", "NIncSimul5000","NIncSimulDistrito", "NIncSimulConcelho", "NIncSimul500090", "NIncSimulDistrito90", "NIncSimulConcelho90"))

#rm(project_dir)
#rm(Data1980_2000, Data1980_2000eliminados, Data2001_now, Data2001_noweliminados)
#rm(bdown, clean_text, replace_problems)
#rm(fire_Base, fire_LandUse, fire_Meteo, fire_Simultaneity, fogosDescriptives, fogoseliminados)


