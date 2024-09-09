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


# Generate descriptive statistics for both datasets
tt <- descriptives(fogos, desc = "rows", vars = colnames(fogos), n=TRUE, missing=TRUE, mean=TRUE, median=TRUE, sd=TRUE, variance=TRUE, min=TRUE, max=TRUE, se=TRUE, skew=TRUE, kurt=TRUE, sw=TRUE)
fogosDescriptives <- as.data.frame(tt$descriptivesT)
rm(tt)
# Selecionar colunas específicas
fire_Base<-  subset(fogos, select = c("Codigo", "CodigoSado", "Tipo", "FonteAlerta", "Ano", "Mes", "Dia", "Hora", "DHInicio", "DH1Intervencao", "DHResolucao", "DHConclusao", "DHFim", "DHFimEstimado", "NUTS3", "NUTS2", "Distrito", "Concelho", "Freguesia", "Local", "INE", "DDCCFF2014", "x_20790", "y_20790", "x_3763", "y_3763", "Lat_4326", "Lon_4326", "QO", "AreaAgric", "AreaMato", "AreaPov", "AreaTotal", "ClasseArea", "HaHora", "CodCausa", "TipoCausa" , "GrupoCausa", "DescricaoCausa","Reacendimento", "Reacendimento_IncendioPai", "OriginouReacendimento", "Fogacho" ) )
fire_LandUse<- subset(fogos, select = c("Codigo", "Perimetro","APS",  "ModFarsite", "AreaManchaModFarsite", "AltitudeMedia", "DecliveMedio", "HorasExposicaoMedia", "Rugosidade", "DendidadeRV", "CosN5Variedade", "Perigosidade", "Dist_CBS_m", "CBS", "DensidadeResidentes", "DensidadeEdificios" ))
fire_Meteo<- subset(fogos, select = c("Codigo", "Temperatura", "HumidadeRelativa", "VentoIntensidade", "VentoIntensidade_vetor", "VentoDirecao_vetor", "VentoDirecao", "Precepitacao", "fwi", "dsr", "isi", "dc", "dmc", "ffmc", "bui", "hFWI", "hFFMC", "hISI", "RCM", "MaxFWIh_48h_PosExtincao","MaxFFMCh_48h_PosExtincao", "MaxISIh_48h_PosExtincao", "MaxDC_48h_DiaPosExtincao", "MaxDMC_48h_PosExtincao", "MaxBUI_48h_PosExtincao" ))
fire_Simultaneity <- subset(fogos, select = c("Codigo", "NIncSimul5000", 
                                              "NIncSimulDistrito", "NIncSimulConcelho", 
                                              "NIncSimul500090", "NIncSimulDistrito90", 
                                              "NIncSimulConcelho90"))

#rm(project_dir)
#rm(Data1980_2000, Data1980_2000eliminados, Data2001_now, Data2001_noweliminados)
#rm(bdown, clean_text, replace_problems)
