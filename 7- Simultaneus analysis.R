# Load required package
if (!require(geosphere)) install.packages("geosphere")
library(geosphere)

# Install and load the pbapply package
if (!require(pbapply)) {
        install.packages("pbapply")
}
library(pbapply)
library(lubridate)

# Function to calculate the number of simultaneous fires within a 5000-meter radius, same Concelho, and same Distrito
count_simultaneous_fires_rect <- function(index, data, radius = 5000) {
        # Get the current fire's location, time window, Concelho, and Distrito
        current_x <- data$x_20790[index]
        current_y <- data$y_20790[index]
        current_start <- data$DHInicio[index]
        current_end <- data$DHFimEstimado[index]
        current_concelho <- data$Concelho[index]
        current_distrito <- data$Distrito[index]
            # Assuming data$DHInicio[index] and data$DHFimEstimado[index] are both date-time objects
        current_end90 <- min(data$DHFimEstimado[index], data$DHInicio[index] + as.difftime(90, units = "mins"))
        
        
        
        
        # Calculate the Euclidean distances from the current fire to all other fires
        distances <- sqrt((data$x_20790 - current_x)^2 + (data$y_20790 - current_y)^2)
        
        # Identify which fires are within the radius and time window
        within_radius <- distances <= radius
        within_time <- (data$DHInicio <= current_end) & (data$DHFimEstimado >= current_start)
        within_time90 <- (data$DHInicio <= current_end90) & (data$DHFimEstimado >= current_start)
       
         # Count the number of simultaneous fires within the radius and time window
        simultaneous_within_radius <- sum(within_radius & within_time) - 1  # Subtract 1 to exclude the current fire itself
        # Count the number of simultaneous fires within the radius and time window in first 90 minutes
        simultaneous_within_radius90 <- sum(within_radius & within_time90) - 1  # Subtract 1 to exclude the current fire itself
 
        # Count the number of simultaneous fires within the same Concelho and time window
        within_concelho <- (data$Concelho == current_concelho)
        simultaneous_within_concelho <- sum(within_concelho & within_time) - 1  # Subtract 1 to exclude the current fire itself
        simultaneous_within_concelho90 <- sum(within_concelho & within_time90) - 1  # Subtract 1 to exclude the current fire itself
        
        # Count the number of simultaneous fires within the same Distrito and time window
        within_distrito <- (data$Distrito == current_distrito)
        simultaneous_within_distrito <- sum(within_distrito & within_time) - 1  # Subtract 1 to exclude the current fire itself
        simultaneous_within_distrito90 <- sum(within_distrito & within_time90) - 1  # Subtract 1 to exclude the current fire itself
        
        return(c(simultaneous_within_radius, simultaneous_within_concelho, simultaneous_within_distrito, simultaneous_within_radius90, simultaneous_within_concelho90, simultaneous_within_distrito90 ))
}

# Initialize an empty data frame to collect results for all years
fires_simultaneous <- data.frame()

# Get the unique years in the dataset
unique_years <- unique(year(fogos$DHInicio))


rd <- subset(fogos,  
             select = c(Codigo, DHInicio, DHFimEstimado, Distrito, Concelho, x_20790, y_20790, Lat_4326, Lon_4326))



# Loop over each year and calculate simultaneous fires
for (year in unique_years) {
  rd <- subset(fogos, Ano == year, 
               select = c(Codigo, DHInicio, DHFimEstimado, Distrito, Concelho, x_20790, y_20790, Lat_4326, Lon_4326))
  print(year)
  
  if (nrow(rd) == 0) {
    next  # Pula para o próximo ano se não houver dados
  }
  
  simultaneous_fires <- pbapply::pblapply(1:nrow(rd), count_simultaneous_fires_rect, data = rd)
  
  simultaneous_fires_df <- do.call(rbind, simultaneous_fires)
  rd$num_simultaneous_5000 <- simultaneous_fires_df[, 1]
  rd$num_simultaneous_concelho <- simultaneous_fires_df[, 2]
  rd$num_simultaneous_distrito <- simultaneous_fires_df[, 3]
  rd$num_simultaneous_500090 <- simultaneous_fires_df[, 4]
  rd$num_simultaneous_concelho90 <- simultaneous_fires_df[, 5]
  rd$num_simultaneous_distrito90 <- simultaneous_fires_df[, 6]
  
  fires_simultaneous <- rbind(fires_simultaneous, rd)
}

# Selecting the last six columns from fires_simultaneous
fire_Simultaneity <- fires_simultaneous[, c("Codigo", 
                                         "num_simultaneous_5000", 
                                         "num_simultaneous_concelho", 
                                         "num_simultaneous_distrito", 
                                         "num_simultaneous_500090", 
                                         "num_simultaneous_concelho90", 
                                         "num_simultaneous_distrito90")]

# Renaming the columns
names(fire_Simultaneity) <- c("Codigo", 
                              "NIncSimul5000_Estimado", 
                              "NIncSimulConcelho_Estimado", 
                              "NIncSimulDistrito_Estimado", 
                              "NIncSimul500090_Estimado", 
                              "NIncSimulConcelho90_Estimado", 
                              "NIncSimulDistrito90_Estimado")

# Merging the columns into fogosClosed based on the Codigo column
fogos <- merge(fogos, fire_Simultaneity, by = "Codigo", all.x = TRUE)

# Now fogosClosed contains the additional columns from fires_simultaneous


# Alternatively, using subset
#fogosClosed <- subset(fogosClosed, select = -c(num_simultaneous_5000, num_simultaneous_concelho, num_simultaneous_distrito, num_simultaneous_500090, num_simultaneous_concelho90, num_simultaneous_distrito90))





rm(columns_to_add, df, rd, simultaneous_fires, simultaneous_fires_df, unique_years, year, count_simultaneous_fires_rect, correlation_with_target, map, points_sf, rd, simultaneous_fires_df, unique_years, year, count_simultaneous_fires, count_simultaneous_fires_rect, get_ndvi)

