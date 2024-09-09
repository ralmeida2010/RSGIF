# Load necessary libraries
library(dplyr)
library(caret)
library(ggplot2)
library(lubridate)


# Subset and mutate the data
rdbase <- subset(fogos, ClassificacaoRegisto=="Closed" & AreaTotal > 0 & DHFim > DHInicio & !is.na(DHFim), 
             select = c(Codigo, INE, DHInicio, DHFim, AreaTotal))

rdbase <- rdbase %>%
  mutate(
    Diff = as.numeric(difftime(DHFim, DHInicio, units = "hours")),  # Calculate the difference in hours
    HaHora = AreaTotal / Diff, # Calculate the new column
    NDHInicio = as.numeric(DHInicio),
    NDHFim = as.numeric(DHFim)
  )

Q1Diff=(quantile(rdbase$Diff, 0.25, na.rm = TRUE) - 1.5 * (quantile(rdbase$Diff, 0.75, na.rm = TRUE) - quantile(rdbase$Diff, 0.25, na.rm = TRUE))) 
Q3Diff=(quantile(rdbase$Diff, 0.75, na.rm = TRUE) + 1.5 * (quantile(rdbase$Diff, 0.75, na.rm = TRUE) - quantile(rdbase$Diff, 0.25, na.rm = TRUE)))  

Q1HaHora= (quantile(rdbase$HaHora, 0.25) -1.5 * (quantile(rdbase$HaHora, 0.75)- quantile(rdbase$HaHora, 0.25)))
Q3HaHora= (quantile(rdbase$HaHora, 0.75) +1.5 * (quantile(rdbase$HaHora, 0.75)- quantile(rdbase$HaHora, 0.25)))

rdbaseArea <- subset(rdbase, DHFim > DHInicio 
              & !is.na(DHFim) 
              & Diff > (quantile(rdbase$Diff, 0.25, na.rm = TRUE) - 1.5 * (quantile(rdbase$Diff, 0.75, na.rm = TRUE) - quantile(rdbase$Diff, 0.25, na.rm = TRUE))) 
              & Diff < (quantile(rdbase$Diff, 0.75, na.rm = TRUE) + 1.5 * (quantile(rdbase$Diff, 0.75, na.rm = TRUE) - quantile(rdbase$Diff, 0.25, na.rm = TRUE)))  
              & AreaTotal > (quantile(rdbase$AreaTotal, 0.25, na.rm = TRUE) - 1.5 * (quantile(rdbase$AreaTotal, 0.75, na.rm = TRUE) - quantile(rdbase$AreaTotal, 0.25, na.rm = TRUE))) 
              & AreaTotal < (quantile(rdbase$AreaTotal, 0.75, na.rm = TRUE) + 1.5 * (quantile(rdbase$AreaTotal, 0.75, na.rm = TRUE) - quantile(rdbase$AreaTotal, 0.25, na.rm = TRUE)))  
              & HaHora > (quantile(rdbase$HaHora, 0.25, na.rm = TRUE) - 1.5 * (quantile(rdbase$HaHora, 0.75, na.rm = TRUE) - quantile(rdbase$HaHora, 0.25, na.rm = TRUE))) 
              & HaHora < (quantile(rdbase$HaHora, 0.75, na.rm = TRUE) + 1.5 * (quantile(rdbase$HaHora, 0.75, na.rm = TRUE) - quantile(rdbase$HaHora, 0.25, na.rm = TRUE)))  
              
)

# Fit a linear regression model for AreaTotal
linear_model <- lm(AreaTotal ~ 0 + Diff, data = rdbaseArea)
# Adicionar  0.001 ao coeficiente do modelo
Coef <- linear_model$coefficients[1] + 0.001



adjust_DHFim <- function(IN_DHInicio, IN_DHFim, IN_AreaTotal, Q1HaHora, Q3HaHora, Q1Diff, Q3Diff, IN_INE, rdbase, Coef) {
  i<-0
  
  
  # If the end time is missing, set it to the start time
  if (is.na(IN_DHFim)) {IN_DHFim <- IN_DHInicio 
  i<-1}
  
  # If the end time is earlier than or equal to the start time and the year of the end time is earlier than the start time,
  # update the end time to the same year as the start time
  if (IN_DHFim <= IN_DHInicio & year(IN_DHFim) < year(IN_DHInicio)) {
    IN_DHFim <- update(IN_DHFim, year = year(IN_DHInicio)) 
    i<-2
  }
  
  # If the end time is earlier than or equal to the start time and the year is the same but the month of the end time is earlier,
  # update the end time to the same month as the start time
  if (IN_DHFim <= IN_DHInicio & year(IN_DHFim) == year(IN_DHInicio) & month(IN_DHFim) < month(IN_DHInicio)) {
    IN_DHFim <- update(IN_DHFim, month = month(IN_DHInicio))
    i<-3
  }
  
  # If the end time is still earlier than the start time after the above adjustments, 
  # increment the end time by one day until it is after the start time
  while (IN_DHFim < IN_DHInicio) {
    IN_DHFim <- IN_DHFim + days(1)
    i<-4
  }
  
  IN_Diff <- as.numeric(difftime(IN_DHFim, IN_DHInicio, units = "hours"))
  
  # If AreaTotal is missing or non-positive and the time difference (IN_Diff) is greater than zero,
  # attempt to adjust the end time using a predictive model based on the historical data
  if ((is.na(IN_AreaTotal) || IN_AreaTotal <= 0) & IN_Diff > 0) {
    IN_AreaTotal <- Coef * IN_Diff  # Use the correct model and data frame
    i<-5
  }
    
  
  IN_Diff <- as.numeric(difftime(IN_DHFim, IN_DHInicio, units = "hours"))
  
  # If the time difference (IN_Diff) is zero , meaning no time has passed between the start and end times,
  # and area burned is greate then zero
  # attempt to adjust the end time using a predictive model based on the historical data
  if (IN_Diff == 0 & IN_AreaTotal>0) {
    
    # Filter the dataset based on the INE value to find similar records
    subset_rd <- subset(rdbase, INE == IN_INE)
    
    # If there are more than 10 records in the filtered dataset, fit a linear model to predict the time difference based on AreaTotal
    if (nrow(subset_rd) > 10) {
      model <- lm(Diff ~ AreaTotal, data = subset_rd)
      predicted_diff_hours <- predict(model, data.frame(AreaTotal = IN_AreaTotal))
      IN_DHFimArea <- IN_DHInicio + as.difftime(predicted_diff_hours, units = "hours")
      
      # If the predicted time difference is greater than zero, update the end time
      if (as.numeric(difftime(IN_DHFimArea, IN_DHInicio, units = "hours")) > 0) {
        IN_DHFim <- IN_DHFimArea
        i<-6
      } 
    }
  }
  
  IN_Diff <- as.numeric(difftime(IN_DHFim, IN_DHInicio, units = "hours"))
  
  # If the time difference (IN_Diff) is zero , meaning no time has passed between the start and end times,
  # and area burned is  zero
  # attempt to adjust the end time using a predictive model based on the historical data
  if (IN_Diff == 0 & IN_AreaTotal==0) {
    IN_AreaTotal<- 0.0001
    
    # Filter the dataset based on the INE value to find similar records
    subset_rd <- subset(rdbase, INE == IN_INE)
    
    # If there are more than 10 records in the filtered dataset, fit a linear model to predict the time difference based on AreaTotal
    if (nrow(subset_rd) > 10) {
      model <- lm(Diff ~ AreaTotal, data = subset_rd)
      predicted_diff_hours <- predict(model, data.frame(AreaTotal = IN_AreaTotal))
      IN_DHFimArea <- IN_DHInicio + as.difftime(predicted_diff_hours, units = "hours")
      
      # If the predicted time difference is greater than zero, update the end time
      if (as.numeric(difftime(IN_DHFimArea, IN_DHInicio, units = "hours")) > 0) {
        IN_DHFim <- IN_DHFimArea
        i<-7
      } 
    }
  }
  
  
  
  
  
  
  
  IN_Diff <- as.numeric(difftime(IN_DHFim, IN_DHInicio, units = "hours"))
  
  # Calculate the rate of AreaTotal per hour (HaHora)
  HaHora <- IN_AreaTotal / IN_Diff
  
  # If HaHora is outside the interquartile range (Q1HaHora, Q3HaHora) and the time difference (IN_Diff) is also outside its interquartile range (Q1Diff, Q3Diff),
  # attempt to adjust the end time using a predictive model based on the historical data
  if ((HaHora < Q1HaHora || HaHora > Q3HaHora) & (IN_Diff < Q1Diff || IN_Diff > Q3Diff) ) {
    
    # Filter the dataset based on the INE value to find similar records
    subset_rd <- subset(rdbase, INE == IN_INE)
    
    # If there are more than 10 records in the filtered dataset, fit a linear model to predict the time difference based on AreaTotal
    if (nrow(subset_rd) > 10) {
      
      #IN_AreaTotal <- Coef * IN_Diff  # Use the correct model and data frame
      
      model <- lm(Diff ~ AreaTotal, data = subset_rd)
      predicted_diff_hours <- predict(model, data.frame(AreaTotal = IN_AreaTotal))
      IN_DHFimArea <- IN_DHInicio + as.difftime(predicted_diff_hours, units = "hours")
      
      # If the predicted time difference is greater than zero, update the end time
      if (as.numeric(difftime(IN_DHFimArea, IN_DHInicio, units = "hours")) > 0) {
        IN_DHFim <- IN_DHFimArea
        i<-8
      } 
    }
  }
  
  IN_Diff <- as.numeric(difftime(IN_DHFim, IN_DHInicio, units = "hours"))
  
  # If the time difference (IN_Diff) is greater than 800 hours,
  # attempt to adjust the end time using a predictive model based on a broader subset of the historical data
  if (IN_Diff > 800) {
    
    # Filter the dataset based on the first two digits of the INE value to find similar records within a broader category
    subset_rd <- subset(rdbase, floor(INE / 100) == floor(IN_INE / 100))
    
    # If there are more than 10 records in the filtered dataset, fit a linear model to predict the time difference based on AreaTotal
    if (nrow(subset_rd) > 10) {
      model <- lm(Diff ~ AreaTotal, data = subset_rd)
      predicted_diff_hours <- predict(model, data.frame(AreaTotal = IN_AreaTotal))
      IN_DHFimArea <- IN_DHInicio + as.difftime(predicted_diff_hours, units = "hours")
      
      # If the predicted time difference is greater than zero, update the end time
      if (as.numeric(difftime(IN_DHFimArea, IN_DHInicio, units = "hours")) > 0) {
        IN_DHFim <- IN_DHFimArea
        i<-9
      } 
    }
  }
  

  return(list(DHFim=  IN_DHFim, AreaTotal = IN_AreaTotal, Tipo=i))

}


rd <- subset(fogos, ClassificacaoRegisto=="Closed"   , 
             select = c(Codigo, INE, DHInicio, DHFim, AreaTotal))

rd <- rd %>%
  mutate(
    Diff = as.numeric(difftime(DHFim, DHInicio, units = "hours")),  # Calculate the difference in hours
    HaHora = AreaTotal / Diff, # Calculate the new column
    NDHInicio = as.numeric(DHInicio),
    NDHFim = as.numeric(DHFim)
  )



rd <- rd %>%
  mutate(
    # Apply the function once and extract the outputs separately
    AdjustedOutput = mapply(adjust_DHFim, DHInicio, DHFim, AreaTotal, Q1HaHora, Q3HaHora, Q1Diff, Q3Diff, INE, MoreArgs = list(rdbase = rdbase, Coef = Coef), SIMPLIFY = FALSE),
    
    # Extract the individual components from the list returned by the function
    NDHFimEstimado = sapply(AdjustedOutput, `[[`, "DHFim"),
    DHFimEstimado = as.POSIXct(sapply(AdjustedOutput, `[[`, "DHFim"), tz = "UTC"),
    AreaTotalEstimado = sapply(AdjustedOutput, `[[`, "AreaTotal"),
    Tipo = sapply(AdjustedOutput, `[[`, "Tipo")
  )


rd <- rd %>%
  mutate(Diff2 = (NDHFimEstimado- NDHInicio)/3600)


rd <- rd %>%
  mutate(DiffAA = (AreaTotal- AreaTotalEstimado))

rm(rdbase, rdbaseArea, linear_model, Coef, project_dir, Q1Diff, Q1HaHora, Q3Diff, Q3HaHora, adjust_DHFim, adjust_DHFim2, ajustar_DHFim, replace_problems)

rd_junta <- subset(rd, !is.na(Codigo), 
             select = c(Codigo, DHFimEstimado, AreaTotalEstimado, Tipo))


fogosClosed <- fogos %>%
  inner_join(rd_junta, by = "Codigo") %>%
  mutate(DHFimEstimado = DHFimEstimado.y) %>%  # Replace DHFimEstimado with values from rd_junta
  #select(-DHFimEstimado.x, -DHFimEstimado.y) %>%   # Remove the old DHFimEstimado columns
  rename(TipoIncendio = Tipo.x) %>%  # Rename Tipo.y to TipoEstimado
  mutate(HaHoraEstimado = AreaTotalEstimado / as.numeric(difftime(DHFimEstimado, DHInicio, units = "hours")))  # Calculate HaHoraEstimado


rm(Data1980_2000, Data1980_2000eliminados, Data2001_noweliminados, Data1980_2000eliminados)
rm(rd, rd_junta, result)
#rm(Data1980_2000,Data1980_2000eliminados, Data2001_now, Data2001_noweliminados)
rm(bdown)
rm(clean_text)
rm(points_sf)
rm(get_ndvi)
rm(map)


#----------------------validações de modelos-----------------------------------------------------------------------
