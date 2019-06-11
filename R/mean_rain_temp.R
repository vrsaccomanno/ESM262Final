#' 
#' Climate function #1 - mean rainfall, maximum temperature, and minimum temperature by year
#' 
#' @param clim_data An input dataframe/table populated with data from the SB-LTER meterology station that includes information on daily maximum temperature, minimum temperature, and rainfall from 1942-2016
#' 
#' @format A data frame with 27274 rows and 8 columns
#' \itemize {
#' \item date day/month/year
#' \item tmin minimum daily temperature (C)
#' \item tmax maximum daily temperature (C)
#' \item rain daily rainfall (mm)
#' \item year
#' \item month
#' \item day
#' \item wy water year
#' }
#' 
#' @return Mean rainfall (mm), maximum temperature (C), and minimum temperature (C) by year
#' @author Claire Powers and Vienna Saccomanno
#' 
#' 

mean_rain_temp = function(clim_table){
  
  
  #Summarize average rainfall, maximum temperature and minimum temperature per year in tidy format
  mean_by_year<- clim_table %>% 
    select(year, tmin, tmax, rain) %>% 
    group_by(year) %>% 
    summarize(mean_rain = mean(rain), mean_tmin = mean(tmin), mean_tmax = mean(tmax))
  
  return(list(mean_by_year=mean_by_year))
}
