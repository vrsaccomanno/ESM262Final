#' 
#' Measure of impact function #1 - 
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
#' @author Claire Powers and Vienna Saccomanno
#' 
#' 


compute_water_NPV = function(df, mean_rain, water_consumption, water_price, drought_co = 1.5) {
 
   if(df$mean_rain <2){
    drought_cost= df$water_price*drought_co
    water_bill = df$water_consumption*drought_cost
  }
  
  else{
    water_bill = df$water_consumption* df$water_price
  }
  
  return(list(water_bill=water_bill))
  }
