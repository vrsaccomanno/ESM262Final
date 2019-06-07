#' 
#' Measure of impact function #1 - 
#' 
#' @param water_use_price An input dataframe/table populated with data from the SB-LTER meterology station that includes information on annual rainfall, average annual water consumption in hectare cubic feet (HCF), and annual average water price in $/HCF from 1942-2016 
#' 
#' @format A data frame with 75 rows and 4 columns
#' \itemize {
#' \item year
#' \item mean_rain average annual rainfall (mm)
#' \item water_consumption average annual household water consumption (HCF)
#' \item water_price average annual price of water ($/HCF)
#' }
#' 
#' @author Claire Powers and Vienna Saccomanno
#' 


compute_water_NPV = function(df, mean_rain, water_consumption, water_price, drought_co = 1.5) {
 
  for(i in 1:nrow(mean_rain)){
    
   if(df$mean_rain[i] <2){
    drought_cost = df$water_price[i]*drought_co [i]
    water_bill = df$water_consumption[i]*drought_cost[i]
  }
  
  else{
    water_bill = df$water_consumption[i]* df$water_price[i]
  }
  }
  
  return(list(water_bill=water_bill))
  }
