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


compute_water_NPV = function(df, mean_rain, water_consumption, water_price, time, drought_co = 1.5, discount = 0.01) {
  
  #Making a blank vector to fill with water bill info
  water_bill_nominal<-rep(0, nrow(df)) #assumes good rain in every year
  water_bill_true<-rep(0, nrow(df)) #What actually happens
  difference <- rep(0, nrow(df))
  year = df$year
  difference_NPV = 0
  
  for(i in 1:nrow(df)){
    
   if(df$mean_rain[i] <2){
    drought_corrfact = drought_co
  }
  
  else{
    drought_corrfact = 1
  }
    water_bill_true [i] = df$water_consumption[i]*df$water_price[i]*drought_corrfact
    water_bill_nominal[i] = df$water_consumption[i]*df$water_price[i]
    difference [i] = water_bill_true[i]-water_bill_nominal[i]
    difference_NPV = difference_NPV + difference[i]/(1+discount)**water_use_price$time[i]
    #sum_difference_NPV = sum(df$difference_NPV[i])
  }
  
future_value = ((1+discount)**75 * difference_NPV)
  return(list(water_use = data.frame(year=year, water_bill_nominal=water_bill_nominal, water_bill_true=water_bill_true, difference = difference), difference_NPV = difference_NPV, future_value))
  }
