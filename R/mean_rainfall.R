#' 
#' Climate function #1
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

mean_rainfall = function(clim_table){
  
  #create an output table for average rainfall per year
  #mean_by_year = as.data.frame(matrix(ncol = 1,
                                         #nrow = 75))
  
  #assign row names
  #rownames(mean_by_year)<- unique(clim_data$year)
  
  #assign column name
  #colnames(mean_by_year)<-c("Mean Rainfall")
  
  #Fill in average rainfall per year in tidy format
  mean_by_year<- clim_data %>% 
    group_by(year) %>% 
    mutate(mean_rainfall = mean(rain, na.rm = TRUE))
  
  return(list(mean_by_year=mean_by_year))
}
