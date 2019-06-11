#' Effect of climate change on marine species abundance in Santa Barbara County
#' 
#' 
#' 
#' 


spp_abun_change = function(df,year_col_name, maxt_col_num){
  
  # Filter data.frame for warmest months
  df_fltr = df %>% 
    select(5,3) %>% 
    group_by(year_col_num) %>% 
    summarise(mean_temp = mean(maxt_col_num))
  
  return(df_fltr)
  
}





