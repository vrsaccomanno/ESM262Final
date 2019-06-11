#' Effect of climate change on marine species abundance in Santa Barbara County
#' 
#' 
#' 
#' 
#' 
#' 


spp_abun_change = function(df, year_col_name, maxt_col_name, species_array){
  
  # Error checking
  # Get rid of incomplete years
  
  # Filter data.frame to keep year and tmax columns
  df = df %>% 
    select(year_col_name,maxt_col_name)
  
  # Specify those column names here
  year = colnames(df)[1]
  tmax = colnames(df)[2]
  
  # Find the mean maximum temperature for each year
  df_fltr = df %>% 
    group_by(year) %>% 
    summarise(mean_tmax = mean(tmax))
  
  # Develop coefficients to have species abundance change with mean daily maximum temperature for each year. 
  
  # For sea cabbage, increases in mean maximum temperature increases species abundance
  cabbage_coefs = data.frame(innertidal_cabb = df_fltr$mean_tmax/df_fltr$mean_tmax[1],
                    midinnertidal_cabb = (df_fltr$mean_tmax*0.99)/df_fltr$mean_tmax[1],
                    openpcean_cabb = (df_fltr$mean_tmax*0.98)/df_fltr$mean_tmax[1])
  
  # For leapord sharks, increases in mean maximum temperature decreases species abundance
  shrk_anem_coefs = data.frame(innertidal_cabb = df_fltr$mean_tmax[1]/df_fltr$mean_tmax,
                              midinnertidal_cabb = df_fltr$mean_tmax[1]/(df_fltr$mean_tmax*0.99),
                              openpcean_cabb = df_fltr$mean_tmax[1]/(df_fltr$mean_tmax*0.98))

  output_df = data.frame(year = df_fltr$year,
                        sharksA = rep(0,nrow(df_fltr)),
                        sharksB = rep(0,nrow(df_fltr)),
                        sharksC = rep(0,nrow(df_fltr)),
                        anemA = rep(0,nrow(df_fltr)),
                        anemB = rep(0,nrow(df_fltr)),
                        anemC = rep(0,nrow(df_fltr)),
                        cabbageA = rep(0,nrow(df_fltr)),
                        cabbageB = rep(0,nrow(df_fltr)),
                        cabbageC = rep(0,nrow(df_fltr)))
  
  # Find total species abundance across all 5 beaches, but keep them seperated by tidal zones and open ocean. 

  species_df = apply(marine_spp_abun,c(3,2),sum)
  
  for(i in 1:nrow(df_fltr)){ # Year layer
    for(j in 1:nrow(species_df)){ # Zone layer
       for(k in 2:ncol(species_df)) # 
       
       # Leopard shark
       output_df[i,k] = species_df[1,j]*shrk_anem_coefs[i,j] # ith row of the innertidal column
       
       # Anemones 
       output_df[i,k+3] = species_df[2,j]*shrk_anem_coefs[i,j] # ith row of the midinnertidal column
       
       # Sea cabbage
       output_df[i,k+6] = species_df[3,j]*cabbage_coefs[i,j] # ith row of the midinnertidal column
        
       
    }
  }
  
  
  return(output_df)
  
}






