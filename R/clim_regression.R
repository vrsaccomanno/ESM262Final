#' Climate regression
#' 

climate_var_regress = function(df,dep_var,ind_var,plot=FALSE){
  
  # Error checking
  if(is.numeric(dep_var)==FALSE||is.factor(dep_var)==FALSE) 
    return("Dependent variable is non-numeric or non-factor. Convert to numeric or factor and rerun function")
  
  if(is.numeric(ind_var)==FALSE||is.factor(ind_var)==FALSE) 
    return("Independent variable is non-numeric or non-factor. Convert to numeric or factor and rerun function")
  
  # Linear regression on variables of interest
  lm = lm(paste0(dep_var, "~", ind_var),data=df)
  coefs = lm$coef
  
  
  # Optional plot of results
  if(plot){
  
    slope=coefs[2]
    
     lm_plot = ggplot(df,aes_string(x = ind_var,y=dep_var))+
       geom_smooth(method = "lm")+
       theme_classic()+
       labs(title=paste0("Slope = ",format(coefs[2],digits = 4)))
     return(list(coefs = coefs,lm_plot=lm_plot))
     }
  
  return(coefs)
}
