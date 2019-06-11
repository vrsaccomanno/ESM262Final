#' Climate variable regression
#' 
#' @param df Dataframe with climate variables of interest
#' @param year_start Beginning of timeframe of interest
#' @param year_end End of timeframe of interest
#' @param dep_var Variable in the dataframe to be used as the dependent variable in simple linear regression
#' @param ind_var Variable in the dataframe to be used as the independent variable in simple linear regression
#' @param plot T/F If true, a plot of the linear regression is returned with the slope of the regression line
#' 

climate_var_regress = function(df,year_start,year_end,ind_var,dep_var,plot=FALSE){
  
 df_fltr = df %>% 
    filter(year %in% year_start:year_end) 
  
 # Error checking
  if(is.character(df$dep_var)==TRUE)
    return("Dependent variable is in character format. Convert to numeric or factor then rerun function")
  
  if(is.character(df$ind_var)==TRUE) 
    return("Independent variable is in character format. Convert to numeric or factor then rerun function")
  
  # Linear regression on variables of interest
  lm = lm(paste0(dep_var, "~", ind_var),data=df_fltr)
  coefs = lm$coef
  
  # Optional plot of results
  if(plot){
    
     lm_plot = ggplot(df_fltr,aes_string(x = ind_var, y = dep_var))+
       geom_smooth(method = "lm")+
       theme_classic()+
       labs(title = "Simple Linear Regression",subtitle=paste0("Slope = ",format(coefs[2],digits = 4),
                         "; R-squared = ", format(summary(lm)$r.squared,digits=4)))+
       theme(plot.title = element_text(size=10,face = "bold",hjust=0.5),
             plot.subtitle = element_text(size=9,hjust=0.5))
     
     scatter_plot = ggplot(df_fltr,aes_string(x = ind_var, y = dep_var))+
       geom_point(color="blue",alpha =0.3,size=0.5)+
       ggtitle("Scatter plot")+
       theme(plot.title = element_text(size=10,face="bold",hjust=0.5),
             axis.title = element_text(size=10))
     
     hist_indvar =  ggplot(df_fltr,aes_string(x = ind_var))+
       geom_histogram(color="black",fill="lightgrey")+
       ggtitle("Independent variable histogram")+
       theme(axis.text = element_text(size=9),
             axis.title = element_text(size=10),
             plot.title = element_text(size=10))
     
     hist_depvar =  ggplot(df_fltr,aes_string(x = dep_var))+
       geom_histogram(color="black",fill="lightgrey")+
       ggtitle("Dependent variable histogram")+
       theme(axis.text = element_text(size=9),
             axis.title = element_text(size=10),
             plot.title = element_text(size=10))
     
     plotgrid = plot_grid(lm_plot,scatter_plot,hist_indvar,hist_depvar,ncol=2)
     
     return(list(coefs = coefs, plots = plotgrid))
     }
  
  return(coefs)
}
