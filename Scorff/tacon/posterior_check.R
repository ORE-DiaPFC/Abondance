#fit.mcmc <- as.mcmc(fit)


hyperparameters <-c(
  ##### INTERCALIBRATION
  "int_width" # intercept of linear relationship between CPUE and density 
  ,"width_coef" # proportional coefficient with width
  ,"rate_lcpu" # inverse scale of gamma distribution
  ,"p_cpue"
  
  ######### ABUNDANCE INDEX SCORFF
  ## HYPER PARAMETERS
  ,"sigma_dSc" # overall standard deviation in density
  
  # Year effect (log scale
  ,"int_ydSc" # intercept (mean 
  ,"log_flow" # slope of flow effect
  ,"sigma_ySc" # standard deviation
  
  # Site effect (log scale
  ,"sigma_siteSc" # standard deviation
  
  ,"mup_rem"
  ,"sd_prem"
  ,"eps"
  ,"mu_d"
  ,"rate_d"
  
  ####################
  # DENSITY
  ,paste0("eps_ydSc[",1:data$Nyear,"]")
 # ,paste0("year_dSc[",1:data$Nyear,"]") # annual year effect in density (log scale
 # ,"site_Sc" # site effect in density (log scale
  
  # Annual density per site and year corrected by observed flow when habitat sampling was done
#  ,"d_Sc_pred"  
  
  ## POPULATION
#  ,"ntot_Sc" # annual abundance
  
  # STATISTIC
#  ,"test" #is log_flow >=0?
) 


pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))

for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}

rate_lcpu <- as.vector(fit.mcmc[,"rate_lcpu"][[1]])
sigma_dSc <-  as.vector(fit.mcmc[,"sigma_dSc"][[1]])
plot(rate_lcpu, sigma_dSc)

caterplot(fit.mcmc,"year_dSc", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc,"site_Sc", reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit.mcmc,"eps_ydSc", reorder = FALSE, horizontal=FALSE, style=c("plain")) 


#par(mfrow=c(2,1))
#caterplot(fit.mcmc,paste0("epsilon[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
#caterplot(fit.mcmc,paste0("epsilon[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

dev.off()

