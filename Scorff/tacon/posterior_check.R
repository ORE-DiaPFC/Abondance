#fit <- as.mcmc(fit)
#load(paste0('results/Results_',stade,"_",year,'.RData'))

hyperparameters <-c(
  ##### INTERCALIBRATION
  "int_width" # intercept of linear relationship between CPUE and density 
  ,"width_coef" # proportional coefficient with width
  ,"rate_lcpu" # inverse scale of gamma distribution
  ,"p_cpue"
  ,"log_k_inter"
  
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
  ,"sigma_d"
  #,"rate_d"
  
  ,"eps_ydSc"
  ####################
  # DENSITY
 # ,paste0("eps_ydSc[",1:data$Nyear,"]")
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
  traplot(fit,par) 
  denplot(fit,par) 
}

years <- 1993:year

#rate_lcpu <- as.vector(fit$sims.list$rate_lcpu)
#sigma_dSc <-  as.vector(fit$sims.list$sigma_dSc)
#plot(rate_lcpu, sigma_dSc)

caterplot(fit,"year_dSc", reorder = FALSE, horizontal=FALSE, style=c("plain"), labels=years) 
title("Effect on densities (year_dSc)")
# Extract MCMC medians for p_male[,1]
medians <- fit$median$year_dSc
#years <- 1994:year  # X-axis values
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:data$Nyear, predict(loess_fit_1), col = "tomato", lwd = 2)


caterplot(fit,"site_Sc", reorder = FALSE, horizontal=FALSE, style=c("plain"))
title("Site effect (site_Sc,on log scale)")
# Extract MCMC medians for p_male[,1]
medians <- fit$median$site_Sc
#years <- 1994:year  # X-axis values
# Fit LOESS model and add smoothing line
sites <- 1:53
loess_fit_1 <- loess(medians ~ sites, span = 0.5)  
lines(sites, predict(loess_fit_1), col = "tomato", lwd = 2)


caterplot(fit,"eps_ydSc", reorder = FALSE, horizontal=FALSE, style=c("plain")) 


par(mfrow=c(1,1))
#years <- 1993:year  
#caterplot(fit,paste0("ntot[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit,paste0("ntot_Sc[",1:data$Nyear,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels=years) 
title("Total abundance of parr0+ in Scorff + tributary rivers")
medians <- fit$median$ntot_Sc# X-axis values
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:length(years), predict(loess_fit_1), col = "tomato", lwd = 2)

dev.off()

