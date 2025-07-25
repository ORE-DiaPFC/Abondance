#fit <- as.mcmc(fit)

hyperparameters <-c(
  ## HYPER PARAMETERS 
  "sd_prem" # sd of probability of capture during successive removals
  ,"mup_rem" # mean of probability of capture during successive removals (probability scale
  
  ,"int_width" # constant linked to the relationship with width
  ,"width_coef" # factor of proportionality with river's width
  ,"rate_lcpu"  # rate (inverse scale of the gamma distribution for lambda_cpu
  
  ,"sigma_dOir" # Overall standard deviation in density in Oir river
  ,"mu_ydOir" # Mean annual density
  ,"sigma_yOir" # Standard deviation of year effect in density
 # ,"year_dOir" # year effect in density 
  ,"sigma_gryrOir" # Standard deviation of interaction between group and year
  
  ,"coef_PC" # coefficient of proportionality for run.
 ,"k_inter"
  
  #,"n_Oir"  # annual abundance in Oir river 
 # , paste0("n_Oir[",1:data$Nyear,"]")
 # ,"n_MB" # annual abundance per group in Moulin du Bois
 # ,"n_PL" # annual abundance per group in Pont Levesque
 # ,"n_LR" # annual abundance per group in La Roche
 # ,"ntot_Oir" # total annual abundance (Oir + tributary rivers
) 

pdf(paste('results/PosteriorCheck_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit[,which(varnames(fit)%in%hyperparameters)])
#traplot(fit,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit,par) 
  denplot(fit,par) 
}

par(mfrow=c(2,1))
caterplot(fit, "year_dOir", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit, paste0("n_Oir[",1:data$Nyear,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

caterplot(fit, "n_MB", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit, "n_PL", reorder = FALSE, horizontal=FALSE, style=c("plain")) 

caterplot(fit, "n_LR", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit, "ntot_Oir", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
dev.off()
