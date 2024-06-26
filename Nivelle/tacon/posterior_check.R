#fit <- as.mcmc(fit)

##### BRESLE

#### ADU
hyperparameters <-c(
  "mu_p_srem" # Mean capture probability by successive removal
  ,"sd_logit_p_srem" # Standard deviation of the logit transformed capture probability
  ,"epsilon_p" # decrease of efficiency between successive passes
  
  ## Density
  ,"beta_dj" # habitat effect 
  #,"alpha_dj" # Year effect
  #,"gamma_dj" # Interaction year X zone
  ,"pi_dj" # Density dependence effect when doing restocking
  ,"xi_dj" # Zone effect in density dependence when doing restocking
  ,"eta_dj" # Inverse scale of a Gamma distribution for density per zone
  ,"zeta_alpha_dj" # Shape and inverse scale of a Gamma distribution for year effect
  ,"zeta_gamma_dj" # Shape parameter of a Gamma distribution for interaction year X zone
  ,"eta_gamma_dj" # Inverse scale parameter of a Gamma distribution for interaction year X zone
  ,"mu_dj_nat" # Mean density issued from reproduction
  ,"delta" # Juvenile survival depending on stage
  ,"k_cpue" # Coefficient of proportionality between mean CPUE and density
  ,"eta_cpue" # Inverse scale parameter of gamma distribution for mean CPUE
  ,"rho_s"  # nonlinear relationshihp between flow and surface
  ,"sd_s_rec" # Standard deviation of the recent surface of stations
  ,"p_cpue" # Non informative prior used to parameterize mean CPUE 
  ,"k_cpue_Puls" # # Coefficient of proportionality for CPUE using Pulsium
  ,"log_k_inter"
) 


pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit[,which(varnames(fit)%in%hyperparameters)])
#traplot(fit,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit,par)
  denplot(fit,par)
}

#traplot(fit, alpha_dj)
#traplot(fit, gamma_dj)

par(mfrow=c(1,1))
caterplot(fit,"alpha_dj", reorder = FALSE, horizontal=FALSE, style=c("plain"))

par(mfrow=c(1,1))
caterplot(fit,paste0("gamma_dj[",1:data$Y_last,",4]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

par(mfrow=c(1,1))
caterplot(fit,paste0("gamma_dj[",1:data$Y_last,",5]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))


dev.off()

