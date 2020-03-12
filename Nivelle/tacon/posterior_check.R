fit.mcmc <- as.mcmc(fit)

##### BRESLE

#### ADU
hyperparameters <-c(
  "mu_p_srem" # Mean capture probability by successive removal
  ,"sd_logit_p_srem" # Standard deviation of the logit transformed capture probability
  ,"epsilon_p" # decrease of efficiency between successive passes
  
  ## Density
  ,"beta_dj" # habitat effect 
  ,"alpha_dj" # Year effect
  ,"gamma_dj" # Interaction year X zone
  ,"pi_dj" # Density dependence effect when doing restocking
  ,"xi_dj" # Zone effect in density dependence when doing restocking
  ,"eta_dj" # Inverse scale of a Gamma distribution for density per zone
  ,"zeta_alpha_dj" # Shape and inverse scale of a Gamma distribution for year effect
  ,"zeta_gamma_dj" # Shape parameter of a Gamma distribution for interaction year X zone
  ,"eta_gamma_dj" # Inverse scale parameter of a Gamma distribution for interaction year X zone
  ,"mu_dj_nat" # Mean density issued from reproduction
  ,"delta" # Juvenile survival depending on stage
  ,"k_cpue" # Coefficient of proportionality between mean CPUE and density
  #,"eta_cpue" # Inverse scale parameter of gamma distribution for mean CPUE
  ,"rho_s"  # nonlinear relationshihp between flow and surface
  ,"sd_s_rec" # Standard deviation of the recent surface of stations
  ,"p_cpue" # Non informative prior used to parameterize mean CPUE 
) 


pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%hyperparameters)])
#traplot(fit.mcmc,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}

# traplot(fit.mcmc, paste0("eps_U[",1:data$Y,",1]"))
# traplot(fit.mcmc, paste0("eps_U[",1:data$Y,",2]"))
# traplot(fit.mcmc, paste0("eps_Ol[",1:data$Y,"]"))
# 
par(mfrow=c(1,1))
caterplot(fit.mcmc,"alpha_dj", reorder = FALSE, horizontal=FALSE, style=c("plain")) 


par(mfrow=c(1,1))
caterplot(fit.mcmc,"gamma_dj", reorder = FALSE, horizontal=FALSE, style=c("plain")) 


dev.off()

