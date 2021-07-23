#fit.mcmc <- as.mcmc(fit)

hyperparameters <-c(
  ## PROBABILITIES 
  "logit_int_MC" # intercept 
  ,"logit_flow_MC" #slope for flow data (April
  ,"log_cess_MC" #slope for capture effort (number of marking sessions
  
  
  # POPULATION
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,"mean_gamma" # Mean parameter of gamma distribution
  ,"var_gamma" # Variance parameter of gamma distribution
  
  ,"lambda" # Poisson parameter
  #,"Ntot" # Number of smolt 
  #,"Nesc" # Number of smolt escaping the river (Ntot-Dead
 # ,"eps_p_MC"
 # ,"overdisp_MC" # Annual overdispersion of capture probability
 # ,"mean_MC" # Mean capture probability
 # ,"p_MC" # Annual capture of probability
 # ,"alpha_MC" # Alpha parameter of the beta binomiale distribution
 # ,"beta_MC" # Beta parameter of the beta binomiale distribution
  
  
) 

pdf(paste('results/PosteriorCheck_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%hyperparameters)])
#traplot(fit.mcmc,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}

traplot(fit.mcmc,"overdisp_MC") 
traplot(fit.mcmc,"eps_p_MC") 
traplot(fit.mcmc,"mean_MC") 
traplot(fit.mcmc,"p_MC") 
traplot(fit.mcmc,"alpha_MC") 
traplot(fit.mcmc,"beta_MC") 

par(mfrow=c(2,1))
caterplot(fit.mcmc, "overdisp_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc, "eps_p_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 

#par(mfrow=c(2,1))
caterplot(fit.mcmc, "mean_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc, "p_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 

caterplot(fit.mcmc, "alpha_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc, "beta_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 

dev.off()
