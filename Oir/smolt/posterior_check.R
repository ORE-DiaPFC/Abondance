#fit <- as.mcmc(fit)

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
#traplot(fit[,which(varnames(fit)%in%hyperparameters)])
#traplot(fit,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit,par) 
  denplot(fit,par) 
}

traplot(fit,"overdisp_MC") 
traplot(fit,"eps_p_MC") 
traplot(fit,"mean_MC") 
traplot(fit,"p_MC") 
#traplot(fit,"alpha_MC") 
#traplot(fit,"beta_MC") 

par(mfrow=c(2,1))
caterplot(fit, "overdisp_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit, "eps_p_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 

#par(mfrow=c(2,1))
caterplot(fit, "mean_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit, "p_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 

#caterplot(fit, "alpha_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
#caterplot(fit, "beta_MC", reorder = FALSE, horizontal=FALSE, style=c("plain")) 

#par(mfrow=c(2,1))
caterplot(fit, "p1c", reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit, "p1y", reorder = FALSE, horizontal=FALSE, style=c("plain")) 

caterplot(fit, paste0("Nc[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit, paste0("Nc[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

dev.off()
