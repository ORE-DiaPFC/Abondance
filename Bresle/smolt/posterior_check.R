
#fit <- as.mcmc(fit)


### JUV
hyperparameters <-c(
## HYPER PARAMETERS 
  "logit_int_B"
  ,"logit_flow_B"
  ,"sigmap_B"
  ,"exp_d_p_B"
  
  ,"logit_int_Eu" # intercept in probability of capture in Eu
  ,"logit_flow_Eu" # slope effect of flow in probability of capture in Eu
  ,"sigmap_Eu" # # standard deviation in probability of capture in Eu
  ,"exp_d_p_Eu"
  
  ## PROBABILITIES
  #,"p_B" # probabilities of capture in Beauchamps (without the decrease
  #,"p_Btot" # global probabilities of capture in Beauchamps
  ,"epsilon_B" # standardized residuals in probability of capture in Beauchamps
  
  #,"p_Eutot"
  #,"p_Eu" # probabilites of capture in Eu
  ,"epsilon_Eu" # standardized residuals in probability of capture in Eu
  
  
  ## POPULATION
  #,"Ntot" # Number of smolt 
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,"mean_gamma" # Mean parameter of gamma distribution
  ,"var_gamma" # Variance parameter of gamma distribution
  
  ,"lambda" # Poisson parameter
  
  #,"Nesc" # Number of smolt escaping the river (Ntot-Dead)
  
)

pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit[,which(varnames(fit)%in%hyperparameters)])
#traplot(fit,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit,par) 
  denplot(fit,par) 
}

par(mfrow=c(2,1))
caterplot(fit,paste0("epsilon_Eu[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("epsilon_B[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(1,1))
caterplot(fit,paste0("Ntot[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels=1982:year) 

dev.off()


