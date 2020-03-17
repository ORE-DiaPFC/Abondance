#fit.mcmc <- as.mcmc(fit)

hyperparameters <-c(
  ## HYPER PARAMETERS 
  paste0("logit_int_MC[",1:2,"]") # intercept
  ,paste0("logit_flow_MC[",1:2,"]") # slope of flow effect
  ,paste0("sigmap_eff[",1:2,"]") # standard deviation of trap efficiency 
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,paste0("s[",1:4,"]") # Dirichlet parameter
  
  ,paste0("Plambda[",1:4,"]") # Proportion distribution
  #,"Plambda0" # Initiate proportion distribution
  
  ,paste0("mup_recap[",1:4,"]")  # Mean of the probabilities to be re-captured
  ,paste0("sigmap_recap[",1:4,"]")  # Standard deviation of the probabilities to be re-captured 
 # ,"p_recap"
  
  ## PROBABILITIES
  ,"pi_MC" # Probability of capture
  ,paste0("p_MC90[",1:2,"]") # decrease in probability of capture in 1990
#  ,"epsilon_MC" # standardized residuals of probability of capture
#  ,"test"
  
) 

pdf(paste('results/PosteriorCheck_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%hyperparameters)])
#traplot(fit.mcmc,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}

traplot(fit.mcmc, paste0("epsilon_MC[",1:Nyears,",1]"))
traplot(fit.mcmc, paste0("epsilon_MC[",1:Nyears,",2]"))

par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("pi_MC[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc,paste0("pi_MC[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("epsilon_MC[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc,paste0("epsilon_MC[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("p_recap[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc,paste0("p_recap[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("p_recap[",1:data$Nyears,",3]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc,paste0("p_recap[",1:data$Nyears,",4]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("Plambda[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc,paste0("Plambda[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("Plambda[",1:data$Nyears,",3]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc,paste0("Plambda[",1:data$Nyears,",4]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

dev.off()
