#fit.mcmc <- as.mcmc(fit)

hyperparameters <-c(
  ## HYPER PARAMETERS 
  "logit_int_MC" # intercept
  ,"logit_flow_MC" # slope of flow effect
  ,"sigmap_eff" # standard deviation of trap efficiency 
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,"s" # Dirichlet parameter
  
  ,"Plambda" # Proportion distribution
  ,"Plambda0" # Initiate proportion distribution
  
  ,"mup_recap"  # Mean of the probabilities to be re-captured
  ,"sigmap_recap" # Standard deviation of the probabilities to be re-captured 
  ,"p_recap"
  
  ## PROBABILITIES
  ,"pi_MC" # Probability of capture
  ,"p_MC90" # decrease in probability of capture in 1990
  ,"epsilon_MC" # standardized residuals of probability of capture
#  ,"test"
  
) 

pdf(paste('results/PosteriorCheck_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%hyperparameters)])
#traplot(fit.mcmc,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}

#traplot(fit.mcmc, paste0("epsilon_MC[",1:Nyears,",1]"))
#traplot(fit.mcmc, paste0("epsilon_MC[",1:Nyears,",2]"))
par(mfrow=c(2,1))
# 1SW
caterplot(fit.mcmc,paste0("epsilon_MC[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# MSW
caterplot(fit.mcmc,paste0("epsilon_MC[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


par(mfrow=c(2,1))
# 1SW
caterplot(fit.mcmc,paste0("p_recap[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# MSW
caterplot(fit.mcmc,paste0("p_recap[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
# M
caterplot(fit.mcmc,paste0("p_recap[",1:data$Nyears,",3]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# F
caterplot(fit.mcmc,paste0("p_recap[",1:data$Nyears,",4]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

dev.off()
