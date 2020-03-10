




fit.mcmc <- as.mcmc(fit)

##### BRESLE

#### ADU
hyperparameters <-c(
  "pi_Eu00" # decrease in probability of capture in Eu in 2000
  ,"pi_Eu01" # decrease in probability of capture in Eu in 2001
  ,"logit_int_Eu" # intercept
  ,"logit_flow_Eu" # slope of flow effect
  ,"lflow_fall_Eu" # slope of flow effect in fall
  ,"sigmapi_Eu" # standard deviation in probability of capture in Eu
  ,"epsilon_Eu" # standardized residuals of probability of capture in Eu
  
  ,"mupi_B" # mean probability of capture in Beauchamps
  ,"sigmapi_B" # standard deviation in probability of capture in Beauchamps
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
) 
pdf(paste('results/Results_',stade,"_",year,'.pdf',sep=""))
#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%hyperparameters)])
#traplot(fit.mcmc,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}

traplot(fit.mcmc, paste0("epsilon_Eu[",1:36,",1]"))
traplot(fit.mcmc, paste0("epsilon_Eu[",1:36,",2]"))
par(mfrow=c(2,1))
# 1SW
caterplot(fit.mcmc,paste0("epsilon_Eu[",1:36,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# MSW
caterplot(fit.mcmc,paste0("epsilon_Eu[",1:36,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

# Capture proba. Beauchamps (logit scale)
par(mfrow=c(2,1))
# 1SW
caterplot(fit.mcmc,paste0("logit_pi_B[",1:36,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

# MSW
caterplot(fit.mcmc,paste0("logit_pi_B[",1:36,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


dev.off()










### JUV
hyperparameters <-c(
## HYPER PARAMETERS 
"mu_B" # mean probability of capture in Beauchamps
,"sigmap_B" # standard deviation in probability of capture in Beauchamps

,"logit_int_Eu" # intercept in probability of capture in Eu
,"logit_flow_Eu" # slope effect of flow in probability of capture in Eu
,"sigmap_Eu" # # standard deviation in probability of capture in Eu

## PROBABILITIES
,"p_B" # probabilities of capture in Beauchamps (without the decrease
,"p_B95" # decrease in probabilities of capture in Beauchamps in 1995
,"p_B00" # decrease in probabilities of capture in Beauchamps in 2000
,"p_B02" # decrease in probabilities of capture in Beauchamps in 2002
,"p_Btot" # global probabilities of capture in Beauchamps
,"epsilon_B" # standardized residuals in probability of capture in Beauchamps

,"p_Eu" # probabilites of capture in Eu
,"epsilon_Eu" # standardized residuals in probability of capture in Eu


## POPULATION
#,"Ntot" # Number of smolt 

,"shape_lambda" # Shape parameter of gamma distribution
,"rate_lambda" # Rate parameter of gamma distribution
,"mean_gamma" # Mean parameter of gamma distribution
,"var_gamma" # Variance parameter of gamma distribution
,"p_keep"
)

pdf(paste('results/Results_',stade,"_",year,'.pdf',sep=""))
#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%hyperparameters)])
#traplot(fit.mcmc,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}

par(mfrow=c(2,1))
# 1SW
caterplot(fit.mcmc,paste0("epsilon_Eu[",1:25,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# MSW
caterplot(fit.mcmc,paste0("epsilon_B[",1:25,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


dev.off()


