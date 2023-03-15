
#fit <- as.mcmc(fit)

hyperparameters <-c(
  "pi_Eu00" # decrease in probability of capture in Eu in 2000
  ,"pi_Eu01" # decrease in probability of capture in Eu in 2001
  ,"logit_int_Eu" # intercept
  ,"logit_flow_Eu" # slope of flow effect
  ,"lflow_fall_Eu" # slope of flow effect in fall
  ,"sigmapi_Eu" # standard deviation in probability of capture in Eu
  #,"epsilon_Eu" # standardized residuals of probability of capture in Eu
  
  ,"mupi_B" # mean probability of capture in Beauchamps
  ,"sigmapi_B" # standard deviation in probability of capture in Beauchamps
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  
  #,"pi_B_eff"
  ,"p_dev"
  #,"eff_B"
) 
pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit[,which(varnames(fit)%in%hyperparameters)])
#traplot(fit,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit,par) 
  denplot(fit,par) 
}

traplot(fit, paste0("epsilon_Eu[",1:data$Y,",1]"))
traplot(fit, paste0("epsilon_Eu[",1:data$Y,",2]"))
par(mfrow=c(2,1))
# 1SW
caterplot(fit,paste0("epsilon_Eu[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# MSW
caterplot(fit,paste0("epsilon_Eu[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


caterplot(fit,paste0("eff_B[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

# Capture proba. Beauchamps (logit scale)
par(mfrow=c(2,1))
# 1SW
caterplot(fit,paste0("logit_pi_B[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

# MSW
caterplot(fit,paste0("logit_pi_B[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


# Capture proba. Beauchamps effective
par(mfrow=c(2,1))
# 1SW
caterplot(fit,paste0("pi_B_eff[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

# MSW
caterplot(fit,paste0("pi_B_eff[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


# N 
caterplot(fit,paste0("n_tot[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


par(mfrow=c(2,1))
# 1SW
caterplot(fit,paste0("n_1SW[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

# MSW
caterplot(fit,paste0("n_MSW[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


dev.off()

