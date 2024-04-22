#fit <- as.mcmc(fit)


hyperparameters <-c(
  ## HYPER PARAMETERS 
  paste0("logit_int_MP[",1:2,"]") # intercept (mean probability of capture at MP
  ,paste0("logit_flow_MP[",1:2,"]") # slope for flow data in probability of capture at MP
  ,"sigmapi_MP" # standard deviation of probability of capture at Moulin des Princes

  ,paste0("mupi_F[",1:2,",",1,"]")
  ,paste0("mupi_F[",1:2,",",2,"]") # mean probability of dying from fishing
  ,paste0("sigmapi_F")#[",1:2,"]") # standard deviation of probability of dying from fishing
#  ,paste0("rho_F[",1:2,"]") # correlation coefficient between probability of dying from fishing between marked and unmarked depending on sea age
#,"sigmapi_F"
, "rho_F"

  ,paste0("logit_int_R[",1:2,"]") # intercept (mean probability of recapture during or after reproduction
  ,paste0("logit_effort_R[",1:2,"]") # slope for effort (number of nights in probability of recapture during reproduction
  ,paste0("logit_flow_R[",1:2,"]") # slope for flow data in probability of recapture during reproduction
  ,"sigmapi_R" # standard deviation of probabilty of recapture during reproduction

  # ,paste0("pi_R_pulsium[",27:data$Y,",1]")
  # ,paste0("pi_R_pulsium[",27:data$Y,",2]")

  ,paste0("mupi_D[",1:2,",1]") # mean probability to die from natural cause
  ,paste0("mupi_D[",1:2,",2]") # mean probability to die from natural cause
#  ,paste0("sigmapi_D[",1:2,"]") # standard deviation of probability of dying from natural cause 
 # ,paste0("rho_D[",1:2,"]") # correlation coefficient between probability of dying between marked and unmarked depending on sea age
,"sigmapi_D"
, "rho_D"

 # ,"mupi_oF" # mean probability of recovering a caught fish (from fishing
 # ,"sigmapi_oF" # standard deviation of probability of recovering a caught fish (from fishing

  ### STATISTIC
  # 1: is logit_flow >=0 for 1SW?
  # 2: is logit_flow >=0 for MSW?
  # 3: is the mean mortality due to fishing different between marked and unmarked 1SW?
  # 4: is the mean mortality due to fishing different between marked and unmarked MSW? 
  # 5: is the mean mortality due to natural cause different between marked and unmarked 1SW?
  # 6: is the mean mortality due to natural cause different between marked and unmarked MSW?
  # 7: is logit_effort >=0 for 1SW? 
  # 8: is logit_effort >=0 for MSW? 
  # 9: is logit_flow >=0 in probability of recapture for 1SW?
  # 10: is logit_flow >=0 in probability of recapture for MSW? 
 # ,"test"
  
  ,"diffF_1SW" # difference in mean in mortality due to fishing between marked and unmarked 1SW 
  ,"diffF_MSW" # difference in mean in mortality due to fishing between marked and unmarked MSW
  ,"diff1SW" # difference in mean in mortality due to natural cause between marked and unmarked 1SW 
  ,"diffMSW" # difference in mean in mortality due to natural cause between marked and unmarked MSW 
  
  ## PROBABILITIES
  ,"pi_MP" # probability of capture at Moulin des Princes
  ,paste0("pi_MP94[",1:2,"]") # decrease in probability of capture in 1994
  ,paste0("p_MP94_tot[",1:2,"]") # total probability of capture in 1994
#  ,"epsilon_MP" # standardized residuals of probability of capture at MP
  
  ,paste0("pi_MP20[",2,"]") # decrease in probability of capture in 1994
  ,paste0("p_MP20_tot[",2,"]") # total probability of capture in 1994

#  ,"pi_oF" # probability to recover a caught fish from fishing (from 1994 to 2002
#  ,"piF_1SW" # probability of dying from fishing for 1SW depending on being marked or not
#  ,"piF_MSW" # probability of dying from fishing for MSW depending on being marked or not
  
 ,"pi_oD" # probability to recover a dead fish
 # ,"piD_1SW" # probability of dying from natural cause for 1SW depending on being marked or not
 # ,"piD_MSW" # probability of dying from natural cause for MSW depending on being marked or not
  
 # ,"pi_R" # probability of recapture during or after reproduction
 # ,"epsilon_R" # standardized residuals of probability of recapture during or after reproduction
  
  
  ## POPULATION
#  ,"n_tot" # total annual number of adults entering the river
#  ,"n_1SW" # annual number of 1SW
#  ,"n_MSW" # annual number of MSW
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda"  # Rate parameter of gamma distribution
  
  ,"lambda_tot0"  # Initiate lambda distribution
  ,paste0("Plambda0[",1:2,"]") # Initiate proportion distribution
  #,"lambda_tot" # Poisson parameter
  #,"Plambda" # Proportion distribution
  
  ,paste0("s[",1:2,"]") # Dirichlet parameter 
  
#  ,"e_tot" # total annual escapement
#  ,"e_1SW" # annual escapement of 1SW
#  ,"e_MSW" # annual escapement of MSW
  
) 


pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))

for (par in hyperparameters){
  traplot(fit,par)
  denplot(fit,par)
}


par(mfrow=c(2,1))
caterplot(fit,paste0("piD_1SW[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("piD_1SW[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit,paste0("piD_MSW[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("piD_MSW[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


par(mfrow=c(2,1))
caterplot(fit,paste0("piF_1SW[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("piF_1SW[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit,paste0("piF_MSW[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("piF_MSW[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

# par(mfrow=c(2,1))
# caterplot(fit,paste0("pi_oF[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# caterplot(fit,paste0("pi_oF[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit,paste0("pi_R[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("pi_R[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit,paste0("epsilon_MP[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("epsilon_MP[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit,paste0("epsilon_R[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("epsilon_R[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

# ### Proportion male par 1SW et MSW
# par(mfrow=c(2,1))
# caterplot(fit,paste0("p_male[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# caterplot(fit,paste0("p_male[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


par(mfrow=c(1,1))
caterplot(fit,"n_tot", reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1994:year)
title("ntot")

par(mfrow=c(2,1))
caterplot(fit,"n_1SW", reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1994:year)
title("n_1SW")
caterplot(fit,"n_MSW", reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1994:year)
title("n_MSW")

#par(mfrow=c(2,1))
#caterplot(fit,paste0("sex_ratio_1SW"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1994:year)
#title("sex_ratio_1SW")
#caterplot(fit,paste0("sex_ratio_MSW"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1994:year)
#title("sex_ratio_MSW")

par(mfrow=c(2,1))
caterplot(fit,paste0("p_male[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1994:year)
title("pmale_1SW")
caterplot(fit,paste0("p_male[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1994:year)
title("pmale_MSW")

### Proportion echnatillonés pour sexage
par(mfrow=c(1,1))
caterplot(fit,paste0("p_smp[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("p_smp[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


dev.off()

