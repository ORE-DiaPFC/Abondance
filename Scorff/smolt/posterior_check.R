




#fit <- as.mcmc(fit)


hyperparameters <-c(
  ## HYPER PARAMETERS 
  paste0("logit_int[",1:2,"]") # intercept (mean) probability of capture (1: MP; 2: ML)
  ,paste0("logit_flow[",1:2,"]")  # slope for flow effect in probability of capture
  ,paste0("sigmap[",1:2,"]") # standard deviation of probability of capture
  ,"rho" # correlation coefficient between probability of capture at the two traps
  
  #,"l_ML_dim" # mean decrease in probability of capture when Lesl? is not working
  # ,"sigmap_ML_dim" # standard deviation of decrease in probability of capture when Lesl? is not working
  # ,"junk" # variance of decrease in probability of capture when Lesl? is not working
  
  ## PROBABILITIES
 # ,"p" # probability of capture at the 2 traps (1: MP; 2: ML 
 # ,"epsilon" # standardized residuals of probability of capture
  
  #,"pi_ML_dim" # decrease in the probability of capture at Lesl?
  #,"epsML_dim" # standardized residuals of the decrease in the probability of capture
  
#  ,"p_MP" # capture probability at Moulin des Princes
#  ,"p_ML" # capture probability at Moulin de Lesl?
  
  ## POPULATION
#,"Ntot" # annual number of smolt
, "N" #  number of smolt by cohort
#,"Nc" #  number of smolt by age
#,"p1c" # proportion smolt 1+ / cohort
#,"p1y" # proportion smolt 1+ / year
,"l1","l2"
,"s1","s2"
#,"mu_p1c"
#,"alpha"

,"p10c"
,"lambda0"
,"N0"
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,"mean_gamma" # Mean parameter of gamma distribution
  ,"var_gamma" # Variance parameter of gamma distribution
  
 # ,paste0("lambda[",1:data$Nyears,"]") # Poisson parameter
,"lambda"

#,paste0("p1c[",1:data$Nyears,"]") # Poisson parameter
,"p1c"
  
#  ,"Nesc" # Number of smolt escaping the river (Ntot-Dead
  
  #### TEST
  # 1: is logit_flow at MP >=0 ?
  # 2: is logit_flow at ML >=0 ?
#  ,"test" 
  # ,"R2"
  
) 


pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))

for (par in hyperparameters){
  traplot(fit,par) 
  denplot(fit,par) 
}


par(mfrow=c(2,1))
caterplot(fit,paste0("epsilon[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("epsilon[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit,paste0("N[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("Ntot[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(1,1))
caterplot(fit,paste0("p1c[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

par(mfrow=c(2,1))
caterplot(fit,paste0("Nc[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("Nc[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

dev.off()

