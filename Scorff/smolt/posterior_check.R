




fit.mcmc <- as.mcmc(fit)


hyperparameters <-c(
  ## HYPER PARAMETERS 
  "logit_int" # intercept (mean) probability of capture (1: MP; 2: ML)
  ,"logit_flow" # slope for flow effect in probability of capture
  ,"sigmap" # standard deviation of probability of capture
  ,"rho" # correlation coefficient between probability of capture at the two traps
  
  #,"l_ML_dim" # mean decrease in probability of capture when Lesl? is not working
  # ,"sigmap_ML_dim" # standard deviation of decrease in probability of capture when Lesl? is not working
  # ,"junk" # variance of decrease in probability of capture when Lesl? is not working
  
  ## PROBABILITIES
 # ,"p" # probability of capture at the 2 traps (1: MP; 2: ML 
  ,"epsilon" # standardized residuals of probability of capture
  
  #,"pi_ML_dim" # decrease in the probability of capture at Lesl?
  #,"epsML_dim" # standardized residuals of the decrease in the probability of capture
  
#  ,"p_MP" # capture probability at Moulin des Princes
#  ,"p_ML" # capture probability at Moulin de Lesl?
  
  
  ## POPULATION
 # ,"Ntot" # annual number of smolt
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,"mean_gamma" # Mean parameter of gamma distribution
  ,"var_gamma" # Variance parameter of gamma distribution
  
  ,"lambda" # Poisson parameter
  
#  ,"Nesc" # Number of smolt escaping the river (Ntot-Dead
  
  #### TEST
  # 1: is logit_flow at MP >=0 ?
  # 2: is logit_flow at ML >=0 ?
#  ,"test" 
  # ,"R2"
  
) 


pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))

for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}


par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("epsilon[",1:data$Nyears,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit.mcmc,paste0("epsilon[",1:data$Nyears,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

dev.off()

