




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
  #,"lambda0"
  ,"N0"
  
  #,"shape_lambda" # Shape parameter of gamma distribution
  #,"rate_lambda" # Rate parameter of gamma distribution
  #,"mean_gamma" # Mean parameter of gamma distribution
  #,"var_gamma" # Variance parameter of gamma distribution
  ,"shape_theta","mean_theta","sigma_theta"
  
  # ,paste0("lambda[",1:data$Nyears,"]") # Poisson parameter
  #,"lambda"
  ,"theta"
  ,"theta0"
  
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

years <- 1995:year


par(mfrow=c(2,1))
caterplot(fit,paste0("p_MP[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = years)
title("Capture probability at Moulin des Princes")
# Extract MCMC medians for p_male[,1]
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, grep("^p_MP\\[", colnames(all_samples))]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$p_MP
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:data$Nyears, predict(loess_fit_1), col = "tomato", lwd = 2)


caterplot(fit,paste0("p_ML[",3:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1997:year)
title("Capture probability at Moulin du Lesle")
# Extract MCMC medians for p_male[,1]
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, grep("^p_ML\\[", colnames(all_samples))]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$p_ML
}
# Fit LOESS model and add smoothing line
tmp <- c(1997:year)
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:length(tmp), predict(loess_fit_1), col = "tomato", lwd = 2)



par(mfrow=c(2,1))
caterplot(fit,paste0("epsilon[",1:data$Nyears,", 1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
title("Probability of capture at MP (standardized residuals)")
caterplot(fit,paste0("epsilon[",3:data$Nyears,", 2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
title("Probability of capture at Lesle (standardized residuals)")


# Ntot[t]: annual total smolt population size 
# Nesc[t]: annual smolt population size escaping the river
par(mfrow=c(2,1))
caterplot(fit,paste0("Ntot[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = years)
title("Total number of smolt")
# Extract MCMC medians for p_male[,1]
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, grep("^Ntot\\[", colnames(all_samples))]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$Ntot
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:data$Nyears, predict(loess_fit_1), col = "tomato", lwd = 2)

caterplot(fit,paste0("Nesc[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = years)
title("Total number of smolt escaped (not dead)")
# Extract MCMC medians for p_male[,1]
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, grep("^Nesc\\[", colnames(all_samples))]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$Nesc
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:data$Nyears, predict(loess_fit_1), col = "tomato", lwd = 2)



# smolts numbers by cohort (year of birth+1)
par(mfrow=c(2,1))
caterplot(fit,paste0("Nc[",1:data$Nyears,", 1]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = years)
title("1+ smolts numbers by cohort (year of birth+1)")
# Extract MCMC medians for p_male[,1]
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, paste0("Nc[",1:data$Nyears,", 1]")]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$Nc[,1]
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:length(years), predict(loess_fit_1), col = "tomato", lwd = 2)

caterplot(fit,paste0("Nc[",1:length(years),", 2]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = years) 
title("2+ smolts numbers by cohort (year of birth+1)")
# Extract MCMC medians for p_male[,1]
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, paste0("Nc[",1:data$Nyears,", 2]")]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$Nc[,1]
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:length(years), predict(loess_fit_1), col = "tomato", lwd = 2)



par(mfrow=c(1,1))
caterplot(fit,paste0("p1c[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = years)
title("Proportion of 1 year old smolts by cohort")
# Extract MCMC medians for p_male[,1]
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, paste0("p1c[",1:data$Nyears,"]")]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$p1c
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:data$Nyears, predict(loess_fit_1), col = "tomato", lwd = 2)


par(mfrow=c(1,1))
caterplot(fit,paste0("p1y[",1:data$Nyears,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = years)
title("Proportion of 1 year old smolts by year")
# Extract MCMC medians for p_male[,1]
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, paste0("p1y[",1:data$Nyears,"]")]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$p1y
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:data$Nyears, predict(loess_fit_1), col = "tomato", lwd = 2) 


par(mfrow=c(2,1))
caterplot(fit,paste0("p_male[",1:data$Nyears,", 1]"), reorder = FALSE, horizontal=FALSE, style=c("plain"),labels = years);
title("Prop Male smolts 1+")
abline(h=0.5,lty=2)
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, paste0("p_male[",1:data$Nyears,", 1]")]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$p_male[,1]
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:data$Nyears, predict(loess_fit_1), col = "tomato", lwd = 2) 

caterplot(fit,paste0("p_male[",1:data$Nyears,", 2]"), reorder = FALSE, horizontal=FALSE, style=c("plain"),labels = years);
title("Prop Male smolts 2+")
abline(h=0.5,lty=2)
if(nimble){
  # Combine both chains into one matrix
  all_samples <- rbind(as.matrix(fit$chain1), as.matrix(fit$chain2))
  # Select only columns corresponding to "N[...]" using grep
  N_samples <- all_samples[, paste0("p_male[",1:data$Nyears,", 2]")]
  # Compute medians for each N[i]
  medians <- apply(N_samples, 2, median)
} else {
  medians <- fit$median$p_male[,2]
}
# Fit LOESS model and add smoothing line
loess_fit_1 <- loess(medians ~ years, span = 0.5)  
lines(1:data$Nyears, predict(loess_fit_1), col = "tomato", lwd = 2) 


par(mfrow=c(2,1))
caterplot(fit,paste0("p_smp[",1:data$Nyears,", 1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("p_smp[",1:data$Nyears,", 2]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 

dev.off()

