

fit.mcmc <- as.mcmc(fit) # using bugs

# DIAGNOSTICS:
parameterstotest <-c(
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


# Start writing to an output file
sink(paste('results/Diagnostics_',stade,"_",year,'.txt',sep=""))

cat("=============================\n")
cat("DIAGNOSTICS\n")
cat("=============================\n")

cat("Number of chains: ", fit$n.chains,"\n")
cat("Number of iterations: ", fit$n.keep,"\n")

if (nChains > 1) {
  cat("Convergence: gelman-Rubin R test\n")
  #gelman.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)],multivariate=TRUE)
  gelman.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)],multivariate=TRUE)
  #test <- gelman.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)],multivariate=TRUE)
  
}
cat("Approximate convergence is diagnosed when the upper limit is close to 1 and <1.1 \n")
#test

cat("\n---------------------------\n")
cat("Heidelberger and Welch's convergence diagnostic\n")
cat("
heidel.diag is a run length control diagnostic based on a criterion of relative accuracy for the estimate of the mean. The default setting corresponds to a relative accuracy of two significant digits.

heidel.diag also implements a convergence diagnostic, and removes up to half the chain in order to ensure that the means are estimated from a chain that has converged.
\n")
#heidel.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], eps=0.1, pvalue=0.05)
heidel.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], eps=0.1, pvalue=0.05)

cat("\n---------------------------\n")
cat("Geweke's convergence diagnostic\n")
cat("
Geweke (1992) proposed a convergence diagnostic for Markov chains based on a test for equality of the means of the first and last part of a Markov chain (by default the first 10% and the last 50%).
If the samples are drawn from the stationary distribution of the chain, the two means are equal and Geweke's statistic has an asymptotically standard normal distribution. 
The test statistic is a standard Z-score: the difference between the two sample means divided by its estimated standard error. The standard error is estimated from the spectral density at zero and so takes into account any autocorrelation.

The Z-score is calculated under the assumption that the two parts of the chain are asymptotically independent, which requires that the sum of frac1 and frac2 be strictly less than 1.
\n")
#geweke.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], frac1 = 0.1, frac2 = 0.5)
geweke.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], frac1 = 0.1, frac2 = 0.5)

cat("\n---------------------------\n")
cat("Raftery and Lewis's diagnostic\n")
raftery.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], q=0.025, r=0.005, s=0.95, converge.eps=0.001)

# Stop writing to the file
sink()
