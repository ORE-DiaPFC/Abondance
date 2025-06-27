

fit.mcmc <- as.mcmc.list(fit) # using bugs

# DIAGNOSTICS:
parameterstotest <- c(
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
  ,"shape_theta","mean_theta","sigma_theta"
  
  # ,paste0("lambda[",1:data$Nyears,"]") # Poisson parameter
  #,"lambda"
  ,"theta"
  
  #,paste0("p1c[",1:data$Nyears,"]") # Poisson parameter
  ,"p1c"
  
  #  ,"Nesc" # Number of smolt escaping the river (Ntot-Dead
  
  #### TEST
  # 1: is logit_flow at MP >=0 ?
  # 2: is logit_flow at ML >=0 ?
  #  ,"test" 
  # ,"R2"
  
) 
# all parameters
# parameterstotest <- c(
#   "epsilon_p"
# )

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
