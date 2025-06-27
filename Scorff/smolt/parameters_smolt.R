parameters <-c(
  
  ## HYPER PARAMETERS 
  "logit_int" # intercept (mean) probability of capture (1: MP; 2: ML)
  ,"logit_flow" # slope for flow effect in probability of capture
  ,"sigmap" # standard deviation of probability of capture
  ,"rho" # correlation coefficient between probability of capture at the two traps
  
  #,"l_ML_dim" # mean decrease in probability of capture when Lesl? is not working
  # ,"sigmap_ML_dim" # standard deviation of decrease in probability of capture when Lesl? is not working
  # ,"junk" # variance of decrease in probability of capture when Lesl? is not working
  
  ## PROBABILITIES
  ,"p" # probability of capture at the 2 traps (1: MP; 2: ML 
  ,"epsilon" # standardized residuals of probability of capture
  
  #,"pi_ML_dim" # decrease in the probability of capture at Lesl?
  #,"epsML_dim" # standardized residuals of the decrease in the probability of capture
  
  ,"p_MP" # capture probability at Moulin des Princes
  ,"p_ML" # capture probability at Moulin de Lesl?
  
  ,"p10c"
  #,"lambda0"
  ,"N0"
  
  #,"n1","n"
  
  ## POPULATION
  ,"Ntot" # annual number of smolt
  , "N" #  number of smolt by cohort
  ,"Nc" #  number of smolt by age
  ,"p1c" # proportion smolt 1+ / cohort
  ,"p1y" # proportion smolt 1+ / year
  ,"s1","s2"
  ,"l1","l2"
  #,"mu_p1c","alpha"
  
  #,"shape_lambda" # Shape parameter of gamma distribution
  #,"rate_lambda" # Rate parameter of gamma distribution
  #,"mean_gamma" # Mean parameter of gamma distribution
  #,"var_gamma" # Variance parameter of gamma distribution
  #,"prob0"
  ,"theta0"
  ,"shape_theta","mean_theta","sigma_theta"
  ,"theta"
  
  #,"lambda" # Poisson parameter
  
  ,"Nesc" # Number of smolt escaping the river (Ntot-Dead
  
  
  ,"p_male"
  ,"p_smp"
  ,"ll"
  
  #### TEST
  # 1: is logit_flow at MP >=0 ?
  # 2: is logit_flow at ML >=0 ?
  ,"test" 
  # ,"R2"
) # percentage of variation explained by flow
