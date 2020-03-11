parameters <-c(
  
  ## PROBABILITIES 
"logit_int_MC" # intercept 
,"logit_flow_MC" #slope for flow data (April
,"log_cess_MC" #slope for capture effort (number of marking sessions
  
  
  # POPULATION
,"shape_lambda" # Shape parameter of gamma distribution
,"rate_lambda" # Rate parameter of gamma distribution
,"mean_gamma" # Mean parameter of gamma distribution
,"var_gamma" # Variance parameter of gamma distribution
  
,"lambda" # Poisson parameter
,"Ntot" # Number of smolt 
,"Nesc" # Number of smolt escaping the river (Ntot-Dead

,"eps_pi_MC"
,"overdisp_MC" # Annual overdispersion of capture probability
,"mean_MC" # Mean capture probability
,"p_MC" # Annual capture of probability
,"alpha_MC" # Alpha parameter of the beta binomiale distribution
,"beta_MC" # Beta parameter of the beta binomiale distribution
  
  ### STATISTIC
  # 1: is log_cess_MC >=0 ?
  # 2: is logit_flow >=0 ?
,"test"
)