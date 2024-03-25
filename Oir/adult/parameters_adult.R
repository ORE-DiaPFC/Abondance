parameters <-c(
  ## HYPER PARAMETERS 
"logit_int_MC" # intercept
,"logit_flow_MC" # slope of flow effect
,"sigmap_eff" # standard deviation of trap efficiency 
  
,"shape_lambda" # Shape parameter of gamma distribution
,"rate_lambda" # Rate parameter of gamma distribution
,"s" # Dirichlet parameter
  
,"Plambda" # Proportion distribution
,"Plambda0" # Initiate proportion distribution
  
,"mup_recap"  # Mean of the probabilities to be re-captured
,"sigmap_recap" # Standard deviation of the probabilities to be re-captured 
  
  ## PROBABILITIES
,"pi_MC" # Probability of capture
,"p_MC90" # decrease in probability of capture in 1990
,"epsilon_MC" # standardized residuals of probability of capture
,"p_MC90_1SW" # Global probability of capture in 1990 for 1SW
,"p_MC90_MSW" # Global probability of capture in 1990 for MSW
  
,"p_recap" # Probability of recapture
  
  ## POPULATION
,"n_tot" # Total number of adults
,"n_1SW" # Total number of 1SW
,"n_MSW"  # Total number of MSW
,"n" # Total number of adults per breeding category
,"sex_ratio_1SW","sex_ratio_MSW"
,"pmale_1SW","pmale_MSW"
  
,"lambda0" # Initiate lambda distribution
,"lambda_n" # Poisson parameter per breeding category
,"lambda" # Poisson parameter per year
  
,"Nesc" # Escapement per breeding category
,"Nesc_1SW" # 1SW escapement
,"Nesc_MSW" # MSW escapement
,"Nesc_tot" # Total escapement
  
  # STATISTICS
  ## 1: is logit_flow >=0 for 1SW?
  ## 2: is logit_flow >=0 for MSW?  
  ## 3: is mean probability of recapture male 1SW >=0 female 1SW
  ## 4: is mean probability of recapture male 1SW >=0 male MSW
  ## 5: is mean probability of recapture male 1SW >=0 female MSW
  ## 6: is mean probability of recapture female 1SW >=0 male MSW
  ## 7: is mean probability of recapture female 1SW >=0 female MSW
  ## 8: is mean probability of recapture male MSW >=0 female MSW
,"test"
)