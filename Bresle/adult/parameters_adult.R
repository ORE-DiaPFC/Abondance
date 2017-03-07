parameters <-c(
  ## PROBABILITIES 
  "pi_Eu00" # decrease in probability of capture in Eu in 2000
  ,"pi_Eu01" # decrease in probability of capture in Eu in 2001
  ,"logit_int_Eu" # intercept
  ,"logit_flow_Eu" # slope of flow effect
  ,"lflow_fall_Eu" # slope of flow effect in fall
  ,"sigmapi_Eu" # standard deviation in probability of capture in Eu
  ,"epsilon_Eu" # standardized residuals of probability of capture in Eu
  
  ,"mupi_B" # mean probability of capture in Beauchamps
  ,"sigmapi_B" # standard deviation in probability of capture in Beauchamps
  
  ,"pi_Eu" # Probability of capture in Eu
  ,"p_Eu00_tot" # Global probability of capture in Eu in 2000
  ,"p_Eu01_tot" # Global probability of capture in Eu in 2001
  ,"pi_B" # Probability of capture in Beauchamps
  
  # STATISTICS
  ## 1: is logit_flow >=0 for 1SW?
  ## 2: is logit_flow >=0 for MSW?  
  ## 3: is logit_flow_fall >=0 for 1SW?
  ## 4: is logit_flow_fall >=0 for 1SW?
  ## 5: is difference in slope >=0 for 1SW? (1SW>MSW
  ## 6: is difference in slope in fall >=0 for 1SW? (1SW>MSW
  ,"test"
  ,"R2" # Percentage of variation explained by flow (spring + fall 1: 1SW, 2: MSW
  
  ## POPULATION
  ,"n_tot" # Total number of adults
  ,"n_1SW" # Total number of 1SW
  ,"n_MSW" # Total number of MSW
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,"lambda_tot0" # Initiate lambda distribution
  ,"Plambda0" # Initiate proportion distribution
  
  ,"s" # Dirichlet parameter
  
  ,"lambda_tot" # Poisson parameter
  ,"Plambda" # Proportion distribution
)
