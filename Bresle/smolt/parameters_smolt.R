parameters <-c(
  ## HYPER PARAMETERS 
 # "mu_B" # mean probability of capture in Beauchamps
  #"sigmap_B" # standard deviation in probability of capture in Beauchamps
  
  "logit_int_B"
  ,"logit_flow_B"
  ,"sigmap_B"
  ,"exp_d_p_B"
  
  ,"logit_int_Eu" # intercept in probability of capture in Eu
  ,"logit_flow_Eu" # slope effect of flow in probability of capture in Eu
  ,"sigmap_Eu" # # standard deviation in probability of capture in Eu
  ,"exp_d_p_Eu"
  
  ## PROBABILITIES
  ,"p_B" # probabilities of capture in Beauchamps (without the decrease
  ,"p_Btot" # global probabilities of capture in Beauchamps
  ,"epsilon_B" # standardized residuals in probability of capture in Beauchamps
  
  ,"p_Eutot"
  ,"p_Eu" # probabilites of capture in Eu
  ,"epsilon_Eu" # standardized residuals in probability of capture in Eu
  
  
  ## POPULATION
  ,"Ntot" # Number of smolt 
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,"mean_gamma" # Mean parameter of gamma distribution
  ,"var_gamma" # Variance parameter of gamma distribution
  
  ,"lambda" # Poisson parameter
  
  ,"Nesc" # Number of smolt escaping the river (Ntot-Dead)
  
  #### TEST
  
  ,"test_Eu" # is logit_flow >=0 ?
  ,"test_B" # is logit_flow >=0 ?
  #,"R2" # Percentage of variation explained by flow  
  ,"p_keep" # probability to keep the blue tag on year 2018
)