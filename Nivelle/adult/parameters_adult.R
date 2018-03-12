parameters <-c(
  ## PROBABILITIES 
  # Mean and standard deviation of the probability to stay in LN1
  "mup_11_1" # from 1984 to 1991
  ,"sigmap_11_1" 
  ,"mup_11_2" # from 1992 to now on
  ,"sigmap_11_2"
  
  # Mean and standard deviation of the probabilities to be captured at Uxondoa
  ,"mupi_U"
  ,"sigmapi_U"
  
  # standard deviation of the probabilities to be captured at Ohla
  ,"sigmapi_Ol"
  ,"eps_Ol" # standardized residuals in probability to be captured at Ohla
  
  # Mean and standard deviation of the probabilities to be Re-captured by EF, angling or found dead
  ,"mupi_EF"
  ,"sigmapi_EF"
  
  # Mean and standard deviation of the probabilities to move from LN2
  ,"mup_n12"
  ,"sigmap_12"
  
  #Mean and standard deviation of the probabilities to stay in UN
  ,"mup_21"
  ,"sigmap_21"
  
  ,"p_11_1" #annual probability to stay in LN1 from 1984 to 1991
  ,"p_11_2" # annual probability to stay in LN1 since 1992
  
  ,"pi_U" # annual probability to be captured at Uxondoa since 1984 given sea age (1:1SW vs 2:MSW
  ,"eps_U" # standardized residuals of probability of capture at Uxondoa
  ,"pi_U_eff" # decreased probability of capture at Uxondoda since 2012
  
  ,"pi_EF" # Probabilities to be Re-captured by EF, angling or found dead
  
  ,"p_n12" # Probabilities to move from LN2 
  ,"eps_12" # standardized residuals in probability of moving from LN2
  
  ,"p_21" # Probabilities to stay in UN  
  
  ,"pi_Ol" # annual probability to be captured at Ohla since 1984 given sea age (1:1SW vs 2:MSW
  
  
  ### TESTS for probability of moving from LN2
  # 1: Male 1SW vs Female 1SW
  # 2: Male 1SW vs Male MSW
  # 3: Male 1SW vs Female MSW
  # 4: Female 1SW vs Male MSW
  # 5: Female 1SW vs Female MSW
  # 6: Male MSW vs Female MSW
  ,"test_p_12" # is there a difference in the mean probability
  
  
  ### REDDS
  ,"k_1" # Proportionality coeffcient for lower Nivelle
  ,"k_2" # Proportionality coeffcient for high catchment zone
  
  ,"RPF" # annual mean redd number per female per zone
  
  ,"alpha_1" # annual effect for Lower Nivelle
  ,"eta_1" # shape and scale paramater for year effect in zone LN
  ,"alpha_2" # annual effect for High Catchement zone (since 1990
  ,"eta_2" # shape and scale paramater for year effect in high catchement zone
  
  ,"rho" # river flow effect (December
  
  ## POPULATION
  ,"lambda_tot" # Poisson parameter
  ,"Plambda" # Proportion distribution
  
  ,"shape_lambda"# Shape parameter of gamma distribution
  ,"rate_lambda" # Rate parameter of gamma distribution
  ,"lambda_tot0"# Initiate lambda distribution 
  ,"Plambda0" # Initiation proportion distribution
  
  , "s" # Dirichlet parameter
  
  ,"n_tot" # Total number of adults entering in the river
  ,"n_1SW" # Total number of 1SW
  ,"n_MSW" # Total number of MSW
  
  ,"n_11" # annual number of fish per breeding category staying in LN1
  
  ,"e_11" # annual escapement for LN1 per breeding category
  ,"e_11_tot" # annual escapement for LN1
  
  ,"e_12" # annual escapement for LN2 per breeding category
  ,"e_12_tot" # annual escapement for LN2
  
  ,"e_21" # annual escapement for UN per breeding category
  ,"e_21_tot" # annual escapement for UN
  
  ,"e_22" # annual escapement for LUR per breeding category
  ,"e_22_tot" # annual escapement for LUR 
  
  ,"e_1SW" # annual escapement for male and female 1SW
  ,"e_MSW" # annual escapement for male and female MSW
  ,"e_1SW_F" # annual escapement for female 1SW
  ,"e_MSW_F" # annual escapement for female MSW
  
  ,"eggs_11" # annual number of eggs produced between Ascain and Uxondoa (LN1
  ,"eggs_12" # annual number of eggs produced between Uxondoa and Olha (LN2
  ,"eggs_21" # annual number of eggs produced in Upper Nivelle
  ,"eggs_22" # annual number of eggs produced in Lurgorrieta
  ,"eggs_tot" # annual number of eggs produced
  
  # Effectif par cohorte et param?tres associ?s
  ,"a_1.1SW" # parameter of the beta distribution for the probability to sample a 1R/1SW individual
  ,"a_2.1SW" # second parameter of the beta distribution for the probability to sample a 2R/1SW individual
  ,"a_MSW" # dirichlet distribution for the probability to sample a MSW individual
  ,"c_1SW" # number of 1SW (either 1R or 2R
  ,"c_2SW" # annual number of 2SW (either 1R or 2R
  ,"c_3SW" # annual number of 3SW (either 1R or 2R
  ,"c_tot" # annual number per cohorte
  ,"P_1SW" # annual proportion of 1SW
  ,"P_MSW" # annual proportion of MSW
)
