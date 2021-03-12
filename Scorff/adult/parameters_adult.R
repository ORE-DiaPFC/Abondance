parameters <-c(
  ## HYPER PARAMETERS 
  "logit_int_MP" # intercept (mean probability of capture at MP
  ,"logit_flow_MP" # slope for flow data in probability of capture at MP
  ,"sigmapi_MP" # standard deviation of probability of capture at Moulin des Princes
 
  ,"mupi_F" # mean probability of dying from fishing
  ,"sigmapi_F" # standard deviation of probability of dying from fishing
  ,"rho_F" # correlation coefficient between probability of dying from fishing between marked and unmarked depending on sea age

  
  ,"logit_int_R" # intercept (mean probability of recapture during or after reproduction
  ,"logit_effort_R" # slope for effort (number of nights in probability of recapture during reproduction
  ,"logit_flow_R" # slope for flow data in probability of recapture during reproduction
  ,"sigmapi_R" # standard deviation of probabilty of recapture during reproduction
  
  ,"pi_R_pulsium" # probability recapture during reproduction using electric fishing (pulsium)
  
  ,"mupi_D" # mean probability to die from natural cause
  ,"sigmapi_D" # standard deviation of probability of dying from natural cause 
  ,"rho_D" # correlation coefficient between probability of dying between marked and unmarked depending on sea age
  
  ,"mupi_oF" # mean probability of recovering a caught fish (from fishing
  ,"sigmapi_oF" # standard deviation of probability of recovering a caught fish (from fishing
  
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
  ,"test"
  
  ,"diffF_1SW" # difference in mean in mortality due to fishing between marked and unmarked 1SW 
  ,"diffF_MSW" # difference in mean in mortality due to fishing between marked and unmarked MSW
  ,"diff1SW" # difference in mean in mortality due to natural cause between marked and unmarked 1SW 
  ,"diffMSW" # difference in mean in mortality due to natural cause between marked and unmarked MSW 
  
  ## PROBABILITIES
  ,"pi_MP" # probability of capture at Moulin des Princes
  ,"pi_MP94" # decrease in probability of capture in 1994
  ,"p_MP94_tot" # total probability of capture in 1994
  ,"pi_MP20" # decrease in probability of capture in 2020 COVID
  ,"p_MP20_tot" # total probability of capture in 2020 COVID
  ,"epsilon_MP" # standardized residuals of probability of capture at MP
  
  ,"pi_oF" # probability to recover a caught fish from fishing (from 1994 to 2002
  ,"piF_1SW" # probability of dying from fishing for 1SW depending on being marked or not
  ,"piF_MSW" # probability of dying from fishing for MSW depending on being marked or not
  
  ,"pi_oD" # probability to recover a dead fish
  ,"piD_1SW" # probability of dying from natural cause for 1SW depending on being marked or not
  ,"piD_MSW" # probability of dying from natural cause for MSW depending on being marked or not
  
  ,"pi_R" # probability of recapture during or after reproduction
  ,"epsilon_R" # standardized residuals of probability of recapture during or after reproduction
  
  
  ## POPULATION
  ,"n_tot" # total annual number of adults entering the river
  ,"n_1SW" # annual number of 1SW
  ,"n_MSW" # annual number of MSW
  
  ,"shape_lambda" # Shape parameter of gamma distribution
  ,"rate_lambda"  # Rate parameter of gamma distribution
  
  ,"lambda_tot0"  # Initiate lambda distribution
  ,"Plambda0" # Initiate proportion distribution
  ,"lambda_tot" # Poisson parameter
  ,"Plambda" # Proportion distribution
  
  ,"s" # Dirichlet parameter 
  
  ,"e_tot" # total annual escapement
  ,"e_1SW" # annual escapement of 1SW
  ,"e_MSW" # annual escapement of MSW
  
  ,"nv_m" ## Fish susceptible to die from other cause than fishing per sea age / marked
  ,"nv_um" ## Fish susceptible to die from other cause than fishing per sea age / unmarked
  ,"Cmuo_F"
  ,"m_D"
  ,"um_D"
  ,"pi_muoF"
)