parameters <-c(
  ##### INTERCALIBRATION
  "int_width" # intercept of linear relationship between CPUE and density 
  ,"width_coef" # proportional coefficient with width
  ,"rate_lcpu" # inverse scale of gamma distribution
  ,"p_cpue"
  ,"mup_rem"
  ,"sd_prem"
  ,"eps"
  ,"mu_d"
  ,"sigma_d"
  #,"rate_d"
  ,"log_k_inter"
  
  ######### ABUNDANCE INDEX SCORFF
  ## HYPER PARAMETERS
  ,"sigma_dSc" # overall standard deviation in density
  
  # Year effect (log scale
  ,"int_ydSc" # intercept (mean 
  ,"log_flow" # slope of flow effect
  ,"sigma_ySc" # standard deviation
  
  # Site effect (log scale
  ,"sigma_siteSc" # standard deviation
  
  , "eps_ydSc" 
  
  ####################
  # DENSITY
  ,"year_dSc" # annual year effect in density (log scale
  ,"site_Sc" # site effect in density (log scale
  
  # Annual density per site and year corrected by observed flow when habitat sampling was done
  ,"d_Sc_pred"  
  
  ## POPULATION
  ,"ntot_Sc" # annual abundance
  
  # STATISTIC
  ,"test" #is log_flow >=0?
)