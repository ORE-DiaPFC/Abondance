parameters <-c(
  ######### SUCCESSIVE REMOVAL AND DENSITY
  ## HYPER PARAMETERS 
"sd_prem" # sd of probability of capture during successive removals
,"mup_rem" # mean of probability of capture during successive removals (probability scale
  
,"int_width" # constant linked to the relationship with width
,"width_coef" # factor of proportionality with river's width
,"rate_lcpu"  # rate (inverse scale of the gamma distribution for lambda_cpu
  
,"sigma_dOir" # Overall standard deviation in density in Oir river
,"mu_ydOir" # Mean annual density
,"sigma_yOir" # Standard deviation of year effect in density
,"year_dOir" # year effect in density 
,"sigma_gryrOir" # Standard deviation of interaction between group and year
  
  ## STATISTIC
  # 1: mean density in Oir river >=0 mean density in La Roche
  # 2: mean density in Oir river >=0 mean density in Pont Levesque
  # 3: mean density in Oir river <=0 mean density in Moulin du Bois
  # 4: mean density in La Roche >=0 mean density in Pont Levesque
  # 5: mean density in La Roche >=0 mean density in Moulin du Bois
  # 6: mean density in La Roche >=0 mean density in Moulin du Bois
,"test"
  
,"k_cpuOir" # proportional relationship with width
  
,"coef_PC" # coefficient of proportionality for run.
  
  ## Probability of capture
  #samplesSet(p_remgr # Probability of capture during successive removal per group and year 
  
  ### DENSITY
,"dRR_Oir" # local density in equivalent rapid per 100m?
,"dIA_Oir" # local density per 100m? observed during CPUE 
  
  ## POPULATION
,"lambda_ynOir_gr" # mean annual abundance per group in Oir river
,"lambda_ynMB" # mean annual abundance per group in Moulin du Bois
,"lambda_ynPL" # mean annual abundance per group in Pont Levesque
,"lambda_ynLR" # mean annual abundance per group in La Roche
,"n_Oir_gr"  # annual abundance per group in Oir river 
,"n_Oir"  # annual abundance in Oir river 
,"n_MB" # annual abundance per group in Moulin du Bois
,"n_PL" # annual abundance per group in Pont Levesque
,"n_LR" # annual abundance per group in La Roche
,"ntot_Oir" # total annual abundance (Oir + tributary rivers
)