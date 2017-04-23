parameters <- c(
## HYPERPARAMETRES
#############

## Successive removal
"mu_p_srem" # Mean capture probability by successive removal
,"sd_logit_p_srem" # Standard deviation of the logit transformed capture probability
,"epsilon_p" # decrease of efficiency between successive passes

## Density
,"beta_dj" # habitat effect 
,"alpha_dj" # Year effect
,"gamma_dj" # Interaction year X zone
,"pi_dj" # Density dependence effect when doing restocking
,"xi_dj" # Zone effect in density dependence when doing restocking
,"eta_dj" # Inverse scale of a Gamma distribution for density per zone
,"zeta_alpha_dj" # Shape and inverse scale of a Gamma distribution for year effect
,"zeta_gamma_dj" # Shape parameter of a Gamma distribution for interaction year X zone
,"eta_gamma_dj" # Inverse scale parameter of a Gamma distribution for interaction year X zone
,"mu_dj_nat" # Mean density issued from reproduction
,"delta" # Juvenile survival depending on stage
,"k_cpue" # Coefficient of proportionality between mean CPUE and density
,"eta_cpue" # Inverse scale parameter of gamma distribution for mean CPUE
,"rho_s"  # nonlinear relationshihp between flow and surface
,"sd_s_rec" # Standard deviation of the recent surface of stations
,"p_cpue" # Non informative prior used to parameterize mean CPUE 

## POPULATION
#############

## Successive removal
,"n1" # Total number of juveniles that could be captured by successive removal
,"n1_nat" # Number of juveniles issued from natural reproduction that could be captured by successive removal
,"n1_comp" # Number of juveniles issued from compensation or restocking that could be captured by successive removal


,"jnat_ns_riff" #number of juveniles issued from natural recruitment in riffle per year and per zone
,"jnat_ns_runs" #number of juveniles issued from natural recruitment in runs per year and per zone
,"jcomp_ns_riff" #number of juveniles produced by released individuals when doing compensation in riffle per year and per zone
,"jcomp_ns_runs" #number of juveniles produced by released individuals when doing compensation in runs per year and per zone
,"jres_ns_riff" # total number of juveniles produced by released individuals when doing restocking in riffle per year and per zone
,"jres_ns_runs" # total number of juveniles produced by released individuals when doing restocking in runs per year and per zone

## DENSITY
###########

,"dj" # density per station

# When compensation or restocking is occuring
,"dj_nat" # density per station issued from natural reproduction
,"dj_comp"  # density per station issued from compensation
)
