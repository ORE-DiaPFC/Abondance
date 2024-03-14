################################################################################
###           Model of CMR data to estimate smolt population size            ###
###                 of Salmo salar in Oir river.                             ###
###                  Sabrina Servanty & Etienne Pr?vost                      ###
###                          April 2015                                      ###
###            Modified by Mathieu Buoro and Etienne Prévost                 ###
###                    in 2024 for introduction of age                       ###
################################################################################

######################################################################
## Considering a beta-binomial for the probability of capture
## Adding a proportional relationship between the number of marking sessions (standardized within the model) and the overdispersion
## Adding a flow effect in the mean probability of capture althoug it is not signigicant (standardized within the model) 
#####################################################################

#######################################################################
######################################################################
#  DATA:
# Nyears: Length of time series
# C_MC[t]: Annual number of smolt captured at the trap (Moulin Cerisel)
# Cm_MC[t]: Annual number of smolt marked and released upstream from the trap (Moulin Cerisel) 
# Cm_R[t]: Annual number of marked smolt recaptured at the trap
# D_MC[t]: Annual number of dead smolt (marked or unmarked, death at the trap)
########################################################################
# NOTATION:
# p_MC[i]: annual trap efficiency for capturing smolt (probability to be trapped)
# lambda[i]: annual mean smolt population size (mean of Poisson distribution)
# Ntot[i]: annual total smolt population size 
############################################################################
#############################################################################

model {

###############          Hyperprior for the trapping efficiency          ################### 
### Mean and standard deviation of trap efficiency
log_cess_MC ~ dunif(-10,10) #slope for capture effort (number of marking sessions)

logit_int_MC ~ dunif(-10,10)    #intercept 
logit_flow_MC ~ dunif(-10,10) #slope for flow data (April)

test[1] <- step(log_cess_MC) # is log_cess_MC >=0 ?
test[2] <- step(logit_flow_MC) # is logit_flow >=0 ?

###############        Hyperparameters for N (smolt number by cohort)       ##################
# Shape and rate parameter for gamma distribution  for negative binomial (see Gelman, 2d edition, p446)
shape_lambda ~ dgamma(0.001,0.001)
rate_lambda ~ dgamma(0.001,0.001)

mean_gamma <- shape_lambda/rate_lambda
var_gamma <- shape_lambda/(rate_lambda*rate_lambda)

lambda.pred ~ dgamma(shape_lambda,rate_lambda)
N.pred ~ dpois(lambda.pred)

############################################################################################
###############     Hyperparameters for p1c (proportion of 1 year old smolt by cohort)
############################################################################################
# Prior pour que s1 et s2 soient superieurs a 1 et que le prior sur p1c soit faiblement informatif
l1 ~ dbeta(1,2); l2 ~ dbeta(1,2)
s1 <- 1+(l1*100); s2 <- 1+(l2*100)

# Modelling the smolts numbers by cohort (year of birth+1) and by age class (1 and 2)
# Initialisation pour la cohorte 0 (1994)
  lambda0 ~ dgamma(shape_lambda,rate_lambda) 
  N0 ~ dpois(lambda0)
  N01c ~ dbin(p10c, N0)
  N02c <- N0-N01c 	
  p10c ~ dbeta(s1, s2)
# Calculating the smolt numbers by year of migration
  Ntot[1] <- Nc[1,1]+N02c
for (t in 1:Nyears) {    
################              Prior for N[t], i=1 to Nyears         ######################
  # Hierarchical under negative binomiale		
  lambda[t] ~ dgamma(shape_lambda,rate_lambda) 
  N[t] ~ dpois(lambda[t])
# Distributing smolt into age classes
  Nc[t,1] ~ dbin(p1c[t], N[t])
  Nc[t,2] <- N[t]-Nc[t,1] 	
# Hierarchcal modelling of the proportion of 1 year old smoltsby cohort
  p1c[t] ~ dbeta(s1, s2)
    } # end of loop over years
# Calculating the smolt numbers by year of migration
for (t in 1:Nyears-1) {  Ntot[t+1] <- Nc[t+1,1]+Nc[t,2] }

# 1985: 1st year
#############	        Prior for p_MC[t]          #################
  ### Overdispersion
  logeff_MC[1] <- log(eff_MC[1]) # ln transformation of covariate of the number of marking sessions
    
  log_disp[1] <- logeff_MC[1] + log_cess_MC  # proportional relationship between number of marking sessions and overdispersion
  overdisp_MC[1] <- exp(log_disp[1])
  
  ### Mean
  logQ_MC[1] <- log(Q_MC[1]) # ln transformation of covariate flow
  stlogQ_MC[1] <- (logQ_MC[1] - mean(logQ_MC[]))/sd(logQ_MC[]) # standardized covariate
  
  lmupi_MC[1] <- logit_int_MC + logit_flow_MC * stlogQ_MC[1]
  mean_MC[1] <- exp(lmupi_MC[1])/(1+exp(lmupi_MC[1]))  # back-transformation on the probability scale
  
  ### Beta-binomiale 
  alpha_MC[1] <- mean_MC[1] * overdisp_MC[1]
  beta_MC[1] <- (1-mean_MC[1]) * overdisp_MC[1]
  p_MC[1] ~ dbeta(alpha_MC[1],beta_MC[1]) 
  
  eps_p_MC[1] <- logit(p_MC[1]) - lmupi_MC[1]  # r?siduals logit scale (March 2020)

####################                 LIKELIHOOD               #######################  		
	# Binomial for Recaptures 
  Cm_R[1] ~ dbin(p_MC[1],Cm_MC[1]) 

	# Binomial for Captures 
	C_MC[1] ~ dbin(p_MC[1],Ntot[1]) 
 
  Nesc[1] <- Ntot[1] - D_MC[1]  # number of smolt escaping the river
  
# Modelling of the age composition data by year of migration
  n1[1] ~ dbin(p1y[1], n[1])
  # Calculating the proportions of 1 year old smolts by year of migration from the proportions by cohort
  p1y[1] <- p1c[1]*N[1]/(p1c[1]*N[1]+(1-p10c)*N0)

# Nyears = 29 years : from 1986 to 2014 (migration year)	
for (t in 2:Nyears) {
		
#############	        Prior for p_MC[t]          #################
  ### Overdispersion
  logeff_MC[t] <- log(eff_MC[t]) # ln transformation of covariate of the number of marking sessions
    
  log_disp[t] <- logeff_MC[t] + log_cess_MC  # proportional relationship between number of marking sessions and overdispersion
  overdisp_MC[t] <- exp(log_disp[t])
  
  ### Mean
  logQ_MC[t] <- log(Q_MC[t]) # ln transformation of covariate flow
  stlogQ_MC[t] <- (logQ_MC[t] - mean(logQ_MC[]))/sd(logQ_MC[]) # standardized covariate
  
  lmupi_MC[t] <- logit_int_MC + logit_flow_MC * stlogQ_MC[t]
  mean_MC[t] <- exp(lmupi_MC[t])/(1+exp(lmupi_MC[t]))  # back-transformation on the probability scale
  
  ### Beta-binomiale 
  alpha_MC[t] <- mean_MC[t] * overdisp_MC[t]
  beta_MC[t] <- (1-mean_MC[t]) * overdisp_MC[t]
  p_MC[t] ~ dbeta(alpha_MC[t],beta_MC[t]) 
  
  eps_p_MC[t] <- logit(p_MC[t]) - lmupi_MC[t]  # r?siduals logit scale (March 2020)
####################                 LIKELIHOOD               #######################  		
	# Binomial for Recaptures 
  Cm_R[t] ~ dbin(p_MC[t],Cm_MC[t]) 

	# Binomial for Captures 
	C_MC[t] ~ dbin(p_MC[t],Ntot[t]) 
 
  Nesc[t] <- Ntot[t] - D_MC[t]  # number of smolt escaping the river
  
  # Modelling of the age composition data by year of migration
  n1[t] ~ dbin(p1y[t], n[t])
# Calculating the proportions of 1 year old smolts by year of migration from the proportions by cohort
  p1y[t] <- p1c[t]*N[t]/(p1c[t]*N[t]+(1-p1c[t-1])*N[t-1])

  } # end of the loop on years

} # end of the model


