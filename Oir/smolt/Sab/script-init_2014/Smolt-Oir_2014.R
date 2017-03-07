################################################################################
###           Model of CMR data to estimate smolt population size         ###
###                 of Salmo salar in Oir river.                          ###
###                  Sabrina Servanty & Etienne Prévost                   ###
###                          April 2015                                   ###
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

###############             Hyperparameters for Ntot       ##################
# Shape and rate parameter for gamma distribution  for negative binomial (see Gelman, 2d edition, p446)
shape_lambda ~ dgamma(0.001,0.001)
rate_lambda ~ dgamma(0.001,0.001)

mean_gamma <- shape_lambda/rate_lambda
var_gamma <- shape_lambda/(rate_lambda*rate_lambda)

lambda.pred ~ dgamma(shape_lambda,rate_lambda)
Ntot.pred ~ dpois(lambda.pred)

# Nyears = 29 years : from 1986 to 2014 (migration year)	
for (t in 1:Nyears) {
  
################              Prior for Ntot[t], i=1 to Nyears         ######################
  # Hierarchical under negative binomiale		
  lambda[t] ~ dgamma(shape_lambda,rate_lambda) 
  Ntot[t] ~ dpois(lambda[t]) 	
		
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
   
####################                 LIKELIHOOD               #######################  		
	# Binomial for Recaptures 
  Cm_R[t] ~ dbin(p_MC[t],Cm_MC[t]) 

	# Binomial for Captures 
	C_MC[t] ~ dbin(p_MC[t],Ntot[t]) 
 
  Nesc[t] <- Ntot[t] - D_MC[t]  # number of smolt escaping the river
  
  } # end of the loop on years

} # end of the model


