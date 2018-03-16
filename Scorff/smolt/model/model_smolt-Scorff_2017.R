################################################################################
###           Model of CMR data to estimate smolt population size         ###
###                 of Salmo salar in Scorff river.                         ###
###                  Sabrina Servanty & Etienne Pr?vost                      ###
###                          March 2015                                  ###
################################################################################

################################################################################
# /!\ /!\ THIS CODE NEEDS TO BE UPDATED EVERY YEAR
# 1/ If  Lesl? was NOT STOPPED due to flow, only needs to add one line of code: p_ML[x] <- p[x,2]
######### where x is the index of the last year (=Nyear); see L. 180 for an example; add this line after L.182
# 2/ If Lesl? was STOPPED due to flow, needs to update code at 2 places:
# - add one to the upper index of the loop (L.149 where j is the number of years where Lesl? was stopped during the trapping season = 7 until 2014)
# - add one line of code: p[x,2] * pi_ML_dim[y]
######### where x is the index of the last year (=Nyear) and y is the upper index of the loop that was changed at the previous step
######### add it after L.182
################################################################################

######################################################################
#  DATA:
# Nyears: Length of time series
# C_ML[t]: Annual number of smolt captured at Moulin de Lesl?
# Cum_ML[t]: Annual number of smolt captured at Moulin de Lesl? and released unmarked downstream Moulin de Lesl?
# Cm_ML[t]: Annual number of smolt marked and released downstream Moulin de Lesl?
# Cm_MP[t]: Annual number of marked smolt captured at Moulin des Princes
# Cum_MP[t]: Annual number of unmarked smolt captured at Moulin des Princes
# D_MP[t]: Annual number of dead smolt at Moulin des Princes
# Q[t,i]: Mean flow observed from 1st April to May 10 for 1. Moulin des Princes and 2. Moulin de Lesl?
########################################################################
# NOTATION:
# i: trap, 1: Moulin des Princes; 2: Moulin de Lesl?
# p_ML[t]: annual trap efficiency for capturing smolt at Moulin de Lesl?
# p_MP[t]: annual trap efficiency for capturing smolt at Moulin des Princes
# lambda[t]: annual mean smolt population size (mean of Poisson distribution)
# Ntot[t]: annual total smolt population size 
# Nesc[t]: annual smolt population size escaping the river
############################################################################
#############################################################################

model {

############################################################################################
###############          Hyperprior for the trapping efficiency          ################### 
############################################################################################
### Mean and standard deviation of trap efficiency 
    for (i in 1:2) {
        logit_int[i] ~ dunif(-10,10)    #intercept 
        logit_flow[i] ~ dunif(-10,10) #slope for flow data (1 April - 10 May)

        sigmap[i] ~ dunif(0,30)
        varp[i] <- sigmap[i]*sigmap[i] 
        precp[i] <- 1/(varp[i]) # precision
        } # end of loop over traps

rho ~ dunif(-1,1) # prior for the correlation coefficient between probability of capture at the two traps
precmat[1:2,1:2] <- inverse(covmat[,])  # precision matrix

# Building the matrix of variance-covariance
covmat[1,1] <- varp[1] # variance of the probability of capture at MP
covmat[1,2] <- rho * sigmap[1] * sigmap[2]  # covariance
covmat[2,1] <- rho * sigmap[1] * sigmap[2]  # covariance
covmat[2,2] <- varp[2] # variance of the probability of capture at ML
       
### Mean and standard deviation for years when Lesl? is either stopped during several days due to flow or not installed whereas migration already begun (7 years)
junk ~ dt(0,1,1)  # Cauchy distribution
sigmap_ML_dim <- abs(junk)  # half-Cauchy distribution
precp_ML_dim <- pow(sigmap_ML_dim,-2)

#sigmap_ML_dim <- pow(precp_ML_dim,-0.5)
#precp_ML_dim ~ dgamma(7,7) # precision
l_ML_dim ~ dt(0,0.4077711,7.763) # mean 

############################################################################################
###############             Hyperparameters for Ntot       ##################
############################################################################################
# Shape and rate parameter for gamma distribution  for negative binomial (see Gelman, 2d edition, p446)
shape_lambda ~ dgamma(0.001,0.001)
rate_lambda ~ dgamma(0.001,0.001)

mean_gamma <- shape_lambda/rate_lambda
var_gamma <- shape_lambda/(rate_lambda*rate_lambda)
################

############################################################################################
####### Standardization of covariates 
############################################################################################
# Standardization of flow for MP
for (t in 1:Nyears) {
   logQ[t,1] <- log(Q[t,1]) # ln transformation of covariate
   stlogQ[t,1] <- (logQ[t,1] - mean(logQ[,1]))/sd(logQ[,1]) # standardized covariate 
   } # end of loop over years
   
#### Standardization of covariates (flow) for ML
for (t in 3:Nyears) {
   logQ[t,2] <- log(Q[t,2]) # ln transformation of covariate
   stlogQ[t,2] <- (logQ[t,2] - mean(logQ[3:Nyears,2]))/sd(logQ[3:Nyears,2]) # standardized covariate  
   } #end of loop over years

############################################################################################
######## Probabilities
############################################################################################   
############# Probability of capture at MP during the first 2 years
for (t in 1:2) {  # first 2 years, Lesl? is not installed
    logit_mupi[t,1] <- logit_int[1] + logit_flow[1] * stlogQ[t,1]
    logit_pi[t,1] ~ dnorm(logit_mupi[t,1],precp[1])
    p[t,1] <- exp(logit_pi[t,1])/(1+exp(logit_pi[t,1]))  # back-transformation on the probability scale
    epsilon[t,1] <- (logit_pi[t,1] - logit_mupi[t,1])/sigmap[1] # standardized residuals
    eps[t,1] <- logit_pi[t,1] - logit_mupi[t,1] # residuals not standardized   
    } #end of loop over the 2 first years
    
############ Probability of capture at the two traps since 1997
for (t in 3:Nyears) {    
    logit_pi[t,1:2] ~ dmnorm(logit_mupi[t,1:2],precmat[1:2,1:2]) #bivariate multinormal sampling for probability of capture
    } # end of loop over years

for (t in 3:Nyears) {
    for (i in 1:2) {      
        logit_mupi[t,i] <- logit_int[i] + logit_flow[i] * stlogQ[t,i] 
        p[t,i] <- exp(logit_pi[t,i])/(1+exp(logit_pi[t,i]))  # back-transformation on the probability scale
        epsilon[t,i] <- (logit_pi[t,i] - logit_mupi[t,i])/sigmap[i] # standardized residuals
        eps[t,i] <- logit_pi[t,i] - logit_mupi[t,i] # residuals not standardized   
        } ## End of loop over traps
      } ## End of loop over years

test[1] <- step(logit_flow[1]) # is logit_flow at MP >=0 ?
test[2] <- step(logit_flow[2]) # is logit_flow at ML >=0 ?

#Calculating R? = 1 -(E(variance of residuals (/!\ not standardized!) / E(variance of capture probabilities))) for Moulin des Princes
# See Gelman & Pardoe 2006
sdeps_MP <- sd(eps[,1]) 
vareps_MP <- sdeps_MP * sdeps_MP

sdlpi_MP <- sd(logit_pi[,1])
varlpi_MP <- sdlpi_MP * sdlpi_MP

R2[1] <- 1 - (mean(vareps_MP)/mean(varlpi_MP)) 

#Calculating R? = 1 -(E(variance of residuals (/!\ not standardized!) / E(variance of capture probabilities))) for Moulin de Lesl?
# See Gelman & Pardoe 2006
sdeps_ML <- sd(eps[3:Nyears,2]) 
vareps_ML <- sdeps_ML * sdeps_ML

sdlpi_ML <- sd(logit_pi[3:Nyears,2])
varlpi_ML <- sdlpi_ML * sdlpi_ML

R2[2] <- 1 - (mean(vareps_ML)/mean(varlpi_ML)) 
       
###### Setting up probability of capture in Lesl? for years when Lesl? is stopped due to flow problem
for (j in 1:7) { # for years when Lesl? was stopped during several days due to flow or installed after migration has begun (total of 7 years)
  lp_ML_dim[j] ~ dnorm(l_ML_dim,precp_ML_dim)
  pi_ML_dim[j] <- exp(lp_ML_dim[j])/(1+exp(lp_ML_dim[j]))  # back-transformation on the probability scale
  } # end of loop over specific years

###### Setting up total probability of capture 
## Moulin des Princes 
for (t in 1:Nyears) {
    p_MP[t] <- p[t,1]
    } # end of loop over years

# Lesl?(in specific years, probabilities are set to be smaller)  
p_ML[3] <- p[3,2] * pi_ML_dim[1] # probability in 97
p_ML[4] <- p[4,2]
p_ML[5] <- p[5,2] * pi_ML_dim[2] # 1999
p_ML[6] <- p[6,2] * pi_ML_dim[3] # 2000
p_ML[7] <- p[7,2] * pi_ML_dim[4] # 2001

for (t in 8:11) { # from 2002 to 2005
    p_ML[t] <- p[t,2]
    } # end of loop over years
    
p_ML[12] <- p[12,2] * pi_ML_dim[5] # probability in 2006 is set to be smaller

for (t in 13:17) { # from 2007 to 2011
    p_ML[t] <- p[t,2]
    } # end of loop over years
    
p_ML[18] <- p[18,2] * pi_ML_dim[6] #2012
p_ML[19] <- p[19,2] * pi_ML_dim[7] #2013

p_ML[20] <- p[20,2]  # 2014

### /!\ /!\ /!\ NEEDS TO ADD CODE HEREAFTER
p_ML[21] <- p[21,2]  # 2015
p_ML[22] <- p[22,2]  # 2016
p_ML[23] <- p[23,2]  # 2017

######################### Population process ###########################################
# Nyears : from 1995 to now on (migration year)	
for (t in 1:Nyears) {    
################              Prior for Ntot[t], i=1 to Nyears         ######################
  # Hierarchical under negative binomiale		
  lambda[t] ~ dgamma(shape_lambda,rate_lambda) 
  Ntot[t] ~ dpois(lambda[t]) 	
  } # end of loop over years

###################                 LIKELIHOOD               #######################  		
for (t in 1:2) { # 1995 and 1996
   # Binomial for captures at Moulin des Princes
  C_MP[t] ~ dbin (p_MP[t],Ntot[t]) # number of fish captured at Moulin des Princes
	Cm_MP[t] ~ dbin(p_MP[t],Cm_ML[t]) # recapture of marked fish
  
  Nesc[t] <- Ntot[t] - D_MP[t]
  } # end of loop over the two first years

###############################################
# Captures at Moulin de Lesl?
################################################  
for (t in 3:Nyears) {
    C_ML[t] ~ dbin(p_ML[t],Ntot[t])       
    num_ML[t] <- Ntot[t] - C_ML[t] + Cum_ML[t] - D_ML[t] # total unmarked fish 

###############################################
# Captures at Moulin des Princes since Lesl? is installed (1997)
################################################ 
    Cm_MP[t] ~ dbin(p_MP[t],Cm_ML[t]) # marked fish 
    Cum_MP[t] ~ dbin(p_MP[t], num_ML[t]) #unmarked fish
 
    ### Total number of smolt escaping the river
    Nesc[t] <- Cm_ML[t] + num_ML[t] - D_MP[t]
    } # end of loop over years

} # end of the model


