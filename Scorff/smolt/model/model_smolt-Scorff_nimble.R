################################################################################
###           Model of CMR data to estimate smolt population size            ###
###                 of Salmo salar in Scorff river.                          ###
###                  Sabrina Servanty & Etienne Prevost                      ###
###                          March 2015                                      ###
###             Modified by Mathieu Buoro & Etienne Prevost                  ###
###                       March 2020 and 2022                                ###
################################################################################

################################################################################
# /!\ /!\ THIS CODE has been significantly modified in 2022 NEEDS not TO BE UPDATED EVERY YEAR
#   The option for years with trapping interruption at Lesle due to high flow has been removed
#   because it cause pb of MCMC convergence with no gain in estimation quality 
################################################################################

######################################################################
#  DATA:
# Nyears: Length of time series
# C_ML[t]: Annual number of smolt captured at Moulin de Lesle
# Cum_ML[t]: Annual number of smolt captured at Moulin de Lesle and released unmarked downstream Moulin de Lesle
# Cm_ML[t]: Annual number of smolt marked and released downstream Moulin de Lesle
# Cm_MP[t]: Annual number of marked smolt captured at Moulin des Princes
# Cum_MP[t]: Annual number of unmarked smolt captured at Moulin des Princes
# D_MP[t]: Annual number of dead smolt at Moulin des Princes
# Q[t,i]: Mean flow observed from 1st April to May 10 for 1. Moulin des Princes and 2. Moulin de Lesle
########################################################################
# NOTATION:
# i: trap, 1: Moulin des Princes; 2: Moulin de Lesle
# p_ML[t]: annual trap efficiency for capturing smolt at Moulin de Lesle
# p_MP[t]: annual trap efficiency for capturing smolt at Moulin des Princes
# lambda[t]: annual mean smolt population size (mean of Poisson distribution)
# Ntot[t]: annual total smolt population size 
# Nesc[t]: annual smolt population size escaping the river
############################################################################
#############################################################################
model <- nimbleCode({
#model {
############################################################################################
###############          Hyperprior for the trapping efficiency          ################### 
############################################################################################
### Intercept, slope and standard deviation of trap efficiency 
    for (i in 1:2) {
        logit_int[i] ~ dunif(-10,10)    #intercept 
        logit_flow[i] ~ dunif(-10,10) #slope for flow data (1 April - 10 May)
        sigmap[i] ~ dunif(0,30)
        varp[i] <- sigmap[i]*sigmap[i] 
        precp[i] <- 1/(varp[i]) # precision
        } # end of loop over traps

rho ~ dunif(-1,1) # prior for the correlation coefficient between probability of capture at the two traps
precmat[1:2,1:2] <- inverse(covmat[1:2,1:2])  # precision matrix

# Building the matrix of variance-covariance
covmat[1,1] <- varp[1] # variance of the probability of capture at MP
covmat[1,2] <- rho * sigmap[1] * sigmap[2]  # covariance
covmat[2,1] <- rho * sigmap[1] * sigmap[2]  # covariance
covmat[2,2] <- varp[2] # variance of the probability of capture at ML
       
############################################################################################
###############       Hyperparameters for N (smolt number by cohort)      ##################
############################################################################################
# Shape and rate parameter for gamma distribution  for negative binomial (see Gelman, 2d edition, p446)
shape_lambda ~ dgamma(0.001,0.001)
rate_lambda ~ T(dgamma(0.001,0.001), 0.00001,) # protection contre les trop faibles valeurs
mean_gamma <- shape_lambda/rate_lambda
var_gamma <- shape_lambda/(rate_lambda*rate_lambda)
################

############################################################################################
###############     Hyperparameters for p1c (proportion of 1 year old smolt by cohort)
############################################################################################
# Prior pour que s1 et s2 soient superieurs a 1 et que le prior sur p1c soit faiblement informatif
l1 ~ dbeta(1,2); l2 ~ dbeta(1,2)
s1 <- 1+(l1*100); s2 <- 1+(l2*100)

############################################################################################
####### Standardization of flow covariates 
############################################################################################
# mb-21.03.2022 / done in data
## Standardization of flow for MP
# for (t in 1:Nyears) {
#    logQ[t,1] <- log(Q[t,1]) # ln transformation of covariate
#    stlogQ[t,1] <- (logQ[t,1] - mean(logQ[,1]))/sd(logQ[,1]) # standardized covariate 
#    } # end of loop over years
#### Standardization of covariates (flow) for ML
# for (t in 3:Nyears) {
#    logQ[t,2] <- log(Q[t,2]) # ln transformation of covariate
#    stlogQ[t,2] <- (logQ[t,2] - mean(logQ[3:Nyears,2]))/sd(logQ[3:Nyears,2]) # standardized covariate  
#    } #end of loop over years

############################################################################################
######## Probabilities of capture at the traps
############################################################################################   
############# Probability of capture at MP during the first 2 years (no lesle trap)
for (t in 1:2) {  # first 2 years, Lesle is not installed
    logit_mupi[t,1] <- logit_int[1] + logit_flow[1] * stlogQ[t,1]
    logit_pi[t,1] ~ dnorm(logit_mupi[t,1],precp[1])
    p[t,1] <- exp(logit_pi[t,1])/(1+exp(logit_pi[t,1]))  # back-transformation on the probability scale
    epsilon[t,1] <- (logit_pi[t,1] - logit_mupi[t,1])/sigmap[1] # standardized residuals
    eps[t,1] <- logit_pi[t,1] - logit_mupi[t,1] # residuals not standardized   
    } #end of loop over the 2 first years
    
############ Probability of capture at the two traps since 1997
for (t in 3:Nyears) {    
    logit_pi[t,1:2] ~ dmnorm(logit_mupi[t,1:2],precmat[1:2,1:2]) #bivariate multinormal sampling for probability of capture
    for (i in 1:2) {      
        logit_mupi[t,i] <- logit_int[i] + logit_flow[i] * stlogQ[t,i] 
        p[t,i] <- exp(logit_pi[t,i])/(1+exp(logit_pi[t,i]))  # back-transformation on the probability scale
        epsilon[t,i] <- (logit_pi[t,i] - logit_mupi[t,i])/sigmap[i] # standardized residuals
        eps[t,i] <- logit_pi[t,i] - logit_mupi[t,i] # residuals not standardized   
        } ## End of loop over traps
      } ## End of loop over years

test[1] <- step(logit_flow[1]) # is logit_flow at MP >=0 ?
test[2] <- step(logit_flow[2]) # is logit_flow at ML >=0 ?

###### Setting up total probability of capture 
## Moulin des Princes 
for (t in 1:Nyears) {p_MP[t] <- p[t,1]} # end of loop over years
# Lesle  
for (t in 3:Nyears) {p_ML[t] <- p[t,2]} # end of loop over years
# 2020 (COVID: traps not activated from March 15th to May 11th 2020)

######################### Population process ###########################################
# Nyears : from 1995 to now on (migration year)	
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
################              Prior for Ntot[t], i=1 to Nyears         ######################
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

###################                 LIKELIHOOD               #######################  		
for (t in 1:2) { # 1995 and 1996
  # Binomial for captures at Moulin des Princes
  C_MP[t] ~ dbin (p_MP[t],Ntot[t]) # number of fish captured at Moulin des Princes
	Cm_MP[t] ~ dbin(p_MP[t],Cm_ML[t]) # recapture of marked fish
### Total number of smolt escaping the river
  Nesc[t] <- Ntot[t] - D_MP[t]
# Modelling of the age composition data by year of migration
  n1[t] ~ dbin(p1y[t], n[t])
  N1[t] ~ dbin(p1y[t], Ntot[t])
  #n1r[t] <- round(n1[t])
  #n2[t] <- round(n[t])-n1r[t]
  #n2[t] <- n[t]-n1[t]
  } # end of loop over the two first years
# Calculating the proportions of 1 year old smolts by year of migration from the proportions by cohort
  p1y[1] <- p1c[1]*N[1]/(p1c[1]*N[1]+(1-p10c)*N0)
  p1y[2] <- p1c[2]*N[2]/(p1c[2]*N[2]+(1-p1c[1])*N[1])

###############################################
# Captures at Moulin de Lesle
################################################  
for (t in 3:Nyears) {
    C_ML[t] ~ dbin(p_ML[t],Ntot[t])       
    num_ML[t] <- Ntot[t] - C_ML[t] + Cum_ML[t] - D_ML[t] # total unmarked fish 

###############################################
# Captures at Moulin des Princes since Lesle is installed (1997)
################################################ 
    Cm_MP[t] ~ dbin(p_MP[t],Cm_ML[t]) # marked fish 
    Cum_MP[t] ~ dbin(p_MP[t], num_ML[t]) #unmarked fish
### Total number of smolt escaping the river
    Nesc[t] <- Ntot[t] - D_ML[t] - D_MP[t]
# Modelling of the age composition data by year of migration
  n1[t] ~ dbin(p1y[t], n[t])
  N1[t] ~ dbin(p1y[t], N[t])
  #n1r[t] <- round(n1[t])
  #n2[t] <- round(n[t])-n1r[t]
  #n2[t] <- n[t]-n1[t]
# Calculating the proportions of 1 year old smolts by year of migration from the proportions by cohort
  p1y[t] <- p1c[t]*N[t]/(p1c[t]*N[t]+(1-p1c[t-1])*N[t-1])
    } # end of loop over years

  
  ## Distributing smolts 1+/2+ into sex classes - mb+ep-09052024
  for (t in 1:Nyears) {
    # Distributing smolts 1+/2+ into sex classes
    n_1_M[t] ~ dbin(p_male[t,1], N1[t]) # male
    n_1_F[t] <- N1[t]-n_1_M[t] # female

    N2[t] <-  N[t] - N1[t]
    n_2_M[t] ~ dbin(p_male[t,2], N2[t]) # male
    n_2_F[t] <- N2[t]-n_2_M[t] # female

    # Hierarchcal modelling of the proportion of male by age
    p_male[t,1] ~ dbeta(q[1], q[2])
    p_male[t,2] ~ dbeta(q[3], q[4])

    # Samples for genetic sexing
    n_sex_smp[t,1] ~ dbin(p_smp[t,1], n_1_M[t]) # male
    n_sex_smp[t,2] ~ dbin(p_smp[t,1], n_1_F[t]) # female
    n_sex_smp[t,3] ~ dbin(p_smp[t,2], n_2_M[t]) # male
    n_sex_smp[t,4] ~ dbin(p_smp[t,2], n_2_F[t]) # female

    # Hierarchcal modelling of the proportion of samples
    p_smp[t,1] ~ dbeta(2, 2)
    p_smp[t,2] ~ dbeta(2, 2)
  } ## End of loop over years
  # Prior pour que s1 et s2 soient superieurs a 1 et que le prior sur p1c soit faiblement informatif
  ll[1] ~ dbeta(1,2); ll[2] ~ dbeta(1,2)
  q[1] <- 1+(ll[1]*100); q[2] <- 1+(ll[2]*100)
  ll[3] ~ dbeta(1,2); ll[4] ~ dbeta(1,2)
  q[3] <- 1+(ll[3]*100); q[4] <- 1+(ll[4]*100)
}) # end of the model


