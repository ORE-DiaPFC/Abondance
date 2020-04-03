################################################################################
###           Model of CMR data to estimate spawners population size         ###
###                 of Salmo salar in Oir river.                             ###
###                  Sabrina Servanty & Etienne Pr?vost                      ###
###                             June 2015                                    ###
###                  Modified by Buoro M & Prévost E                         ###
###                             March 2020                                   ###
################################################################################

############################################################################################
## t: year; 1 to Y - from 1994 to Y
## 
## a: sea age; 
##    1-1SW (Grisle), 
##    2-MSW (salmon)
## u: effect of being marked
##    1 - marked
##    2 - unmarked
## Sequential stages: Capture at Moulin des Princes, Fishing (exploitation), Mortality followed by recovery of dead fish,
## Recovery during before or shortly after reproduction
##  - Probability of capture at Moulin des Princes the first year (1994) is included in the hierarchy but is considered to be smaller. Captures begun very late for MSW and didn't cover the full season for 1SW. 
## - From 1994 to 2002 (included), we considered 2 steps for fishing. (1): Fish are dying from fishing, (2) Some fished fishes are examined at Moulin des Princes and the marked fish can be recovered.
## - In 2003, fishing on 1SW was forbidden. Set p1_F[10,1] = 0
## - Variance of the hyperparameter of sigmapi_R is not considered to be sea age dependent (only one variance), sigmapi_D and sigmapi_F is not depending on mark but still depending on sea age.
## - Including the effect of ln(flow) in the probability of capture at Moulin des Princes: 1SW (15 June - 15 August), MSW (1 April - 1 June). Flow data are standardized (Q[t] - mean(Q))/sd(Q) within the model. Tracking the standardized residual (temporal variance)
### - Including recapture effort during or after reproduction: standardized ln(number of nights of recapture on spawning ground). Considering a different slope and intercept depending on sea age but same resiudal temporal variance.
## - Temporal residual variance for capture at Moulin des Princes is not sea age dependent. 
## - Including an effect of standardized ln(flow) from 25th november to 24th december in the probability of recapture during or after reproduction. Slope is sea age dependent.
## Fishing and natural mortality are set as a multinormal distribution depending on mark and sea age
## This aspect has been simplified in 2020 with the use of a single cc for fishing exploitation between marked and unmarked (rhoF)
## For the sake of consistency, the same approach is applied to the correlation among death rates (rhoD)
## Associated variance parameters are also no longer dependent on sea age  
############################################################################################

model {
  #################################################################################
  # PROBABILITIES (p)
  # ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  ## HYPER-PARAMETERS
  ## ------
  #################################################################################
  ### Mean and standard deviation of probabilities depending on sea age.
  for (a in 1:2) {
    # Probabilities to be captured at Moulin des Princes.
    pi_MP94[a] ~ dbeta(1,1)  # first year 1994 is considered to be different
      
    logit_int_MP[a] ~ dunif(-10,10)    #intercept 
    logit_flow_MP[a] ~ dunif(-10,10) #slope for flow data (1SW at MP in 15 june - 15 august, MSW in April-June)
      
    # Probabilities to be captured during reproduction or after
    logit_int_R[a] ~ dunif(-10,10) # intercept 
    logit_effort_R[a] ~ dunif(-10,10) # slope for recapture effort
    logit_flow_R[a] ~ dunif(-10,10) # slope for flow in December 
    
    # Probabilities to die (not from fishing). Depends on being marked or not and on sea age.
    for (u in 1:2) {
      mupi_D[a,u] ~ dbeta(1,1) 
    
    # Probability of fishing (exploitation)
      mupi_F[a,u] ~ dbeta(1,1)
      } # End of loop over mark category
# Variance of mortality depends on sea age (reveal by data)    
    sigmapi_D[a] ~ dunif(0,20)
    varpi_D[a] <- (sigmapi_D[a])*(sigmapi_D[a])
    precpi_D[a] <- 1/(varpi_D[a]) # precision
    
#    sigmapi_F[a] ~ dunif(0,20)
#    varpi_F[a] <- sigmapi_F[a]*sigmapi_F[a]
#    precpi_F[a] <- 1/(varpi_F[a]) # precision
 
 # Modified in 2020 : a sing cc is used whatever de the sae age category   
 #   rho_F[a] ~ dunif(-1,1) # prior for the correlation coefficient between probability of being fished between marked and unmarked depending on sea age
 #   rho_D[a] ~ dunif(-1,1) # prior for the correlation coefficient between probability of dying between marked and unmarked depending on sea age
    }# end of loop over sea age

  rho_D ~ dunif(-1,1) # prior for the correlation coefficient between probability of dying between marked and unmarked depending on sea age
# The same option is implemented for the variance  
  sigmapi_MP ~ dunif(0,20) # standard deviation of probabilty of capture at Moulin des Princes
  sigmapi_R ~ dunif(0,20) # standard deviation of probabilty of recapture during reproduction
    
#  sigmapi_D ~ dunif(0,20)
#  varpi_D <- (sigmapi_D)*(sigmapi_D)
#  precpi_D <- 1/(varpi_D) # precision
    
  rho_F ~ dunif(-1,1) # prior for the correlation coefficient between probability of being fished between marked and unmarked depending on sea age
#    for (u in 1:2) {
#      sigmapi_F[u] ~ dunif(0,20)
#      varpi_F[u] <- sigmapi_F[u]*sigmapi_F[u]
#      precpi_F[u] <- 1/(varpi_F[u]) # precision
#     } # End of loop over mark category
  sigmapi_F ~ dunif(0,20)
  varpi_F <- sigmapi_F*sigmapi_F
  precpi_F <- 1/(varpi_F) # precision
  
# Building the matrix of variance-covariance for fishing with different var for marked and unmarked
  precmatF[1:2,1:2] <- inverse(covmatF[,])  # precision matrix
#  covmatF[1,1] <- varpi_F[1] # variance of the probability of being fished for marked individual
#  covmatF[1,2] <- rho_F * sigmapi_F[1] * sigmapi_F[2]  # covariance
#  covmatF[2,1] <- rho_F * sigmapi_F[2] * sigmapi_F[1]  # covariance
#  covmatF[2,2] <- varpi_F[2] # variance of the probability of being fished for unmarked individual

# Modifed in 2020 : a single matrix of variance-covariance for fishing whatever sea age and mark status
  covmatF[1,1] <- varpi_F # variance of the probability of being fished 
  covmatF[1,2] <- rho_F * sigmapi_F * sigmapi_F  # covariance
  covmatF[2,1] <- rho_F * sigmapi_F * sigmapi_F  # covariance
  covmatF[2,2] <- varpi_F # variance of the probability of being fished for unmarked individual
#  covmatF_1SW[1,1] <- varpi_F # variance of the probability of being fished for marked individual
#  covmatF_1SW[1,2] <- rho_F[1] * sigmapi_F[1] * sigmapi_F[1]  # covariance
#  covmatF_1SW[2,1] <- rho_F[1] * sigmapi_F[1] * sigmapi_F[1]  # covariance
# Modifed in 2020 : a single cc whatever the sea age
#  covmatF_1SW[1,2] <- rho_F * sigmapi_F * sigmapi_F  # covariance
#  covmatF_1SW[2,1] <- rho_F * sigmapi_F * sigmapi_F  # covariance
#  covmatF_1SW[2,2] <- varpi_F # variance of the probability of being fished for unmarked individual

# Building the matrix of variance-covariance for MSW for fishing
#  covmatF_MSW[1,1] <- varpi_F # variance of the probability of being fished for marked individual
#  covmatF_MSW[1,2] <- rho_F[2] * sigmapi_F[2] * sigmapi_F[2]  # covariance
#  covmatF_MSW[2,1] <- rho_F[2] * sigmapi_F[2] * sigmapi_F[2]  # covariance
# Modifed in 2020 : a single cc whatever the sea age
#  covmatF_MSW[1,2] <- rho_F * sigmapi_F * sigmapi_F  # covariance
#  covmatF_MSW[2,1] <- rho_F * sigmapi_F * sigmapi_F  # covariance
#  covmatF_MSW[2,2] <- varpi_F # variance of the probability of being fished for unmarked individual

  # Building the matrix of variance-covariance for 1SW for dying from natural causes
  precmatD_1SW[1:2,1:2] <- inverse(covmatD_1SW[,])  # precision matrix
  covmatD_1SW[1,1] <- varpi_D[1] # variance of the probability of dying from natural causes for marked individual
#  covmatD_1SW[1,2] <- rho_D[1] * sigmapi_D[1] * sigmapi_D[1]  # covariance
#  covmatD_1SW[2,1] <- rho_D[1] * sigmapi_D[1] * sigmapi_D[1]  # covariance
# Modifed in 2020 : a single cc whatever the sea age
  covmatD_1SW[1,2] <- rho_D * sigmapi_D[1] * sigmapi_D[1]  # covariance
  covmatD_1SW[2,1] <- rho_D * sigmapi_D[1] * sigmapi_D[1]  # covariance
  covmatD_1SW[2,2] <- varpi_D[1] # variance of the probability of dying from natural causes for unmarked individual

  # Building the matrix of variance-covariance for MSW for dying from natural causes
  precmatD_MSW[1:2,1:2] <- inverse(covmatD_MSW[,])  # precision matrix
  covmatD_MSW[1,1] <- varpi_D[2] # variance of the probability of dying from natural causes for marked individual
#  covmatD_MSW[1,2] <- rho_D[2] * sigmapi_D[2] * sigmapi_D[2]  # covariance
#  covmatD_MSW[2,1] <- rho_D[2] * sigmapi_D[2] * sigmapi_D[2]  # covariance
# Modifed in 2020 : a single cc whatever the sea age
  covmatD_MSW[1,2] <- rho_D * sigmapi_D[2] * sigmapi_D[2]  # covariance
  covmatD_MSW[2,1] <- rho_D * sigmapi_D[2] * sigmapi_D[2]  # covariance
  covmatD_MSW[2,2] <- varpi_D[2] # variance of the probability of dying from natural causes for unmarked individual

# Probability of recover a caught fish (exploitation)
  mupi_oF ~ dbeta(1,1) ; sigmapi_oF ~ dunif(0,20)
  
  ### Mean and standard deviation of the probabilities to observed dead fish
  pi_oD ~ dbeta(1,12)   # informative prior (1 fish out 13 was observed dead)

  ################################################################################
  ## PROBABILITY DISTRIBUTIONS
  ## -------------------------
  ## pi_MP94[a]:probability to be captured at Moulin des Princes given sea age for 1994 
  ## pi_MP[t,a]:annual probability to be captured at Moulin des Princes given sea age
  ## pi_D_1SW[t,a]: annual probability for a marked 1SW to die from cause other than fishing given marking
  ## pi_D_MSW[t,a]: annual probability for a marked MSW to die from cause other than fishing given marking
  ## pi_Dum[t,a]: annual probability for a unmarked fish to die from cause other than fishing given sea age
  ## pi_oD: probability to recover a fish that die from other cause than fishing
  ## pi_F_1SW[t,a]: annual probability of a 1SW to be captured by fishing given marking
  ## pi_F_MSW[t,a]: annual probability of a MSW to be captured by fishing given marking 
  ## pi_oF[t,a]: annual probability to recover a caught fish (exploitation). From 1994 to 2002.
  ## pi_R[t,a]: annual probability to be captured during or after reproduction given sea age
  ####################################################################################  
  ### Probabilities to be captured at Moulin des Princes (time and sea age dependent)
    var_MP <- (sigmapi_MP)*(sigmapi_MP)
    prec_MP <- 1/(var_MP) #precision for residual temporal variance
        
    for (t in 1:Y) { ## pi_MP: Exchangeable from 1994 to now on     
        for (a in 1:2) {
            logQ[t,a] <- log(Q[t,a]) # ln transformation of covariate
            stlogQ[t,a] <- (logQ[t,a] - mean(logQ[,a]))/sd(logQ[,a]) # standardized covariate
               
            logit_mupi_MP[t,a] <- logit_int_MP[a] + logit_flow_MP[a] * stlogQ[t,a]
            logit_pi_MP[t,a] ~ dnorm(logit_mupi_MP[t,a],prec_MP)
            pi_MP[t,a] <- exp(logit_pi_MP[t,a])/(1+exp(logit_pi_MP[t,a]))  # back-transformation on the probability scale
            epsilon_MP[t,a] <- (logit_pi_MP[t,a] - logit_mupi_MP[t,a])/sigmapi_MP # standardized residuals
            } ## End of loop over fish ages 
        } ## End of loop over years

  test[1] <- step(logit_flow_MP[1]) # is logit_flow >=0 for 1SW?
  test[2] <- step(logit_flow_MP[2]) # is logit_flow >=0 for MSW?
  
  ### Probabilities to be re-captured by fishing (time and sea age dependent)
  for (a in 1:2) {
      for (u in 1:2) {
          logit_mupi_F[a,u] <- log(mupi_F[a,u]/(1-mupi_F[a,u])) # logit transformation
          } # end of loop over mark category
      } # end of loop over sea age

  diffF_1SW <- logit_mupi_F[1,1] - logit_mupi_F[1,2]
  diffF_MSW <- logit_mupi_F[2,1] - logit_mupi_F[2,2]

  test[3] <- step(diffF_1SW) # is the mean mortality different between marked and unmarked 1SW?
  test[4] <- step(diffF_MSW) # is the mean mortality different between marked and unmarked MSW?

    
  for (t in 1:9) { ## pi_F: Exchangeable from 1994 to 2002
#      logit_piF_1SW[t,1:2] ~ dmnorm(logit_mupi_F[1,1:2],precmatF_1SW[1:2,1:2])  # 1SW
#      logit_piF_MSW[t,1:2] ~ dmnorm(logit_mupi_F[2,1:2],precmatF_MSW[1:2,1:2])  # MSW
# A single covariance matrix is used for both sea age
      logit_piF_1SW[t,1:2] ~ dmnorm(logit_mupi_F[1,1:2],precmatF[1:2,1:2])  # 1SW
      logit_piF_MSW[t,1:2] ~ dmnorm(logit_mupi_F[2,1:2],precmatF[1:2,1:2])  # MSW

      for (u in 1:2) {
      piF_1SW[t,u] <- exp(logit_piF_1SW[t,u])/(1+exp(logit_piF_1SW[t,u]))  # back-transformation on the probability scale 1SW
      piF_MSW[t,u] <- exp(logit_piF_MSW[t,u])/(1+exp(logit_piF_MSW[t,u]))  # back-transformation on the probability scale MSW
      } ## End of loop over mark category
  } ## End of loop over years
  
  piF_1SW[10,1] <- 0 # No exploitation allowed on 1SW in 2003
  piF_1SW[10,2] <- 0 # No exploitation allowed on 1SW in 2003 
  
#  logit_piF_MSW[10,1:2] ~ dmnorm(logit_mupi_F[2,1:2],precmatF_MSW[1:2,1:2])  # MSW
  logit_piF_MSW[10,1:2] ~ dmnorm(logit_mupi_F[2,1:2],precmatF[1:2,1:2])  # MSW
  piF_MSW[10,1] <- exp(logit_piF_MSW[10,1])/(1+exp(logit_piF_MSW[10,1]))  # back-transformation on the probability scale for marked MSW
  piF_MSW[10,2] <- exp(logit_piF_MSW[10,2])/(1+exp(logit_piF_MSW[10,2]))  # back-transformation on the probability scale for unmarked MSW

  for (t in 11:Y) { ## pi_F: Exchangeable from 2004 to now on
#      logit_piF_1SW[t,1:2] ~ dmnorm(logit_mupi_F[1,1:2],precmatF_1SW[1:2,1:2])  # 1SW
#      logit_piF_MSW[t,1:2] ~ dmnorm(logit_mupi_F[2,1:2],precmatF_MSW[1:2,1:2])  # MSW
      logit_piF_1SW[t,1:2] ~ dmnorm(logit_mupi_F[1,1:2],precmatF[1:2,1:2])  # 1SW
      logit_piF_MSW[t,1:2] ~ dmnorm(logit_mupi_F[2,1:2],precmatF[1:2,1:2])  # MSW

      for (u in 1:2) {
      piF_1SW[t,u] <- exp(logit_piF_1SW[t,u])/(1+exp(logit_piF_1SW[t,u]))  # back-transformation on the probability scale 1SW
      piF_MSW[t,u] <- exp(logit_piF_MSW[t,u])/(1+exp(logit_piF_MSW[t,u]))  # back-transformation on the probability scale MSW
      } ## End of loop over mark category
  } ## End of loop over years

  ### Probability that a caught fish is showed at Moulin des Princes (time and sea age dependent)
    logit_mupi_oF <- log(mupi_oF/(1-mupi_oF)) # logit transformation
    varpi_oF <- (sigmapi_oF)*(sigmapi_oF) 
    precpi_oF <- 1/(varpi_oF) # precision
  
    for (t in 1:9) { ## pi_oF: Exchangeable from 1994 to 2002 
      for (a in 1:2) {
#      logit_pi_oF[t] ~ dnorm(logit_mupi_oF,precpi_oF)
#      pi_oF[t] <- exp(logit_pi_oF[t])/(1+exp(logit_pi_oF[t]))  # back-transformation on the probability scale
# Changed in 2020 : the probability of observing a catch is allow to vary according to year an sea age (consistent with data)
        logit_pi_oF[t,a] ~ dnorm(logit_mupi_oF,precpi_oF)
        pi_oF[t,a] <- exp(logit_pi_oF[t,a])/(1+exp(logit_pi_oF[t,a]))  # back-transformation on the probability scale  
        pi_uoF[t,a] <- 1 - pi_oF[t,a]
        Cuo_F[t,a] ~ dbin(pi_uoF[t,a],C_F[t,a])
# The cut has been introduced because othewise pi_Of is not properly sampled
# The small hierrachcal model above is used to generate "inforamtive priors"
       cut_pi_oF[t,a] <- cut(pi_oF[t,a])  
        }# end of loop over mark category  
      } ## End of loop over years
  
  ### Probabilities to die from other cause than fishing (time, mark and sea age dependent)
  for (a in 1:2) {
    for (u in 1:2) {
      logit_mupi_D[a,u] <- log(mupi_D[a,u]/(1-mupi_D[a,u])) # logit transformation
      }# end of loop over mark category
   
  } # end of loop over sea age
  
  for (t in 1:Y) { ## pi_D: Exchangeable from 1994 to now on
     logit_piD_1SW[t,1:2] ~ dmnorm(logit_mupi_D[1,1:2],precmatD_1SW[1:2,1:2])  # 1SW
     logit_piD_MSW[t,1:2] ~ dmnorm(logit_mupi_D[2,1:2],precmatD_MSW[1:2,1:2])  # MSW
  
     for (u in 1:2) {
      piD_1SW[t,u] <- exp(logit_piD_1SW[t,u])/(1+exp(logit_piD_1SW[t,u]))  # back-transformation on the probability scale 1SW
      piD_MSW[t,u] <- exp(logit_piD_MSW[t,u])/(1+exp(logit_piD_MSW[t,u]))  # back-transformation on the probability scale MSW
      } ## End of loop over mark category
  } ## End of loop over years
 
  
  diff1SW <- logit_mupi_D[1,1] - logit_mupi_D[1,2]
  diffMSW <- logit_mupi_D[2,1] - logit_mupi_D[2,2]

  test[5] <- step(diff1SW) # is the mean mortality different between marked and unmarked 1SW?
  test[6] <- step(diffMSW) # is the mean mortality different between marked and unmarked MSW?

  ### Probabilities to be captured during or after reproduction. (Time dependent and sea age)
  varpi_R <- (sigmapi_R)*(sigmapi_R) 
  precpi_R  <- 1/(varpi_R) # precision
  
  for (t in 1:Y) { ## pi_R: Exchangeable from 1994 to now on
      logeff_R[t] <- log(eff_R[t]) # ln transformation of effort
      stlogeff_R[t] <- (logeff_R[t] - mean(logeff_R[]))/sd(logeff_R[]) # standardized covariate
      
      logQ_dec[t] <- log(Q_dec[t]) # ln transformation of december flow
      stlogQ_dec[t] <- (logQ_dec[t] - mean(logQ_dec[]))/sd(logQ_dec[]) # standardized covariate  
              
      for (a in 1:2) { 
          logit_mupi_R[t,a] <- logit_int_R[a] + logit_effort_R[a] * stlogeff_R[t] + logit_flow_R[a] * stlogQ_dec[t]
          logit_pi_R[t,a] ~ dnorm(logit_mupi_R[t,a],precpi_R)
          pi_R[t,a] <- exp(logit_pi_R[t,a])/(1+exp(logit_pi_R[t,a]))  # back-transformation on the probability scale
          epsilon_R[t,a] <- (logit_pi_R[t,a] - logit_mupi_R[t,a])/sigmapi_R #standardized residuals
          } ## End of loop over sea age 
      } #end of loop over years
 
  test[7] <- step(logit_effort_R[1]) # is logit_effort >=0 for 1SW?   
  test[8] <- step(logit_effort_R[2]) # is logit_effort >=0 for MSW? 
  test[9] <- step(logit_flow_R[1]) #is logit_flow >=0 for 1SW?
  test[10] <- step(logit_flow_R[2]) # is logit_flow >=0 for MSW?  
  
  ######################################################################################################################################
  ######################################################################################################################################
  ## DATA
  ## --------------
  ##  C_MP[t,a]: Annual number of fish captured at Moulin des Princes per sea age category (1SW/MSW)
  ##  Cum_MP[t,a]: Annual number of fish captured at Moulin des Princes and released unmarked per sea age category (happened only once in 1995 for 1SW)
  ##  Cm_MP[t,a]: Annual number of fish captured and marked at Moulin des Princes per sea age category
  ##  Cm_D[t,a]: Annual number of marked fish recaptured dead (not from fishing) per sea age category
  ##  Cum_D[t,a]: Annual number of unmarked fish recaptured dead (not from fishing) per sea age category
  ##  C_F[t,a]: Annual number of fish caught by fishing per sea age category (from 1994 to 2002)
  ##  Cm_F[t,a]: Annual number of marked fish caught by fishing per sea age category and showed at Moulin des Princes
  ##  Cum_F[t,a]: Annual number of unmarked fish caught by fishing per sea age category and shown at Moulin des Princes
  ##  Cuo_F[t,a] :  Annual number of fish caught by fishing per sea age category and not shown at Moulin des Princes (new from 2020)
  ##  Cm_R[t,a]: Annual number of marked fish caught during or after reproduction per sea age category
  ##  Cum_R[t,a]: Annual number of unmarked fish caught during or after reproduction per sea age category
    
  ######################################################################################
  ## NUMBERS PER GROUP
  ## |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  ## n_tot[t]: annual total number of fish                     
  ## n_1SW[t]: annual Number of 1SW  
  ## n_MSW[t]: annual Number of MSW
  ## We included a hierarchy on breeding category.
  ####################################################  
  ## PRIORS
  ## ------
  ####################################################################################
  ## Annual returns
  for (t in 1:Y) { 
    n_tot[t] <- sum(n[t,])    # total number
    n_1SW[t] <- n[t,1] # Number of males and females 1SW
    n_MSW[t] <- n[t,2] # Number of males and females MSW 
  } ## End of loop over years
  
  # Shape and rate parameter for gamma distribution for negative binomial (see Gelman, 2d edition, p446)
  shape_lambda ~ dgamma(0.003,0.003)
  rate_lambda ~ dgamma(0.003,0.003)
  
  #To be able to initiate lambda_tot ensuite
  lambda_tot0 ~ dgamma(shape_lambda,rate_lambda)
  Plambda0[1:2] ~ ddirich(s[])
  
  # Hyperprior for lambda_n
  for (t in 1:Y) {
    lambda_tot[t] ~ dgamma(shape_lambda,rate_lambda) 
    Plambda[t,1:2] ~ ddirich(s[])
  }
  
  for (a in 1:2) {
    s[a] ~ dexp(0.25)T(0.02,)
    #s[a] ~ dexp(0.1)T(0.02,)
    }
  
  for (t in 1:Y-1) {
    lambda_n[t,1]  <- lambda_tot[t] * Plambda[t,1]
    lambda_n[t+1,2]  <- lambda_tot[t] * Plambda[t,2]
    }
  
  lambda_n[1,2] <- lambda_tot0 * Plambda0[2]
    
  lambda_n[Y,1] <- lambda_tot[Y] * Plambda[Y,1] 
    
  for (a in 1:2) {
    for (t in 1:Y) {
      n[t,a] ~ dpois(lambda_n[t,a]) # annual number of fish per sea age
      }
  }
    
    #################################
    ## CAPTURE AT MOULIN DES PRINCES
    ## -------
    ## n_um[t,a]: Annual number of fish not captured (not marked) at Moulin des Princes per sea age
    ##################################
  for (a in 1:2) {
    
    p_MP94_tot[a] <- pi_MP[1,a] * pi_MP94[a] # probability in 94 to be observed is set to be smaller than later

    C_MP[1,a] ~ dbin(p_MP94_tot[a],n[1,a])   # 1994 is considered to be different.
    n_um[1,a] <- n[1,a] - C_MP[1,a] + Cum_MP[1,a] ## Fish number avoiding the trap, not marked fish (n_um) in 1994
    
    for (t in 2:Y) {
    ## Likelihood on trapped fish number
      C_MP[t,a] ~ dbin(pi_MP[t,a],n[t,a]) 
       
    ## Fish number avoiding the trap, not marked fish (n_um). 
      n_um[t,a] <- n[t,a] - C_MP[t,a] + Cum_MP[t,a]
      } #end of loop over years
  } # end of loop over sea age category

    ########################################
    ## MORTALITY FROM FISHING
    ## --------------------------------
    ## m_F[t,a]: annual number of marked fish caught by fishing per sea age
    ## um_F[t,a]: annual number of unmarked fish caught by fishing per sea age 

# First year (1994)-> see explanations below
      pi_moF[1,1] <- piF_1SW[1,1] * cut_pi_oF[1,1]
      Cm_F[1,1] ~ dbin(pi_moF[1,1],Cm_MP[1,1])  # Changed in March 2020
      pi_umoF[1,1] <- piF_1SW[1,2] * cut_pi_oF[1,1]
      Cum_F[1,1] ~ dbin(pi_umoF[1,1],n_um[1,1])  # Changed in March 2020
      pi_moF[1,2] <- piF_MSW[1,1] * cut_pi_oF[1,2]
      Cm_F[1,2] ~ dbin(pi_moF[1,2],Cm_MP[1,2])  # Changed in March 2020
      pi_umoF[1,2] <- piF_MSW[1,2] * cut_pi_oF[1,2]
      Cum_F[1,2] ~ dbin(pi_umoF[1,2],n_um[1,2])  # Changed in March 2020
      xuo_F[1,1] <- Cuo_F[1,1]-0.1  # Protection against 0 when all fish were presented
      pi_muoF[1,1] <- (step(xuo_F[1,1])*(p_MP94_tot[1]*piF_1SW[1,1]) / (p_MP94_tot[1]*piF_1SW[1,1] + (1-p_MP94_tot[1])*piF_1SW[1,2])) + (1-step(xuo_F[1,1]))
      uo_F[1,1] <- step(xuo_F[1,1])* Cuo_F[1,1] + (1-step(xuo_F[1,1])) 
      Cmuo_F[1,1] ~ dbin(pi_muoF[1,1],uo_F[1,1])
      xuo_F[1,2] <- Cuo_F[1,2]-0.1
      pi_muoF[1,2] <- (step(xuo_F[1,2])*(p_MP94_tot[2]*piF_MSW[1,1]) / (p_MP94_tot[2]*piF_MSW[1,1] + (1-p_MP94_tot[2])*piF_MSW[1,2])) + (1-step(xuo_F[1,2]))
      uo_F[1,2] <- step(xuo_F[1,2])* Cuo_F[1,2] + (1-step(xuo_F[1,2]))
      Cmuo_F[1,2] ~ dbin(pi_muoF[1,2],uo_F[1,2])
      pi_uo_F[1,1] <- ((Cm_MP[1,1]*piF_1SW[1,1] + n_um[1,1]*piF_1SW[1,2])/(Cm_MP[1,1] + n_um[1,1])) * (1-cut_pi_oF[1,1]) # 1SW
      pi_uo_F[1,2] <- ((Cm_MP[1,2]*piF_MSW[1,1] + n_um[1,2]*piF_MSW[1,2])/(Cm_MP[1,2] + n_um[1,2])) * (1-cut_pi_oF[1,2]) # MSW
     for (a in 1:2) {    
      n_F[1,a] <- Cm_MP[1,a] + n_um[1,a]
      C_uoF[1,a] ~ dbin(pi_uo_F[1,a],n_F[1,a])  # Changed in March 2020
#      m_F[1,a] <- Cm_F[1,a] + step(xuo_F[1,a])*Cmuo_F[1,a]
#      um_F[1,a] <- Cum_F[1,a] + Cuo_F[1,a] - step(xuo_F[1,a])*Cmuo_F[1,a]  
      nv_m[1,a] <- Cm_MP[1,a] - Cm_F[1,a] - step(xuo_F[1,a])*Cmuo_F[1,a] # marked
      nv_um[1,a] <- n_um[1,a] - Cum_F[1,a] - Cuo_F[1,a] + step(xuo_F[1,a])*Cmuo_F[1,a] #unmarked 
      }

#  for (t in 1:9) { # from 1994 to 2002
# Major change of this section in 2020
  for (t in 2:9) { # from 1995 to 2002
      # Marked fish
#      m_F[t,1] ~ dbin(piF_1SW[t,1],Cm_MP[t,1])  #1SW  
#      m_F[t,2] ~ dbin(piF_MSW[t,1],Cm_MP[t,2])  #MSW  

      ## Unmarked fish
#      um_F[t,1] ~ dbin(piF_1SW[t,2],n_um[t,1])  #1SW  
#      um_F[t,2] ~ dbin(piF_MSW[t,2],n_um[t,2])  #MSW  
            
#    for (a in 1:2) {    
#     # WINBUGS trick because dsum function is not available (available in JAGS)
#     xC_F[t,a] <- m_F[t,a] + um_F[t,a]
#      C_F[t,a] ~ dnorm(xC_F[t,a],100)
#              
#      ## Fish susceptible to die from other cause than fishing per sea age
#      nv_m[t,a] <- Cm_MP[t,a] - m_F[t,a] # marked
#      nv_um[t,a] <- n_um[t,a] - um_F[t,a] #unmarked 
#     Probability of being fished and observed at MP for a 1SW marked fish
      pi_moF[t,1] <- piF_1SW[t,1] * cut_pi_oF[t,1]
      Cm_F[t,1] ~ dbin(pi_moF[t,1],Cm_MP[t,1])  # Changed in March 2020
#     Probability of being fished and observed at MP for a 1SW unmarked fish
      pi_umoF[t,1] <- piF_1SW[t,2] * cut_pi_oF[t,1]
      Cum_F[t,1] ~ dbin(pi_umoF[t,1],n_um[t,1])  # Changed in March 2020
#     Same structure for MSW fish
      pi_moF[t,2] <- piF_MSW[t,1] * cut_pi_oF[t,2]
      Cm_F[t,2] ~ dbin(pi_moF[t,2],Cm_MP[t,2])  # Changed in March 2020
      pi_umoF[t,2] <- piF_MSW[t,2] * cut_pi_oF[t,2]
      Cum_F[t,2] ~ dbin(pi_umoF[t,2],n_um[t,2])  # Changed in March 2020
#     Modeling the number of marked fish among the 1SW fish not observed at MP
      xuo_F[t,1] <- Cuo_F[t,1]-0.1  # Protection against 0 when all fish were presented
#     Calculation of the probability of being marked among the fish not observed at MP
#     When Cuo_F = 0, pi_muoF[t,1] = 1 & uo_F[t,1] = 1 
      pi_muoF[t,1] <- (step(xuo_F[t,1])*(pi_MP[t,1]*piF_1SW[t,1]) / (pi_MP[t,1]*piF_1SW[t,1]+ (1-pi_MP[t,1])*piF_1SW[t,2])) + (1-step(xuo_F[t,1]))
#     When Cuo_F = 0, uo_F[t,1] = 1  otherwise Cuo_F
      uo_F[t,1] <- step(xuo_F[t,1])* Cuo_F[t,1] + (1-step(xuo_F[t,1])) 
      Cmuo_F[t,1] ~ dbin(pi_muoF[t,1],uo_F[t,1])
#     Same structure for MSW fish
      xuo_F[t,2] <- Cuo_F[t,2]-0.1
      pi_muoF[t,2] <- (step(xuo_F[t,2])*(pi_MP[t,2]*piF_MSW[t,1]) / (pi_MP[t,2]*piF_MSW[t,1]+ (1-pi_MP[t,2])*piF_MSW[t,2])) + (1-step(xuo_F[t,2]))
      uo_F[t,2] <- step(xuo_F[t,2])* Cuo_F[t,2] + (1-step(xuo_F[t,2]))
      Cmuo_F[t,2] ~ dbin(pi_muoF[t,2],uo_F[t,2])
#     Probability of being fished and unobserved at MP
      pi_uo_F[t,1] <- ((Cm_MP[t,1]*piF_1SW[t,1] + n_um[t,1]*piF_1SW[t,2])/(Cm_MP[t,1] + n_um[t,1])) * (1-cut_pi_oF[t,1]) # 1SW
      pi_uo_F[t,2] <- ((Cm_MP[t,2]*piF_MSW[t,1] + n_um[t,2]*piF_MSW[t,2])/(Cm_MP[t,2] + n_um[t,2])) * (1-cut_pi_oF[t,2]) # MSW
      
    for (a in 1:2) {    
     # WINBUGS trick because dsum function is not available (available in JAGS)
#     xC_F[t,a] <- m_F[t,a] - Cm_F[t,a] + um_F[t,a] - Cum_F[t,a]
#     pi_uoF[t,a]<- 1 - pi_oF[t,a]
#     Cuo_F[t,a] ~ dbin(pi_uoF[t,a],xC_F[t,a])

#     Fish caught but unobserved à MP
#     Total number of 1SW fish available to the fishery
      n_F[t,a] <- Cm_MP[t,a] + n_um[t,a]
      C_uoF[t,a] ~ dbin(pi_uoF[t,a],n_F[t,a])  # Changed in March 2020
              
      # Marked fish
#      m_F[t,a] <- Cm_F[t,a] + step(xuo_F[t,a])*Cmuo_F[t,a]
      ## Unmarked fish
#      um_F[t,a] <- Cum_F[t,a] + Cuo_F[t,a] - step(xuo_F[t,a])*Cmuo_F[t,a]    

      ## Fish susceptible to die from other cause than fishing per sea age
      nv_m[t,a] <- Cm_MP[t,a] - Cm_F[t,a] - step(xuo_F[t,a])*Cmuo_F[t,a] # marked
      nv_um[t,a] <- n_um[t,a] - Cum_F[t,a] - Cuo_F[t,a] + step(xuo_F[t,a])*Cmuo_F[t,a] #unmarked 

      ########################################
      ## RECOVERY OF DEAD FISH FROM FISHING (from 1994 to 2002)
      ## --------------------------------
      ##  
      # Marked fish
#      Cm_F[t,a] ~ dbin(pi_oF[t],m_F[t,a])
#      Cm_F[t,a] ~ dbin(pi_oF[t,a],m_F[t,a])  # Changed in March 2020
      ## Unmarked fish
#      Cum_F[t,a] ~ dbin(pi_oF[t],um_F[t,a])      
#      Cum_F[t,a] ~ dbin(pi_oF[t,a],um_F[t,a])  # Changed in March 2020    
#      Cum_F[t,a] ~ dbin(pi_oF[t,a],um_F[t,a])  # Changed in March 2020    
      } # end of loop over sea age
   } # end of loop over years
  
  for (t in 10:Y) { # from 2003 to now on
   # Marked fish
   Cm_F[t,1] ~ dbin(piF_1SW[t,1],Cm_MP[t,1])  #1SW
   Cm_F[t,2] ~ dbin(piF_MSW[t,1],Cm_MP[t,2])  #MSW 
   
   # Unmarked fish
   Cum_F[t,1] ~ dbin(piF_1SW[t,2],n_um[t,1])  #1SW 
   Cum_F[t,2] ~ dbin(piF_MSW[t,2],n_um[t,2])  #MSW
   
   for (a in 1:2) {
       ## Fish susceptible to die from other cause than fishing per sea age
        nv_m[t,a] <- Cm_MP[t,a] - Cm_F[t,a] # marked
        nv_um[t,a] <- n_um[t,a] - Cum_F[t,a] #unmarked   
        } #end of loop over sea age category 
  } # end of loop over years
    
    
    ####################################
    ## MORTALITY FROM OTHER CAUSE THAN FISHING
    ## --------------
    ## m_D[t,a]: annual number of marked fish dying from other cause than fishing per sea age
    ## um_D[t,a]: annual number of unmarked fish dying from other cause than fishing per sea age 
    ## 
  for (t in 1:Y) { 
    ## Marked fish
    m_D[t,1] ~ dbin(piD_1SW[t,1],nv_m[t,1])   #1SW
    m_D[t,2] ~ dbin(piD_MSW[t,1],nv_m[t,2])   #MSW
      
    ## Unmarked fish    
    um_D[t,1] ~ dbin(piD_1SW[t,2],nv_um[t,1])  #1SW
    um_D[t,2] ~ dbin(piD_MSW[t,2],nv_um[t,2])  #MSW
      
    #######################################
    ## RECOVERY OF DEAD FISH NOT FROM FISHING
    ## -------------------------------
    ##
    for (a in 1:2) {
        ## Marked fish
        Cm_D[t,a] ~ dbin(pi_oD,m_D[t,a])
        ## Unmarked fish
        Cum_D[t,a] ~ dbin(pi_oD,um_D[t,a])
     
        ## Fish susceptible to be caught during or after reproduction (escapement)
        e_m[t,a] <- nv_m[t,a] - m_D[t,a]
        e_um[t,a] <- nv_um[t,a] - um_D[t,a]
      
      #####################################
      ## RECAPTURE OF FISH DURING OR AFTER REPRODUCTION
      ## -------------------------------------
      ##
      ## Marked fish
      Cm_R[t,a] ~ dbin(pi_R[t,a],e_m[t,a])
      ## Unmarked fish
     Cum_R[t,a] ~ dbin(pi_R[t,a],e_um[t,a])
     } # end of loop over sea age
  } # end of loop over years

  ############
  # ESCAPEMENT
  ##############  
  for (t in 1:Y){
  
    e_tot[t] <- sum(e_m[t,]) + sum(e_um[t,]) # total escapement
    e_1SW[t] <- e_m[t,1] + e_um[t,1] # escapement of 1SW
    e_MSW[t] <- e_m[t,2] + e_um[t,2] # escapement of MSW
    }  # end of loop over years 
  
} # end of the model
      
