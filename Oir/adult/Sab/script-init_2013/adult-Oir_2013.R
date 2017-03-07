################################################################################
###           Model of CMR data to estimate spawners population size         ###
###                 of Salmo salar in Oir river.                             ###
###                  Sabrina Servanty & Etienne Prévost                      ###
###                          December 2014                                   ###
################################################################################

model {

############################################################################
# Differences from Rivot & Prevost (2002) is:
# - considering nested binomial for likelihood of captured/recapture fish instead of hypergeometric distribution
# - adding breeding categories (1: Male 1SW, 2: Female 1SW, 3: Male MSW, 4: Female MSW). The few unidentified individuals were reattibuted in a breeding categories depending on the mean observed proportion of the bredding categories or on best guess when observing coinjointly the table of capture and the table of recapture. 
# - considering probability to be trapped different depending on sea age.                         
# - considering probability to be recaptured different depending on sea age and breeding categories 
# - probability of capture at Moulin de Cerisel in 1990 was set to be smaller: trapping begun only 1rst november
# - including the effect of flow (covariate) in the probability of capture at Moulin de Cerisel. Same time period for 1SW & MSW: 15 October to 31 December. Flow data are standardized (ln(Q[t]) - mean(ln(Q)))/sd(ln(Q)) within the model. Residuals are standardized and followed.
##########################################################################

##########################################################################################
# Used indices:
## t: year; 1 to Nyears - from 1984 to Nyears
## g: Adult/Breeder category (i.e. sex X sea age class); 
##    1-Male/1 Sea Winter,
##    2-Female/1 Sea Winter, 
##    3-Male/Multi Sea Winter, 
##    4-Female/Multi Sea Winter
## a: sea age; 
##    1-1SW (Grisle), 
##    2-MSW (salmon)
######################################################################################################################################

#########################################################################
# DATA:
# Nyears: Length of time series
# C_MC[t,g]: Annual number of fish captured at the trap (Moulin Cerisel) per breeding category
# Cm_MC[t,g]: Annual number of fish marked and released at the trap (Moulin Cerisel) per breeding category 
# Cm_R[t,g]: Annual number of mark-recaptured fish per breeding category
# Cum_R[t,g]: Annual number of unmark-recaptured fish (Nr-Nrm) per breeding category
# Q[t]: Annual mean flow from 15 october to 31 december 
########################################################################
 
#################################################################################
#################################################################################
## HYPERPRIORS ON PROBABILITIES
## ------
#################################################################################
################## Hyperprior for the probability of capture  at Moulin de Cerisel            #######################
for (a in 1:2){
  ### Mean and standard deviation of trap efficiency. Depends on sea age.
  p_MC90[a] ~ dbeta(1,1)  # year 1990 is considered to be different
  
  logit_int_MC[a] ~ dunif(-10,10)    #intercept 
  logit_flow_MC[a] ~ dunif(-10,10)  #slope for flow data depends on sea age (same flow used: 15 october -31 december)
  sigmap_eff[a] ~ dunif(0.001,20)
  
  varp_eff[a] <- (sigmap_eff[a])*(sigmap_eff[a]) 
  precp_eff[a] <- 1/(varp_eff[a]) # precision
  } # end of loop over sea age

test[1] <- step(logit_flow_MC[1]) # is logit_flow >=0 for 1SW?
test[2] <- step(logit_flow_MC[2]) # is logit_flow >=0 for MSW?

#####             Hyperprior for the probability of recapture ######################
### Mean and standard deviation of the probabilities to be Re-captured #######################
for (g in 1:4) {
  mup_recap[g] ~ dbeta(1,1) 
  sigmap_recap[g] ~ dunif(0.01,30)

  logit_mup_recap[g] <- log(mup_recap[g]/(1-mup_recap[g])) # logit transformation
  varp_recap[g] <- (sigmap_recap[g])*(sigmap_recap[g]) 
  precp_recap[g] <- 1/(varp_recap[g]) # precision  
  } # end of loop over breeding categories

# Is there any difference in mean in the probability of recapture?
diffrec[1] <- mup_recap[1] - mup_recap[2] #Male 1SW vs Female 1SW
diffrec[2] <- mup_recap[1] - mup_recap[3] #Male 1SW vs Male MSW
diffrec[3] <- mup_recap[1] - mup_recap[4] #Male 1SW vs Female MSW
diffrec[4] <- mup_recap[2] - mup_recap[3] #Female 1SW vs Male MSW 
diffrec[5] <- mup_recap[2] - mup_recap[4] #Female 1SW vs Female MSW
diffrec[6] <- mup_recap[3] - mup_recap[4] #Male MSW vs Female MSW 

test[3] <- step(diffrec[1])
test[4] <- step(diffrec[2]) 
test[5] <- step(diffrec[3])  
test[6] <- step(diffrec[4])  
test[7] <- step(diffrec[5])  
test[8] <- step(diffrec[6])  

################################################################################
## PROBABILITY DISTRIBUTIONS
## -------------------------
## pi_MC[t,a]: annual probability to be captured at Moulin de Cerisel per sea age
## p_recap[t,g]: annual probability to be recaptured per breeding categories
##################
# Capture at Moulin Cerisel
######
for (t in 1:Nyears) { 
   logQ[t] <- log(Q[t]) # ln transformation of covariate
   stlogQ[t] <- (logQ[t] - mean(logQ[]))/sd(logQ[]) # standardized covariate
   
   for (a in 1:2) {
      logit_mupi_eff[t,a] <- logit_int_MC[a] + logit_flow_MC[a] * stlogQ[t]
      logit_pi_eff[t,a] ~ dnorm(logit_mupi_eff[t,a],precp_eff[a])
      pi_MC[t,a] <- exp(logit_pi_eff[t,a])/(1+exp(logit_pi_eff[t,a]))  # back-transformation on the probability scale
      epsilon_MC[t,a] <- (logit_pi_eff[t,a] - logit_mupi_eff[t,a])/sigmap_eff[a] # standardized residuals
      } ## End of loop over sea age
   } ## End of loop over years

### Probabilities to be Re-captured
for (t in 1:Nyears) {
    for (g in 1:4) {
      #Prior for p_recap
      logit_p_recap[t,g] ~ dnorm(logit_mup_recap[g],precp_recap[g])
      p_recap[t,g] <- exp(logit_p_recap[t,g])/(1+exp(logit_p_recap[t,g]))  # back-transformation on the probability scale
      } ## End of loop over sea age
   } ## End of loop over breeding categories
        
######################################################################################
## NUMBERS PER GROUP
## |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## n_tot[t]: annual total number of fish                     
## n_1SW[t]: annual Number of 1SW  
## n_MSW[t]: annual Number of MSW
## lambda[t]: annual mean population size (mean of Poisson distribution)
## Plambda[t,g]: annual mean proportion of breeding category
## Nesc[t,g]: annual number of adults that going to reproduce per breeding category.
## Nesc_tot[t]: annual number of adults that going to reproduce
## We included a hierarchy on breeding category.
####################################################  
## PRIORS
## ------
####################################################################################
## Annual returns
for (t in 1:Nyears) {
  n_tot[t] <- sum(n[t,])    # total number
  n_1SW[t] <- sum(n[t,1:2]) # Number of males and females 1SW
  n_MSW[t] <- sum(n[t,3:4]) # Number of males and females MSW 
  } # end of loop over years
  
##################                 Hyperprior for Ntot                #######################
# Shape and rate parameter for gamma distribution  for negative binomial (see Gelman, 2d edition, p446)
shape_lambda ~ dgamma(0.005,0.005)
rate_lambda ~ dgamma(0.005,0.005)

# Prior for dirichlet distribution 
for (g in 1:4) { 
  s[g] ~ dexp(0.25)
  } #end of loop over sea age

#To be able to initiate lambda for each breeding category
lambda0 ~ dgamma(shape_lambda,rate_lambda)
Plambda0[1:4] ~ ddirich(s[])

# Prior for lambda_tot and Plambda
for (t in 1:Nyears) {
  #Hierarchical under negative binomial  
  lambda[t] ~ dgamma(shape_lambda,rate_lambda) 
  Plambda[t,1:4] ~ ddirich(s[])
  
  for (g in 1:4) {
    n[t,g] ~ dpois(lambda_n[t,g]) # annual number of fish per breeding category
  } # end of loop over breeding category
}# end of loop over years

### lambda_n is calculated by smolt cohort (proportion and mean population size of MSW is one year yearlier than 1SW)
## Firt year for MSW 
lambda_n[1,3] <- lambda0 * Plambda0[3]
lambda_n[1,4] <- lambda0 * Plambda0[4]

for (t in 1:(Nyears-1)) {
  lambda_n[t,1]  <- lambda[t] * Plambda[t,1]
  lambda_n[t,2]  <- lambda[t] * Plambda[t,2]
  lambda_n[t+1,3] <- lambda[t] * Plambda[t,3] 
  lambda_n[t+1,4] <- lambda[t] * Plambda[t,4] 
  }# end of loop over years

# Last year for 1SW 
lambda_n[Nyears,1] <- lambda[Nyears] * Plambda[Nyears,1]
lambda_n[Nyears,2] <- lambda[Nyears] * Plambda[Nyears,2]
  

#################################
## CAPTURE AT MOULIN DE CERISEL
## -------
for (t in 1:6) { #from 1984 to 1989 
 for (g in 1:2) { # 1SW
    # Binomial model for capture at the trap (Moulin Cerisel)
    C_MC[t,g] ~ dbin(pi_MC[t,1],n[t,g])  
    } #end of loop over 1SW
  
  for (g in 3:4) { #MSW
    # Binomial model for capture at the trap (Moulin Cerisel)
    C_MC[t,g] ~ dbin(pi_MC[t,2],n[t,g])  
    } # end of loop over MSW
  } # end of loop over years

## Year 1990: partial trapping
p_MC90_1SW <- pi_MC[7,1] * p_MC90[1]
p_MC90_MSW <- pi_MC[7,2] * p_MC90[2]

for (g in 1:2) { # 1SW
    C_MC[7,g] ~ dbin(p_MC90_1SW,n[7,g]) 
    } # end of loop over 1SW

for (g in 3:4) { #MSW
    # Binomial model for capture at the trap (Moulin Cerisel)
    C_MC[7,g] ~ dbin(p_MC90_MSW,n[7,g])  
    } # end of loop over MSW 
    
for (t in 8:Nyears) { #from 1991 to now on
 for (g in 1:2) { # 1SW
    # Binomial model for capture at the trap (Moulin Cerisel)
    C_MC[t,g] ~ dbin(pi_MC[t,1],n[t,g])  
    } #end of loop over 1SW
  
 for (g in 3:4) { #MSW
    # Binomial model for capture at the trap (Moulin Cerisel)
    C_MC[t,g] ~ dbin(pi_MC[t,2],n[t,g])  
    } # end of loop over MSW
 } # end of loop over years

## Escapement
for (t in 1:Nyears) { 
  for (g in 1:4) { #over all breeding category
    # Number that escape to the trap (non marked fish that mate)
    Cum_MC[t,g] <- n[t,g] - C_MC[t,g]
    
    # Escapement per breeding category (number of fish that mate)
    Nesc[t,g] <- Cum_MC[t,g] + Cm_MC[t,g]   
    } #end of loop over breeding categories
    
  # Total escapement
  Nesc_tot[t] <- sum(Nesc[t,])
  Nesc_1SW[t] <- sum(Nesc[t,1:2])
  Nesc_MSW[t] <- sum(Nesc[t,3:4])
  }# end of loop over years
  
##########################
# Recapture
############################    
for (t in 1:Nyears) {
  for (g in 1:4) {     
      # Binomial model for recapture of marked fished
      Cm_R[t,g] ~ dbin(p_recap[t,g],Cm_MC[t,g]) 
    
      # Binomial model for recapture for unmarked fish
      Cum_R[t,g] ~ dbin(p_recap[t,g],Cum_MC[t,g]) 
      } # end of loop over breeding category
    } # end of the loop on year 
	
} # end of the model