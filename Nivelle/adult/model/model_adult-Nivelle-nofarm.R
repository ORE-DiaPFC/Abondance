################################################################################
###           Model of CMR data to estimate spawners population size         ###
###                 of Salmo salar in Nivelle river.                         ###
###                  Sabrina Servanty & Etienne Prévost                      ###
###                          December 2014                                   ###
################################################################################

model {

##########################################################################################
## Differences compare to Mélanie Brun's model:
## - Correction for calculating number of eggs (there was a mistake, fecundity was applied to both males and females)
## - Hierarchy on lambda over breeding category
## - Tried to incorporate a flow effect in probability of capture at Uxondoa. But the effect of flow was non significant so it is not included. Selected temporal window was:
############ 1SW:1st June-31 July
##########   MSW 15 April - 15 June
## - Reduce effort in trapping since 2012:
############ Uxondoa : Introducing new data which is the ratio of the number of trapping realized every year since 2012 over the mean number of nights of trapping over the period 1984-2011
############ Olha : Until 2011, except 2000, all fish that are going in the HC zone are captured(data = nm_2 and num_2).
# Since 2012, not every fish is captured (and so for 2000). Data observed are Cm_O and Cum_O.
# Until 2011, escapment was perfectly known which is not the case since 2012. We're introducing new data corresponding to the sum of the fish removed from the population (e.g., removed for experiment) or added to the population (case of 2000 for instance) = net balance.
# Introducing new data corresponding to the ratio of the number of nights of trapping realized every year since 2012 (and 2000) over the mean number of nights of trapping over the period (1994 to 2011 without 2000). 1992 and 1993 are not included because captures were not done during the full period but effort is considered to be one for those two years because the trap was working when fish are passing in UN.
# Since 2018 the reduction in trapping effort is modeled hiererchically with a variation and a systematic diffrential between 
# the proportion of days with trapping and the actual probability of capture. 
# The differntial do not apply to 2000 for Olha as that year is not equivalent to the others : very few days of trappinga and targeting on days with favourable flows     
############################################################################################
# Since 2018 all the precison parameters are modeled hierarchically with a common gamma distribution
##########################################################################################
# Used indices:
## t: year; 1 to Y - from 1984 to Y
## ij: zone; 11-downstream Lower Nivelle (LN1), 
##           12-upstream Lower Nivelle (LN2),
##           1.-Lower Nivelle (LN): LN1 + LN2, 
##           21-Upper Nivelle (UN), 
##           22-Lurgorrieta (LUR),
##           2.-High Catchment (HC): UN + LUR
## g: Adult/Breeder category (i.e. sex X sea age class); 
##    1-Male/1 Sea Winter,
##    2-Female/1 Sea Winter, 
##    3-Male/Multi Sea Winter, 
##    4-Female/Multi Sea Winter
## h: habitat; 
##    1-riffle/rapid, 
##    2-run
## v: capture device; 
##    U-Uxondoa partial trap, 
##    EF-recapture by Electric Fishing, angling or by finding dead adults (until 1991),
##    O-Olha total trap (from 1992)
## a: sea age; 
##    1-1SW (Grisle), 
##    2-MSW (salmon)
######################################################################################################################################

######################################################################################################################################
## DATA
## --------------
## Y: Length of the time series
##
## D_11[t,g]: Annual number of dead fish between Ascain and Uxondoa per breeding category
## D_12[t,g]: Annual number of dead fish between Uxondoa and Olha per breeding category
## Dm_12[t,g]: Annual number of dead fish with a mark between Uxondoa and Olha per breeding category (data no longer used since changes made for accounting for partial trapping at Olha)
##   = D_12[t,g] except for the first years (until 1991)
## Dum_12[t,g]: Annual number of dead fish without a mark at Olha per breeding category. Start in 1992 (data no longer used since changes made for accounting for partial trapping at Olha)
##
## A_11[t,g]: Annual number of fish caught by angling between Ascain and Uxondoa per breeding category
## A_12[t,g]: Annual number of fish caught by angling between Uxondoa and Olha per breeding category
## Am_12[t,g]: Annual number of marked fish caught by angling between Uxondoa and Olha per breeding category (data no longer used since changes made for accounting for partial trapping at Olha)
##   = A_12[t,g] except for the first years (until 1991)
## Aum_12[t,g]: Annual number of unmarked fish caught by angling between Uxondoa and Olha per breeding category (data no longer used since changes made for accounting for partial trapping at Olha)
##
## R_EF[t,g]: Annual number of fish removed for experiment by electric fishing between Uxondoa and Olha per breeding category (from 1984 to 1991)
## M_EF[t,g]: Annual number of fish removed between Uxondoa and Olha and moved upstream Olha per breeding category (1990 & 1991)
##
## C_U[t,g]: Annual number of fish captured at Uxondoa per breeding category
## Cm_U[t,g]: Annual number of fish captured at Uxondoa and released with mark per breeding category = C_U[t,g] except for 1993 and 1994
## Cum_U[t,g]: Annual number of fish captured at Uxondoa but released unmarked (happened only in 1993 and 1994) per breeding category
##
## Cm_EF[t,g]: Annual number of marked fish captured by EF, angling or found dead between Uxondoa and Olha per breeding category: 1984 to 1991
## Cum_EF[t,g]: Annual number of unmarked fish captured by EF, angling or found dead between Uxondoa and Olha per breeding category: 1984 to 1991
##
## nm_2[t,g]: Annual number of marked fish captured at Olha per breeding category (when trapping is total, globally from 92 to 2011)
## num_2[t,g]: Annual number of unmarked fish captured at Olha per breeding category (when trapping is total, globally from 92 to 2011)
##
## Cm_O[t,g]: Annual number of marked fish captured at Olha per breeding category since 2012 (partial trapping)
## Cum_O[t,g]: Annual number of unmarked fish captured at Olha per breeding category since 2012 (partial trapping)
##
## e_2[t,g]: annual breeding escapement in the high catchment zone (UN + LUR) per breeding category
##    = number of fish released upstream Olha = NA until 1989, in 2000 and since 2012
## NB[t,g]: annual breeding escapement per breeding category in the high catchment zone added or removed in the HC zone = sum of the number of fish removed from the population and the number of fish released upstream Olah. Happened in 2000 and since 2012.
##
## Q[t]: annual mean December flow
##
## eff_Ux[t]: ratio of the annual number of nights of trapping since 2012 (partial trapping) over the mean number of nights of trapping in Uxondoa from 1984 to 2011 (302 nights)
## eff_Ol[t] : ratio of the annual number of nights of trapping since 2012 (partial trapping) and for year 2000 over the mean number of nights of trapping in Olha from 1994 to 2011 (294 nights, year 1992, 1993 and 2000 are excluded)
##
## OMEGA[t,z]: annual redd count per zone. 1/ LN1 (from Ascain to Uxondoa), 2/ LN2 (from Uxondoa to Olha), 4/ UN, 5/ LUR /!\ Zone 3 does not exist!
##
## ech_1.1SW[t]: annual number of captured individuals 1R/1SW
## ech_1SW_tot[t]: annual number of captured individuals 1SW (sum of 1R/1SW and 2R/1SW)
## ech_MSW_tot[t]: annual number of captured individuals MSW (1R/2SW, 2R/2SW, 1R3SW, 2R3SW, second spawning)
## ech_MSW[,1]: annual number of captured individuals 1R/2SW
## ech_MSW[,2]: annual number of captured individuals 2R/2SW
## ech_MSW[,3]: annual number of captured individuals 1R3SW
## ech_MSW[,4]: annual number of captured individuals 2R3SW
## ech_MSW[,5]: annual number of captured individuals second spawning
######################################################################################################
#######################################################################################################

#################################################################################
#################################################################################
## HYPERPRIORS ON PROBABILITIES
## ------
#################################################################################
###Shape and rate of the hyperprior on the precisions (modified by Buoro & Prévost March 2020)
mean_var ~ dchisqr(3)
mean_prec <- 1/mean_var
rate_prec ~ dgamma(0.1,0.1)
shape_prec <- mean_prec * rate_prec
### Mean and standard deviation of the probability to stay in LN1
mup_11_1 ~ dbeta(1,1) ; sigmap_11_1 <- sqrt(1/precp_11_1) ; precp_11_1 ~ dgamma(shape_prec,rate_prec)  # from 1984 to 1991
mup_11_2 ~ dbeta(1,1) ; sigmap_11_2 <- sqrt(1/precp_11_2) ; precp_11_2 ~ dgamma(shape_prec,rate_prec) # from 1992 to now on

### Mean and standard deviation of the probabilities to be captured at Uxondoa. Depend on sea age.
### Standard deviation of the probabilities to be captured at Olha when trapping is not continuous  
for (a in 1:2) {
  mupi_U[a] ~ dbeta(1,1) ; sigmapi_U[a] <- sqrt(1/precpi_U[a]) ; precpi_U[a] ~ dgamma(shape_prec,rate_prec)
  d_pi_U[a] ~ dunif(-5,5) # diffrential in the probability of capture since 2012
  } ## End of loop over fish ages
  sigmapi_Ol <- sqrt(1/precpi_Ol) ; precpi_Ol ~ dgamma(shape_prec,rate_prec)
  d_pi_Ol ~ dunif(-5,5) # diffrential in the probability of capture since 2012
### Mean and standard deviation of the probabilities to be Re-captured by EF, angling or found dead
mupi_EF ~ dbeta(1,1) ; sigmapi_EF <- sqrt(1/precpi_EF) ; precpi_EF ~ dgamma(shape_prec,rate_prec)

### Mean and standard deviation of the probabilities to move from LN2 (not stay in LN2). Depends on breeding categories.
  for (g in 1:4) {
  mup_n12[g] ~ dbeta(1,1) ; sigmap_12[g] <- sqrt(1/precp_12[g]) ; precp_12[g] ~ dgamma(shape_prec,rate_prec)
  } ## End of loop over breeding categories

### Mean and standard deviation of the probabilities to stay in UN
mup_21 ~ dbeta(1,1) ; sigmap_21 <- sqrt(1/precp_21) ; precp_21 ~ dgamma(shape_prec,rate_prec)

### Test for
## 1/ DIFFERENCES in probabilities to move from LN2
diff_12[1] <- mup_n12[1]-mup_n12[2] # Male 1SW vs Female 1SW
diff_12[2] <- mup_n12[1]-mup_n12[3] # Male 1SW vs Male MSW
diff_12[3] <- mup_n12[1]-mup_n12[4] # Male 1SW vs Female MSW
diff_12[4] <- mup_n12[2]-mup_n12[3] # Female 1SW vs Male MSW
diff_12[5] <- mup_n12[2]-mup_n12[4] # Female 1SW vs Female MSW
diff_12[6] <- mup_n12[3]-mup_n12[4] # Male MSW vs Female MSW

# step = 1 when diff >= 0 and step = 0 when diff < 0
test_p_12[1] <- step(diff_12[1]) 
test_p_12[2] <- step(diff_12[2])
test_p_12[3] <- step(diff_12[3]) 
test_p_12[4] <- step(diff_12[4])
test_p_12[5] <- step(diff_12[5]) 
test_p_12[6] <- step(diff_12[6])

#################################################################################
################################################################################
## PROBABILITY OF SPATIAL DISTRIBUTION AND CAPTURE
## -------------------------
## p_11_1[t]: annual probability to stay in LN1 from 1984 to 1991
## p_11_2[t]: annual probability to stay in LN1 since 1992
## p_n12[t,g]: annual probability to move from LN2 since 1992 given breeding category
## p_21[t]: annual probability to stay in UN since 1990
##
## pi_U[t,a]: annual probability to be captured at Uxondoa since 1984 given sea age (1SW vs MSW)
## pi_EF[t]: annual probability to be recaptured by electric fishing, angling or be found dead from 1984 to 1991
################################################################################
#################################################################################
### Probability to stay in LN1 (Time dependent). Distinction between before and after 1992
logit_mup_11_1 <- log(mup_11_1/(1-mup_11_1)) # logit transformation

for (t in 1:8) { ## p_11_1: Exchangeable from 1984 to 1991
  logit_p_11_1[t] ~ dnorm(logit_mup_11_1,precp_11_1)
  p_11_1[t] <- exp(logit_p_11_1[t])/(1+exp(logit_p_11_1[t]))  # back-transformation on the probability scale
  } ## End of loop over years

logit_mup_11_2 <- log(mup_11_2/(1-mup_11_2)) # logit transformation

for (t in 9:Y) { ## p_11_2: Exchangeable from 1992 to now on
  logit_p_11_2[t] ~ dnorm(logit_mup_11_2,precp_11_2)
  p_11_2[t] <- exp(logit_p_11_2[t])/(1+exp(logit_p_11_2[t])) 
  } ## End of loop over years

########################################################
### Probabilities to be captured at Uxondoa (time and sea age dependent)
for (a in 1:2) {
  logit_mupi_U[a] <- log(mupi_U[a]/(1-mupi_U[a])) # logit transformation
   
  for (t in 1:28) { ## pi_U: Exchangeable from 1984 to 2011
    logit_pi_U[t,a] ~ dnorm(logit_mupi_U[a],precpi_U[a])
    pi_U[t,a] <- exp(logit_pi_U[t,a])/(1+exp(logit_pi_U[t,a]))  # back-transformation on the probability scale
    eps_U[t,a] <- (logit_pi_U[t,a] - logit_mupi_U[a]) / sigmapi_U[a] # standardized residuals
    } ## End of loop over years

  for (t in 29:Y) { ## from 2012 to now. Partial trapping
     pi_U_mupi[t,a] <- exp(logit_mupi_U[a])/(1+exp(logit_mupi_U[a]))  # back-transformation on the probability scale    
     pi_U_eff[t,a] <- pi_U_mupi[t,a] *  eff_Ux[t]  # eff_Ux is a ratio (data)
 #logit transformation of the probability of capture with a systematic diffrential
     lpi_U[t,a] <- log(pi_U_eff[t,a]/(1-pi_U_eff[t,a]))+d_pi_U[a]        
     logit_pi_U[t,a] ~ dnorm(lpi_U[t,a],precpi_U[a])
     pi_U[t,a] <- exp(logit_pi_U[t,a])/(1+exp(logit_pi_U[t,a])) # back-transformation on the probability scale 
     eps_U[t,a] <- (logit_pi_U[t,a] - lpi_U[t,a]) / sigmapi_U[a] # standardized residuals
     } ## End of loop over years
  } ## End of loop over fish ages

######################################        
### Probabilities to be Re-captured by EF, angling or found dead (time dependent)
logit_mupi_EF <- log(mupi_EF/(1-mupi_EF)) # logit transformation
  
for (t in 1:8) { ## pi_EF: Exchangeable from 1984 to 1991
  logit_pi_EF[t] ~ dnorm(logit_mupi_EF,precpi_EF)
  pi_EF[t] <- exp(logit_pi_EF[t])/(1+exp(logit_pi_EF[t]))  # back-transformation on the probability scale
  } ## End of loop over years

###################
### Probabilities to move from LN2 (not stay in LN2) (depend on time and breeding categories). /!\ only possible since 1992
for (g in 1:4) {
  logit_mup_n12[g] <- log(mup_n12[g]/(1-mup_n12[g])) # logit transformation
  
  for (t in 9:Y) { ## 1_p_12: Exchangeable from 1992 to now on
    logit_p_n12[t,g] ~ dnorm(logit_mup_n12[g],precp_12[g])
    p_n12[t,g] <- exp(logit_p_n12[t,g])/(1+exp(logit_p_n12[t,g]))  # back-transformation on the probability scale
    eps_12[t,g] <- (logit_p_n12[t,g] - logit_mup_n12[g]) / sigmap_12[g] # standardized residuals
    } ## End of loop over years
  } # end of loop over breeding categories
  
##############################################
### Probabilities to stay in UN  (time dependent). /!\ Possible since 1990 and not 1992 because some spawners were released up from Olha
logitp_21 <- log(mup_21/(1-mup_21)) # logit transformation
  
for (t in 7:Y) { ## p_21: Exchangeable from 1990 to now on (Spawners in HC)
  logit_p_21[t] ~ dnorm(logitp_21,precp_21)
  p_21[t] <- exp(logit_p_21[t])/(1+exp(logit_p_21[t])) # back-transformation on the probability scale
  } ## End of loop over years

######################################################################################
######################################################################################
## NUMBERS PER GROUP
## |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## n_tot[t]: annual total number of fish                     
## n_1SW[t]: annual Number of males and females 1SW  
## n_MSW[t]: annual Number of males and females MSW
## lambda[t]: annual mean population size (mean of Poisson distribution)
## Plambda[t,g]: annual mean proportion of breeding category
## -> We provided changes on how to estimate lambda_n. We included a hierarchy on breeding category.
####################################################  
## PRIORS
## ------
####################################################################################
## Annual returns
for (t in 1:Y) { 
  n_tot[t] <- sum(n[t,])    # total number
  n_1SW[t] <- sum(n[t,1:2]) # Number of males and females 1SW
  n_MSW[t] <- sum(n[t,3:4]) # Number of males and females MSW 
  } ## End of loop over years

# Shape and rate parameter for gamma distribution  for negative binomial (see Gelman, 2d edition, p446)
shape_lambda ~ dgamma(0.005,0.005)
rate_lambda ~ dgamma(0.005,0.005)

# Prior on the parameters of the Dirichlet
for (g in 1:4) {
     s[g] ~ dexp(0.25)T(0.02,)
    } # end of loop over breeding categories
    
#To be able to initiate lambda_tot ensuite
lambda_tot0 ~ dgamma(shape_lambda,rate_lambda)
Plambda0[1:4] ~ ddirich(s[])

# Prior for lambda_tot and Plambda
for (t in 1:Y) {
    lambda_tot[t] ~ dgamma(shape_lambda,rate_lambda) # annual mean population size 
    Plambda[t,1:4] ~ ddirich(s[]) # proportion of breeding categories
    } # end of loop over years

### lambda_n is calculated by smolt cohort (proportion and mean population size of MSW is one year yearlier than 1SW)
## Firt year for MSW
lambda_n[1,3] <- lambda_tot0 * Plambda0[3]
lambda_n[1,4] <- lambda_tot0 * Plambda0[4]

for (t in 1:Y-1) { 
    lambda_n[t,1]  <- lambda_tot[t] * Plambda[t,1]
    lambda_n[t,2]  <- lambda_tot[t] * Plambda[t,2]
    lambda_n[t+1,3] <- lambda_tot[t] * Plambda[t,3]
    lambda_n[t+1,4] <- lambda_tot[t] * Plambda[t,4]
    } #end of loop over years

## Last year for 1SW
lambda_n[Y,1] <- lambda_tot[Y] * Plambda[Y,1] 
lambda_n[Y,2] <- lambda_tot[Y] * Plambda[Y,2] 

for (g in 1:4) {
  for (t in 1:Y) {
    n[t,g] ~ dpois(lambda_n[t,g]) # annual number of fish per breeding category
    } ## End of loop over years

######################################
## BETWEEN ASCAIN AND UXONDOA (LN1)
## --------------------------------
## n_11[t,g] : annual number of fish per breeding category staying in LN1
## n_n11[t,g]: annual number of fish per breeding category not staying in LN1 (migrating upstream)
## e_11_tot[t]: annual escapement for LN1
#############################################
### FISH NUMBER STAYING IN LN1 (n_11)
  for (t in 1:8) { ## 1984 -> 1991 (p_LN1_1 exchangeable before 1992)
    lim_11[t,g] <- D_11[t,g] + A_11[t,g]
    n_11[t,g] ~ dbin(p_11_1[t],n[t,g])I(lim_11[t,g],)
    } ## End of loop over years
    
  for (t in 9:Y) { ## 1992 to now on (p_LN1_2 exchangeable after 1992)
    lim_11[t,g] <- D_11[t,g] + A_11[t,g]
    n_11[t,g] ~ dbin(p_11_2[t],n[t,g])I(lim_11[t,g],)
    } ## End of loop over years

### BREEDING ESCAPEMENT (e_11) & FISH MIGRATING UPSTREAM UXONDOA(n_n11)
  for (t in 1:Y) { # from 1984 to now on
    e_11[t,g] <- n_11[t,g] - D_11[t,g] - A_11[t,g]
    n_n11[t,g] <- n[t,g] - n_11[t,g]
    } ## End of loop over years
  } ## End of loop over breeding category
  
for (t in 1:Y) {
  e_11_tot[t] <- sum(e_11[t,])
  } ## End of loop over years

#################################
## CAPTURE AT UXONDOA
## -------
## n_um[t,g]: Annual number of fish not captured (not marked) at Uxondoa per breeding category
##################################
## Likelihood on trapped fish number
for (t in 1:Y) { # from 1984 to now on
  for (g in 1:2) { ## Grisle
    C_U[t,g] ~ dbin(pi_U[t,1],n_n11[t,g])
    } ## End of loop over fish 1SW
    
  for (g in 3:4) { ## MSW
    C_U[t,g] ~ dbin(pi_U[t,2],n_n11[t,g])
    } ## End of loop over fish MSW
  } ## End of loop over years

## Fish number avoiding the trap, not marked fish (n_um): 
for (g in 1:4) {
  for (t in 1:9) { # from 1984 to 1992
    n_um[t,g] <- n_n11[t,g] - C_U[t,g]
    } ## End of loop over years
    
  # Year 1993 and 1994. Some individuals were captured but released unmarked 
  n_um[10,g] <- n_n11[10,g] - C_U[10,g] + Cum_U[10,g]
  n_um[11,g] <- n_n11[11,g] - C_U[11,g] + Cum_U[11,g]
  
  for (t in 12:Y) { ## from 1995 to now on
    n_um[t,g] <- n_n11[t,g] - C_U[t,g]
    } ## End of loop over years

#####################################################
## BETWEEN UXONDOA AND OLHA
## ------------------------
## n_12[t,g] : annual number of fish per breeding category staying in LN2 since 1992
## e_12_tot[t] : annual escapement for LN2
####################################################
## BEFORE 1992 = CAPTURE BY EF, ANGLING OR DEAD (Cm_EF or Cum_EF)
## Marked fish
  for (t in 1:4) {  # from 1984 to 1987
    Cm_EF[t,g] ~ dbin(pi_EF[t],Cm_U[t,g])
    } ## End of loop over years
    
  for (t in 6:8) { # from 1989 to 1991
    Cm_EF[t,g] ~ dbin(pi_EF[t],Cm_U[t,g])
    } ## End of loop over years

## Unmarked fish    
  for (t in 1:8) { # from 1984 to 1991
    Cum_EF[t,g] ~ dbin(pi_EF[t],n_um[t,g])
    } ## End of loop over years
  } ## End of loop over breeding category
  
## Year 1988 for marked fish. No marked male MSW was captured by EF, angling or found death (Cm_U[5,3] = 0)
for (g in 1:2) { 
  Cm_EF[5,g] ~ dbin(pi_EF[5],Cm_U[5,g])
  } ## End of loop over 1SW fish
  
Cm_EF[5,4] ~ dbin(pi_EF[5],Cm_U[5,4]) # for female MSW

## Breeding escapement (e_12) per breeding category
for (g in 1:4) { 
  for (t in 1:6) {
    e_12[t,g] <- Cm_U[t,g] + n_um[t,g] - D_12[t,g] - A_12[t,g] - R_EF[t,g]
    } ## End of loop over years
  for (t in 7:8) {
    e_12[t,g] <- Cm_U[t,g] + n_um[t,g] - D_12[t,g] - A_12[t,g] - R_EF[t,g] - M_EF[t,g]
    } ## End of loop over years

##################################
## AFTER 1992 = RECAPTURE AT OLHA  
## Fish number staying in LN2 (n_12) since 1992 per breeding category
  for (t in 9:Y) { # from 1992 to now on 
      n_12[t,g] <- Cm_U[t,g] + n_um[t,g] - nm_2[t,g] - num_2[t,g]
      } # end of loop over years
         
## Breeding escapement (e_12) per breeding category
  for (t in 9:Y) { # from 1992 to now on
    e_12[t,g] <- n_12[t,g]-D_12[t,g]-A_12[t,g]
    } ## End of loop over years
  } ## End of loop over breeding category
  
## Annual escapement (no more per breeding category)  
for (t in 1:Y) {
  e_12_tot[t] <- sum(e_12[t,])
  } ## End of loop over years

########################### 
## CAPTURE AT OLHA
## ----
## Likelihood on trapped fish number
## 1/ Number of fish passing in HC zone = number of fish captured at Olah until 2011 except few years
################################
## Male or female 1SW

# In 2000 (17) and from 2012 (29) trapping is not continuous at Olha
# Note : Contrary to Uxondoa, probaility of capture at Olha is 1 when there is no interruption of trapping      
     lpi_Ol[17] <- log(eff_Ol[17]/(1-eff_Ol[17])) # logit transformation of eff_Ux which is a ratio (data)
     logit_pi_Ol[17] ~ dnorm(lpi_Ol[17],precpi_Ol)
     pi_Ol[17]<- exp(logit_pi_Ol[17])/(1+exp(logit_pi_Ol[17])) # back-transformation on the probability scale 
     eps_Ol[17] <- (logit_pi_Ol[17] - lpi_Ol[17]) / sigmapi_Ol # standardized residuals
  for (t in 29:Y) {
# logit transformation of probability of capture at Olha wher eff_Ux which is a ratio (data) and d_pi_Ol is a systematic differential    
     lpi_Ol[t] <- log(eff_Ol[t]/(1-eff_Ol[t]))+d_pi_Ol
     logit_pi_Ol[t] ~ dnorm(lpi_Ol[t],precpi_Ol)
     pi_Ol[t]<- exp(logit_pi_Ol[t])/(1+exp(logit_pi_Ol[t])) # back-transformation on the probability scale 
     eps_Ol[t] <- (logit_pi_Ol[t] - lpi_Ol[t]) / sigmapi_Ol # standardized residuals
     }

for (g in 1:2) { 
  for (t in 9:16) { # from 1992 to 1999
      nm_2[t,g] ~ dbin(p_n12[t,g],Cm_U[t,g])
      num_2[t,g] ~ dbin(p_n12[t,g],n_um[t,g])
      } # end of loop over years
  
  # Year 2000 (reduced trapping effort)
      nm_2[17,g] ~ dbin(p_n12[17,g],Cm_U[17,g])
      num_2[17,g] ~ dbin(p_n12[17,g],n_um[17,g])
      Cm_O[17,g] ~ dbin(pi_Ol[17],nm_2[17,g]) # eff_Ol[t] is a ratio (data)
      Cum_O[17,g] ~ dbin(pi_Ol[17],num_2[17,g])
  
  for (t in 18:28) { # from 2001 to 2011
      nm_2[t,g] ~ dbin(p_n12[t,g],Cm_U[t,g])
      num_2[t,g] ~ dbin(p_n12[t,g],n_um[t,g])
      } # end of loop over years  
  
  for (t in 29:Y) { # from 2012 to now on (reduced trapping effort)
      nm_2[t,g] ~ dbin(p_n12[t,g],Cm_U[t,g])
      num_2[t,g] ~ dbin(p_n12[t,g],n_um[t,g])
      Cm_O[t,g] ~ dbin(pi_Ol[t],nm_2[t,g])
      Cum_O[t,g] ~ dbin(pi_Ol[t],num_2[t,g])
      } # end of loop over years
  } ## End of loop over 1SW breeding category

##########################
# Trapped marked male MSW number  (some years no marked male MSW was captured)
# Year 1992
nm_2[9,3] ~ dbin(p_n12[9,3],Cm_U[9,3])

# Between 1994 and 1997
for (t in 11:14) {
  nm_2[t,3] ~ dbin(p_n12[t,3],Cm_U[t,3])
  } ## End of loop over years

# Year 1999  
nm_2[16,3] ~ dbin(p_n12[16,3],Cm_U[16,3])

# Year 2000
nm_2[17,3] ~ dbin(p_n12[17,3],Cm_U[17,3])
Cm_O[17,3] ~ dbin(pi_Ol[17],nm_2[17,3])

# Between 2001 and 2004
for (t in 18:21) {
  nm_2[t,3] ~ dbin(p_n12[t,3],Cm_U[t,3])
  } ## End of loop over years
  
# Between 2006 and 2008
for (t in 23:25) {
  nm_2[t,3] ~ dbin(p_n12[t,3],Cm_U[t,3])
  } ## End of loop over years

# 2010 and 2011
for (t in 27:28) {
  nm_2[t,3] ~ dbin(p_n12[t,3],Cm_U[t,3])
  } ## End of loop over years

# from 2012 to now on (reduced trapping effort)
for (t in 29:Y) {
    nm_2[t,3] ~ dbin(p_n12[t,3],Cm_U[t,3])
    Cm_O[t,3] ~ dbin(pi_Ol[t],nm_2[t,3])
    } ## End of loop over years 
  
##########################
# Trapped unmarked male MSW number  
# From 1992 to 1999 
for (t in 9:16) {
    num_2[t,3] ~ dbin(p_n12[t,3],n_um[t,3])
    } ## End of loop over years

# Year 2000 (reduced trapping effort)
num_2[17,3] ~ dbin(p_n12[17,3],n_um[17,3]) 
Cum_O[17,3] ~ dbin(pi_Ol[17],num_2[17,3])

for (t in 18:28) { # from 2001 to 2011
    num_2[t,3] ~ dbin(p_n12[t,3],n_um[t,3])
    } # end of loop over years 

for (t in 29:Y) { # from 2012 to now on (reduced trapping effort)
    num_2[t,3] ~ dbin(p_n12[t,3],n_um[t,3]) 
    Cum_O[t,3] ~ dbin(pi_Ol[t],num_2[t,3])
    } ## End of loop over years
  
##########################
# Trapped female MSW number (marked or unmarked) 
# From 1992 to 1999
for (t in 9:16) {
    nm_2[t,4] ~ dbin(p_n12[t,4],Cm_U[t,4])
    num_2[t,4] ~ dbin(p_n12[t,4],n_um[t,4])
    } ## End of loop over years

# Year 2000 (reduced trapping effort)
nm_2[17,4] ~ dbin(p_n12[17,4],Cm_U[17,4])
num_2[17,4] ~ dbin(p_n12[17,4],n_um[17,4]) 
Cm_O[17,4] ~ dbin(pi_Ol[17],nm_2[17,4])
Cum_O[17,4] ~ dbin(pi_Ol[17],num_2[17,4])

for (t in 18:28) { # from 2001 to 2011
    nm_2[t,4] ~ dbin(p_n12[t,4],Cm_U[t,4])
    num_2[t,4] ~ dbin(p_n12[t,4],n_um[t,4])
    } ## End of loop over years

for (t in 29:Y) { # from 2012 to now on
  nm_2[t,4] ~ dbin(p_n12[t,4],Cm_U[t,4])
  num_2[t,4] ~ dbin(p_n12[t,4],n_um[t,4])  
  Cm_O[t,4] ~ dbin(pi_Ol[t],nm_2[t,4])
  Cum_O[t,4] ~ dbin(pi_Ol[t],num_2[t,4])
  } ## End of loop over years  

##############################
## UPSTREAM OLHA
## -------------
## e_2[t]: annual escapment for HC zone. Unknown in 2000 and since 2012
## e_21_tot[t]: annual escapement for UN
## e_22_tot[t]: annual escapement for LUR 
###################################
# Annual escapement per breeding category and per zone (UN and LUR)
for (g in 1:4) { # breeding category
  e_2[17,g] <- num_2[17,g] + nm_2[17,g] + NB[17,g]
  
  for (t in 29:Y) { # from 2012 to now on
        e_2[t,g] <- num_2[t,g] + nm_2[t,g] + NB[t,g]
        } # end of loop over years
  } # end of loop over breeding category
  
for (g in 1:4) { # breeding category  
  # From 1990 to 2003
  for (t in 7:20) {
    e_21[t,g] ~ dbin(p_21[t],e_2[t,g]) 
    e_22[t,g] <- e_2[t,g] - e_21[t,g]
    } ## End of loop over years
    
  # Year 2006 & 2007  
  for (t in 23:24) {
    e_21[t,g] ~ dbin(p_21[t],e_2[t,g]) 
    e_22[t,g] <- e_2[t,g] - e_21[t,g]
    } ## End of loop over years
  
  #From 2010 to now on  
  for (t in 27:Y) {
    e_21[t,g] ~ dbin(p_21[t],e_2[t,g]) 
    e_22[t,g] <- e_2[t,g] - e_21[t,g]
    } ## End of loop over years
  } ## End of loop over breeding category

# Year 2004 & 2005, male and female 1SW  
for (t in 21:22) {
  for (g in 1:2) {
    e_21[t,g] ~ dbin(p_21[t],e_2[t,g]) 
    e_22[t,g] <- e_2[t,g] - e_21[t,g]
    } ## End of loop over male and female 1SW

   # Male MSW
   e_21[t,3] <- 0
   e_22[t,3] <- 0 
   
   # Female MSW   
   e_21[t,4] ~ dbin(p_21[t],e_2[t,4]) 
   e_22[t,4] <- e_2[t,4] - e_21[t,4]
   } ## End of loop over years

# Year 2008 & 2009, male and female 1SW  
for (t in 25:26) {
  for (g in 1:2) {
    e_21[t,g] ~ dbin(p_21[t],e_2[t,g]) 
    e_22[t,g] <- e_2[t,g] - e_21[t,g]
    } ## End of loop over breeding category
    
  #Male MSW
  e_21[t,3] <- 0
  e_22[t,3] <- 0
  
  #Female MSW  
  e_21[t,4] ~ dbin(p_21[t],e_2[t,4]) 
  e_22[t,4] <- e_2[t,4]-e_21[t,4]
  } ## End of loop over years

###########################################
# Annual escapement (not anymore per breeding category)
for (t in 7:Y) { # from 1990 to now on
  e_21_tot[t] <- sum(e_21[t,]) ; e_22_tot[t] <- sum(e_22[t,])
  } ## End of loop over years

#################################################################################
######################################################################
## REDDS
## |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## PRIORS
## ------
##############################################################################
# Proportionality coefficient per zone (lower Nivelle vs High catchment)
k_1 ~ dgamma(0.01,0.01)
k_1_cut <- cut(k_1)

k_2 ~ dgamma(0.01,0.01) 
k_2_cut <- cut(k_2)

# Fixed river flow effect
rho ~ dgamma(0.01,0.01) 
rho_cut <- cut(rho)

# Shape and scale parameter for year effect per zone  
eta_1 ~ dgamma(0.01,0.01) 
eta_2 ~ dgamma(0.01,0.01)

##################################
## PROBABILITY DISTRIBUTIONS
## -------------------------
## alpha_1[t]: annual effect for Lower Nivelle
## alpha_2[t]: annual effect for High Catchement zone (since 1990)
## RPF[t,z]: annual mean redd number per female per zone (LN vs High Catchment)
##################################
#################################
# Annual effect. Only since 1990 for High Catchement zone
for (t in 1:Y) {
  alpha_1[t] ~ dgamma(eta_1,eta_1)
  } ## End of loop over years
  
for (t in 7:Y) { # from 1990 to now on
  alpha_2[t] ~ dgamma(eta_2,eta_2)
  } ## End of loop over years

#######################################
## Annual Mean redd number per female for Lower Nivelle
for (t in 1:Y) { 
  RPF[t,1] <- k_1_cut*pow(rho_cut,Q[t]) 

  # Annual mean parameter for poisson distribution of redd count 
  lambda_OMEGA[t,1] <- k_1*pow(rho,Q[t])*alpha_1[t]*(e_11[t,2]+e_11[t,4]) # between Ascain and Uxondoa (LN1)
  lambda_OMEGA[t,2] <- k_1*pow(rho,Q[t])*alpha_1[t]*(e_12[t,2]+e_12[t,4]) # between Uxundoa and Olha (LN2)

  # Annual redd count data for LN1 and LN2
  for (z in 1:2) {
    OMEGA[t,z] ~ dpois(lambda_OMEGA[t,z])
    } ## End of loop over zones
  } ## End of loop over years

################################
## Annual Mean redd number per female for High Catchement
for (t in 7:Y) { # from 1990 to now on 
  RPF[t,2] <- k_2_cut*pow(rho_cut,Q[t])

  # Annual mean parameter for poisson distribution of redd count /!\ lambda_OMEGA[t,3] does not exist
  lambda_OMEGA[t,4] <- k_2*pow(rho,Q[t])*alpha_2[t]*(e_21[t,2]+e_21[t,4])
  lambda_OMEGA[t,5] <- k_2*pow(rho,Q[t])*alpha_2[t]*(e_22[t,2]+e_22[t,4])
  
  # Annual redd count data for UN and LUR /!\ OMEGA[t,3] does not exist
  for (z in 4:5) {
    OMEGA[t,z] ~ dpois(lambda_OMEGA[t,z])
    } ## End of loop over zones
  } ## End of loop over years

########################################################
########################################################
# Breeding escapment per breeding category and per zone
# -----------------------------------------------------
# e_1SW[t]: annual escapement for male and female 1SW
# e_MSW[t]: annual escapement for male and female MSW
# e_1SW_F[t]: annual escapement for female 1SW
# e_MSW_F[t]: annual escapement for female MSW
# eggs_11[t]: annual number of eggs produced between Ascain and Uxondoa (LN1): fixed number of 4500 eggs/female 1SW and 7200 eggs/female MSW 
# eggs_12[t]: annual number of eggs produced between Uxondoa and Olha (LN2)
# eggs_21[t]: annual number of eggs produced in Upper Nivelle
# eggs_22[t]: annual number of eggs produced in Lurgorrieta
# eggs_tot[t]:annual number of eggs produced
########################################################
for (t in 1:6) { # from 1984 to 1990
  e_1SW[t] <- sum(e_11[t,1:2]) + sum(e_12[t,1:2])
  e_MSW[t] <- sum(e_11[t,3:4]) + sum(e_12[t,3:4])
  
  e_1SW_F[t] <- e_11[t,2] + e_12[t,2]
  e_MSW_F[t] <- e_11[t,4] + e_12[t,4]
  
  eggs_11[t] <- 4500*e_11[t,2]+7200*e_11[t,4]
  eggs_12[t] <- 4500*e_12[t,2]+7200*e_12[t,4]
  eggs_tot[t] <- 4500*e_1SW_F[t]+7200*e_MSW_F[t]
  } ## End of loop over years
    
for (t in 7:Y) { # from 1990 to now on 
  e_1SW[t] <- sum(e_11[t,1:2]) + sum(e_12[t,1:2]) + sum(e_2[t,1:2])
  e_MSW[t] <- sum(e_11[t,3:4]) + sum(e_12[t,3:4]) + sum(e_2[t,3:4])
  
  e_1SW_F[t] <- e_11[t,2] + e_12[t,2] + e_2[t,2]
  e_MSW_F[t] <- e_11[t,4] + e_12[t,4] + e_2[t,4]
  
  eggs_11[t] <- 4500*e_11[t,2]+7200*e_11[t,4]
  eggs_12[t] <- 4500*e_12[t,2]+7200*e_12[t,4]
  eggs_21[t] <- 4500*e_21[t,2]+7200*e_21[t,4]
  eggs_22[t] <- 4500*e_22[t,2]+7200*e_22[t,4]
  eggs_tot[t] <- 4500*e_1SW_F[t]+7200*e_MSW_F[t]
  } ## End of loop over years

##################################################
################################################
### Retour total par age de mer X age eau douce
## ----------------------------------------------
## n_1.1SW[t]: annual number of fish 1R/1SW (captured or not)
## n_2.1SW[t]: annual number of fish 2R/1SW (captured or not)
## n_1.2SW[t]: annual number of fish 1R/2SW (captured or not)  
## n_2.2SW[t]: annual number of fish 2R/2SW (captured or not) 
## n_1.3SW[t]: annual number of fish 1R/3SW (captured or not)  
## n_2.3SW[t]: annual number of fish 2R/3SW (captured or not)  
## n_PS[t]: annual number of fish spawning for the second time  
##################################################
## From 1984 to now on
for (t in 1:Y) {
# Modified in 2023 to separate the adults of wild origin in the returns in the first 6 years
# Wild origin : not issuing from smolts releases (wild reproduction or released as YOY)

for (t in 1:6) {

  ### Individuals 1SW

  ech_1.1SW[t] ~ dbin(p_1.1SW[t], ech_1SW_wild[t]) # Individuals 1R/1SW sampled
  
  n_1SW.cut[t] <- cut(n_1SW[t])
  
  ech_1SW_wild[t] ~ dbin(p_1SW_wild[t], ech_1SW_tot[t]) # Wild individuals among the 1SW sampled
  p_1SW_wild[t] ~ dbeta(1,1)
  
  no_ech_1SW[t] <- n_1SW.cut[t] - ech_1SW_tot[t] # Individuals 1SW not sampled
  no_ech_1SW_wild[t] ~ dbin(p_1SW_wild[t], no_ech_1SW[t]) # Individuals 1SW wild not sampled
  no_ech_1.1SW[t] ~ dbin(p_1.1SW[t], no_ech_1SW_wild[t]) # Individuals 1R/1SW not sampled 
  
  n_1.1SW[t] <- ech_1.1SW[t] + no_ech_1.1SW[t] # Total number of individuals 1SW
  n_2.1SW[t] <- ech_1SW_wild[t] - ech_1.1SW[t] + no_ech_1SW_wild[t] - no_ech_1.1SW[t] # Individuals 2R/1SW
  
  ### Individuals MSW
  ech_MSW[t,1:5] ~ dmulti(p_MSW[t,], ech_MSW_wild[t]) # Individuals MSW sampled
  
  n_MSW.cut[t] <- cut(n_MSW[t]) 
  
  ech_MSW_wild[t] ~ dbin(p_MSW_wild[t], ech_MSW_tot[t]) # Wild individuals among the 1SW sampled
  p_MSW_wild[t] ~ dbeta(1,1)
  
  no_ech_MSW_tot[t] <- n_MSW.cut[t]- ech_MSW_tot[t] # Individuals MSW not sampled
  no_ech_MSW_wild[t] ~ dbin(p_MSW_wild[t], no_ech_MSW[t]) # Individuals MSW wild not sampled
       
  no_ech_MSW[t,1] ~ dbin(p_MSW[t,1], no_ech_MSW_wild[t]) # Individuals 1R/2SW not sampled
  no_ech_MSW_r[t,1] <- no_ech_MSW_wild[t] - no_ech_MSW[t,1] # Individuals MSW minus individuals 1R/2SW
  
  p_MSW_r[t,1]<- p_MSW[t,2] / (1 - p_MSW[t,1]) # to ensure that the total sum of p_MSW[t,] is equal to one (use of multinomial function)
  no_ech_MSW[t,2] ~ dbin(p_MSW_r[t,1], no_ech_MSW_r[t,1]) # Individuals 2R/2SW not sampled 
  no_ech_MSW_r[t,2] <- no_ech_MSW_r[t,1] - no_ech_MSW[t,2] # Individuals MSW minus individuals (1R/2SW + 2R/2SW)
   
  p_MSW_r[t,2]<- p_MSW[t,3] / (1 - sum(p_MSW[t,1:2]))
  no_ech_MSW[t,3] ~ dbin(p_MSW_r[t,2], no_ech_MSW_r[t,2]) # Individuals 1R/3SW not sampled
  no_ech_MSW_r[t,3] <- no_ech_MSW_r[t,2] - no_ech_MSW[t,3] # Individuals MSW minus individuals (1R/2SW + 2R/2SW + 1R/3SW)
  
  p_MSW_r[t,3]<- p_MSW[t,4] / (1 - sum(p_MSW[t,1:3]))
  no_ech_MSW[t,4] ~ dbin(p_MSW_r[t,3], no_ech_MSW_r[t,3]) # Individuals 2R/3SW not sampled
   
  no_ech_MSW[t,5] <- no_ech_MSW_r[t,3] - no_ech_MSW[t,4]  # Individuals MSW minus individuals (1R/2SW + 2R/2SW + 1R/3SW + 2R/3SW)
   
  n_1.2SW[t] <- ech_MSW[t,1] + no_ech_MSW[t,1]
  n_2.2SW[t] <- ech_MSW[t,2] + no_ech_MSW[t,2]
  n_1.3SW[t] <- ech_MSW[t,3] + no_ech_MSW[t,3]
  n_2.3SW[t] <- ech_MSW[t,4] + no_ech_MSW[t,4]
  n_PS[t] <- ech_MSW[t,5] + no_ech_MSW[t,5]
  
  p_1.1SW[t] ~ dbeta(a_1.1SW, a_2.1SW) # annual probability to sample a 1R/1SW individual
  p_MSW[t,1:5] ~ ddirich(a_MSW[]) # annual probability to sample a MSW individual. 
  } # end of loop over years

for (t in 7:Y) {

  ### Individuals 1SW
  ech_1.1SW[t] ~ dbin(p_1.1SW[t], ech_1SW_tot[t]) # Individuals 1R/1SW sampled
  
  n_1SW.cut[t] <- cut(n_1SW[t])
  
  no_ech_1SW[t] <- n_1SW.cut[t] - ech_1SW_tot[t] # Individuals 1SW not sampled
  no_ech_1.1SW[t] ~ dbin(p_1.1SW[t], no_ech_1SW[t]) # Individuals 1R/1SW not sampled 
  
  n_1.1SW[t] <- ech_1.1SW[t] + no_ech_1.1SW[t] # Total number of individuals 1SW
  n_2.1SW[t] <- n_1SW.cut[t] - n_1.1SW[t] # Individuals 2R/1SW
  
  ### Individuals MSW
  ech_MSW[t,1:5] ~ dmulti(p_MSW[t,], ech_MSW_tot[t]) # Individuals MSW sampled
  
  n_MSW.cut[t] <- cut(n_MSW[t]) 
  
  no_ech_MSW_tot[t] <- n_MSW.cut[t]- ech_MSW_tot[t] # Individuals MSW not sampled
      
  no_ech_MSW[t,1] ~ dbin(p_MSW[t,1], no_ech_MSW_tot[t]) # Individuals 1R/2SW not sampled
  no_ech_MSW_r[t,1] <- no_ech_MSW_tot[t] - no_ech_MSW[t,1] # Individuals MSW minus individuals 1R/2SW
  
  p_MSW_r[t,1]<- p_MSW[t,2] / (1 - p_MSW[t,1]) # to ensure that the total sum of p_MSW[t,] is equal to one (use of multinomial function)
  no_ech_MSW[t,2] ~ dbin(p_MSW_r[t,1], no_ech_MSW_r[t,1]) # Individuals 2R/2SW not sampled 
  no_ech_MSW_r[t,2] <- no_ech_MSW_r[t,1] - no_ech_MSW[t,2] # Individuals MSW minus individuals (1R/2SW + 2R/2SW)
   
  p_MSW_r[t,2]<- p_MSW[t,3] / (1 - sum(p_MSW[t,1:2]))
  no_ech_MSW[t,3] ~ dbin(p_MSW_r[t,2], no_ech_MSW_r[t,2]) # Individuals 1R/3SW not sampled
  no_ech_MSW_r[t,3] <- no_ech_MSW_r[t,2] - no_ech_MSW[t,3] # Individuals MSW minus individuals (1R/2SW + 2R/2SW + 1R/3SW)
  
  p_MSW_r[t,3]<- p_MSW[t,4] / (1 - sum(p_MSW[t,1:3]))
  no_ech_MSW[t,4] ~ dbin(p_MSW_r[t,3], no_ech_MSW_r[t,3]) # Individuals 2R/3SW not sampled
   
  no_ech_MSW[t,5] <- no_ech_MSW_r[t,3] - no_ech_MSW[t,4]  # Individuals MSW minus individuals (1R/2SW + 2R/2SW + 1R/3SW + 2R/3SW)
   
  n_1.2SW[t] <- ech_MSW[t,1] + no_ech_MSW[t,1]
  n_2.2SW[t] <- ech_MSW[t,2] + no_ech_MSW[t,2]
  n_1.3SW[t] <- ech_MSW[t,3] + no_ech_MSW[t,3]
  n_2.3SW[t] <- ech_MSW[t,4] + no_ech_MSW[t,4]
  n_PS[t] <- ech_MSW[t,5] + no_ech_MSW[t,5]
  
  p_1.1SW[t] ~ dbeta(a_1.1SW, a_2.1SW) # annual probability to sample a 1R/1SW individual
  p_MSW[t,1:5] ~ ddirich(a_MSW[]) # annual probability to sample a MSW individual. 
  } # end of loop over years

## Prior for probabilities to sample an individual depending on number of sea winter and river winter  
a_1.1SW ~ dexp(0.25) T(0.02,)  # shape parameter of the beta distribution for p_1.1SW
a_2.1SW ~ dexp(0.25) T(0.02,)  # shape parameter of the beta distribution for p_1.1SW
a_MSW[1] ~ dexp(0.25) T(0.02,) # 1R/2SW
a_MSW[2] ~ dexp(0.25) T(0.02,) # 2R/2SW
a_MSW[3] ~ dexp(0.25) T(0.02,) # 1R/3SW
a_MSW[4] ~ dexp(0.25) T(0.02,) # 2R/3SW
a_MSW[5] ~ dexp(0.25) T(0.02,) # Second spawning

#######################################################################
########################################################################  
### Retour (total et par age de mer) par année de naissance (cohorte c)
## -----------------------------------------------------------------------
## c_1SW[t]: number of 1SW (either 1R or 2R)
## c_2SW[t]: annual number of 2SW (either 1R or 2R)
## c_3SW[t]: annual number of 3SW (either 1R or 2R)
## P_1SW[t]: annual proportion of 1SW
## P_MSW[t]: annual proportion of MSW
## /!\ First year of birth is 1982
for (t in 1:Y-3) { # (Y-3 because 3SW possible)
  c_1SW[t] <- n_1.1SW[t] + n_2.1SW[t+1]
  c_2SW[t] <- n_1.2SW[t+1] + n_2.2SW[t+2]
  c_3SW[t] <- n_1.3SW[t+2] + n_2.3SW[t+3]
  
  c_tot[t] <- c_1SW[t] + c_2SW[t] + c_3SW[t]
  
  P_1SW[t] <- c_1SW[t] / c_tot[t]
  P_MSW[t] <- 1 - P_1SW[t]
  } # end of loop over years
  
  c_1SW[Y-2] <- n_1.1SW[Y-2] + n_2.1SW[Y-1]
  c_2SW[Y-2] <- n_1.2SW[Y-1] + n_2.2SW[Y]
  c_3SW[Y-2] <- n_1.3SW[Y]
  c_tot[Y-2] <- c_1SW[Y-2] + c_2SW[Y-2] + c_3SW[Y-2]
  P_1SW[Y-2] <- c_1SW[Y-2] / c_tot[Y-2]
  P_MSW[Y-2] <- 1 - P_1SW[Y-2]
  
  c_1SW[Y-1] <- n_1.1SW[Y-1] + n_2.1SW[Y]
  c_2SW[Y-1] <- n_1.2SW[Y]
  c_tot[Y-1] <- c_1SW[Y-1] + c_2SW[Y-1]
  P_1SW[Y-1] <- c_1SW[Y-1] / c_tot[Y-1]
  P_MSW[Y-1] <- 1 - P_1SW[Y-1]
} ## Fin du model
