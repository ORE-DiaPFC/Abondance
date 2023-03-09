################################################################################
###           Model of CMR data to estimate spawners population size         ###
###                 of Salmo salar in Bresle river.                         ###
###                  Sabrina Servanty & Etienne Pr?vost                      ###
###                          January 2015                                  ###
################################################################################

model {


############################################################################################
## Difference with Sebastien's report (Delmotte et al. 2010):
## - set probability of capture at Eu in 2000 and 2001 to be smaller than the other years (partial trapping)
## - didn't consider recapture probability in 1989 and 1993 because trap in Beauchamps was not working those years (and so in 2000 and 2001 as in the report)
## - adding a flow effect in pi_Eu. Considering a different temporal window depending on sea age. Flow data are standardized (ln(Q[t]) - mean(ln(Q)))/sd(ln(Q)) within the model. Residuals are standardized and followed.
########### - 15 june - 31 august for 1SW
########### - 15 april - 30 june for MSW
## - adding another effect of flow corresponding to the second peak of migration.Flow data are standardized within the model.
########### Same temporal window for 1SW and MSW: 1 octobre - 30 november
## - calculating calculating R? (% of variation explained by the two covariates in the probability of capture at Eu)
###############################################################################################

############################################################################################
## Used indices:
## t: year; 1 to Y - from 1984 to Y  ## 
## a: sea age; 
##    1-1SW (Grisle), 
##    2-MSW (salmon)
#####################################################################################################

#####################################################################################################
## DATA
## --------------
##  Y: length of the time series (number of years)
##  C_Eu[t,a]: Annual number of fish captured at Eu per sea age category (1SW/MSW)
##  Cm_Eu[t,a]: Annual number of fish captured and marked at Eu per sea age category
##  Cm_B[t,a]: Annual number of marked fish captured at Beauchamps per sea age category
##  Cum_B[t,a]: Annual number of unmarked fish captured at Beauchamps per sea age category 
##  Q[t,a]: Annual mean flow associated to sea age category 
##  Q2pic[t]: Annual mean flow associated to the second peak of migration  
######################################################################################


  #################################################################################
  # PROBABILITIES (p)
  # ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  ## HYPER-PARAMETERS
  ## ------
  #################################################################################
  ### Mean and standard deviation of probabilities depending on sea age.
    # Probabilities to be captured at Eu.
  
  k <- 3 # k degree of freedom
  
  for (a in 1:2) {
    pi_Eu00[a] ~ dbeta(1,1)  # year 2000 is considered to be different (partial trapping)
    pi_Eu01[a] ~ dbeta(1,1)  # year 2001 is considered to be different (partial trapping)
    
    # from 2018, partial capture at traps (no trapping over weekend)
    for (y in 1:(Y-34)) {
        pi_EuYY[y,a] <- 5/7  # year 2018 is considered to be different (partial trapping); 5 nights fishing over 7 a week
    }
    
    logit_int_Eu[a] ~ dunif(-10,10)    #intercept 
    logit_flow_Eu[a] ~ dunif(-10,10) #slope for flow data (1SW at Eu: 15 june - 31 august, MSW: 15 april - 30 june)
    #sigmapi_Eu[a] ~ dunif(0,20)    
    sigmapi_Eu[a] <- sqrt(varpi_Eu[a])
    varpi_Eu[a]~dchisqr(k) # k degree of freedom
    
    lflow_fall_Eu[a] ~ dunif(-10,10) # slope for flow data, second peak of migration (same period for both 1SW and MSW)
        
    # Probability to be captured at Beauchamps (after reproduction)
    mupi_B[a] ~ dbeta(1,1)
    #sigmapi_B[a] ~ dunif(0,20)
    sigmapi_B[a] <- sqrt(varpi_B[a])
    varpi_B[a]~dchisqr(k) # k degree of freedom
  } # end loop a
  

                                 
  ################################################################################
  ## PROBABILITY DISTRIBUTIONS
  ## -------------------------
  ## pi_Eu[t,a]:annual probability to be captured at Eu given sea age
  ## pi_B[t,a]: annual probability to be captured at Beauchamps given sea age (after reproduction)
  ####################################################################################  
  ### Probabilities to be captured at Eu (time and sea age dependent)
  # mb-21.03.2022: now in data
  # for (t in 1:Y) {
  #    lQ2pic[t] <- log(Q2pic[t]) # ln transformation of autumn covariate
  #    stlQ2pic[t] <- (lQ2pic[t] - mean(lQ2pic[]))/sd(lQ2pic[]) # standardized covariate 
  #    } #end of loop over years
  
  for (a in 1:2) { 
    #varpi_Eu[a] <- (sigmapi_Eu[a])*(sigmapi_Eu[a]) 
    precpi_Eu[a] <- 1/(varpi_Eu[a]) # precision 
        
        for (t in 1:34) { #logit_pi_Eu exchangeable from 1984 to now on 
        
          # mb-21.03.2022: now in data
            #logQ[t,a] <- log(Q[t,a]) # ln transformation of covariate
           #stlogQ[t,a] <- (logQ[t,a] - mean(logQ[,a]))/sd(logQ[,a]) # standardized covariate
          
            logit_mupi_Eu[t,a] <- logit_int_Eu[a] + logit_flow_Eu[a] * stlogQ[t,a] + lflow_fall_Eu[a] * stlQ2pic[t] 
            logit_pi_Eu[t,a] ~ dnorm(logit_mupi_Eu[t,a],precpi_Eu[a])
            pi_Eu[t,a] <- exp(logit_pi_Eu[t,a])/(1+exp(logit_pi_Eu[t,a]))  # back-transformation on the probability scale
            epsilon_Eu[t,a] <- (logit_pi_Eu[t,a] - logit_mupi_Eu[t,a]) / sigmapi_Eu[a] # standardized residuals
            eps_Eu[t,a] <- logit_pi_Eu[t,a] - logit_mupi_Eu[t,a] # residuals not standardized
             } ## End of loop over years
    
        ## Partial trapping from 2018
        for (t in 35:Y) {  # from 2018 to now on
    
          # mb-21.03.2022: now in data
            #logQ[t,a] <- log(Q[t,a]) # ln transformation of covariate
            #stlogQ[t,a] <- (logQ[t,a] - mean(logQ[,a]))/sd(logQ[,a]) # standardized covariate
      
            logit_mupi_Eu[t,a] <- logit_int_Eu[a] + logit_flow_Eu[a] * stlogQ[t,a] + lflow_fall_Eu[a] * stlQ2pic[t] 
            logit_pi_Eu[t,a] ~ dnorm(logit_mupi_Eu[t,a],precpi_Eu[a])
            pi_Eu[t,a] <- (exp(logit_pi_Eu[t,a])/(1+exp(logit_pi_Eu[t,a])))* pi_EuYY[t-34,a]  # back-transformation on the probability scale * partial trapping
            epsilon_Eu[t,a] <- (logit_pi_Eu[t,a] - logit_mupi_Eu[t,a]) / sigmapi_Eu[a] # standardized residuals
            eps_Eu[t,a] <- logit_pi_Eu[t,a] - logit_mupi_Eu[t,a] # residuals not standardized
        } # end loop t
    
 
 # The following section is removed because R2 calculation is wrong and useless (March 2020)            
        #Calculating R? = 1 -(E(variance of residuals (/!\ not standardized!) / E(variance of capture probabilities)))
        # See Gelman & Pardoe 2006
        #sdeps_Eu[a] <- sd(eps_Eu[,a]) 
        #vareps_Eu[a] <- sdeps_Eu[a] * sdeps_Eu[a]
        
        #sdlpi_Eu[a] <- sd(logit_pi_Eu[,a])
        #varlpi_Eu[a] <- sdlpi_Eu[a] * sdlpi_Eu[a]
        
        #R2[a] <- 1 - (mean(vareps_Eu[a])/mean(varlpi_Eu[a])) 
    }# end of loop over sea age
                
   test[1] <- step(logit_flow_Eu[1]) # is logit_flow >=0 for 1SW?
   test[2] <- step(logit_flow_Eu[2]) # is logit_flow >=0 for MSW?  
   test[3] <- step(lflow_fall_Eu[1]) # is logit_flow_fall >=0 for 1SW?
   test[4] <- step(lflow_fall_Eu[2]) # is logit_flow_fall >=0 for MSW?

   diff_flow <- logit_flow_Eu[1] - logit_flow_Eu[2] 
   test[5] <- step(diff_flow) #is difference in slope >=0 for 1SW? (1SW>MSW)
   
   diff_fall <- lflow_fall_Eu[1] - lflow_fall_Eu[2]
   test[6] <- step(diff_fall) # is difference in slope in fall >=0 for 1SW? (1SW>MSW)

   ### Probabilities to be captured at Beauchamps after reproduction (time and sea age dependent)
  for (a in 1:2) {
    logit_mupi_B[a] <- log(mupi_B[a]/(1-mupi_B[a])) # logit transformation
    #varpi_B[a] <- (sigmapi_B[a])*(sigmapi_B[a]) 
    precpi_B[a] <- 1/(varpi_B[a]) # precision
    
    for (t in 1:5) { ## pi_B: Exchangeable from 1984 to 1988
      logit_pi_B[t,a] ~ dnorm(logit_mupi_B[a],precpi_B[a])
      pi_B[t,a] <- exp(logit_pi_B[t,a])/(1+exp(logit_pi_B[t,a])) * pd[t]
      pd[t] <- exp(logit_pd[t])/(1+exp(logit_pd[t])) # back-transformation on the probability scale
      logit_pd[t] <- a_pd + b_pd * logit(rp[t])
      } ## End of loop over years
      
    pi_B[6,a] <- 0 # Trap not working in 1989
  
    for (t in 7:9) { ## pi_B: Exchangeable from 1990 to 1992
      logit_pi_B[t,a] ~ dnorm(logit_mupi_B[a],precpi_B[a])
      pi_B[t,a] <- exp(logit_pi_B[t,a])/(1+exp(logit_pi_B[t,a]))  # back-transformation on the probability scale
      } ## End of loop over years
    
    pi_B[10,a] <- 0 # Trap not working in 1993
    
    for (t in 11:16) { ## pi_B: Exchangeable from 1994 to 1999
      logit_pi_B[t,a] ~ dnorm(logit_mupi_B[a],precpi_B[a])
      pi_B[t,a] <- exp(logit_pi_B[t,a])/(1+exp(logit_pi_B[t,a]))  # back-transformation on the probability scale
    } ## End of loop over years
    
    pi_B[17,a] <- 0 # Trap not working in 2000
    pi_B[18,a] <- 0 # Trap not working in 2001
    
    for (t in 19:Y) { ## pi_B: Exchangeable from 2002 to now on
      logit_pi_B[t,a] ~ dnorm(logit_mupi_B[a],precpi_B[a])
      pi_B[t,a] <- exp(logit_pi_B[t,a])/(1+exp(logit_pi_B[t,a]))  # back-transformation on the probability scale
      } ## End of loop over years
    } # end of loop over sea age
    
  ######################################################################################################################################
  ######################################################################################################################################
  ## DATA
  ## --------------
  ##  C_Eu[t,a]: Annual number of fish captured at Eu per sea age category (1SW/MSW)
  ##  Cm_Eu[t,a]: Annual number of fish captured and marked at Eu per sea age category
  ##  Cm_B[t,a]: Annual number of marked fish captured at Beauchamps per sea age category
  ##  Cum_B[t,a]: Annual number of unmarked fish captured at Beauchamps per sea age category    
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
  shape_lambda ~ dgamma(0.001,0.001)
  rate_lambda ~ dgamma(0.001,0.001)
  
  #To be able to initiate lambda_tot ensuite
  lambda_tot0 ~ dgamma(shape_lambda,rate_lambda)
  Plambda0[1:2] ~ ddirich(s[])
    
  # Hyperprior for lambda_n
  for (t in 1:Y) {
    lambda_tot[t] ~ dgamma(shape_lambda,rate_lambda) 
    Plambda[t,1:2] ~ ddirich(s[])
  } # end of loop over years
  
  for (a in 1:2) {
    s[a] ~ dexp(0.25)T(0.02,)
    } # end of loop over sea age
  
  for (t in 1:Y-1) {
    lambda_n[t,1]  <- lambda_tot[t] * Plambda[t,1]
    lambda_n[t+1,2]  <- lambda_tot[t] * Plambda[t,2]
    }  # end of loop over years
  
  ### lambda_n is calculated by smolt cohort (proportion and mean population size of MSW is one year yearlier than 1SW)
  ## First year for MSW
  lambda_n[1,2] <- lambda_tot0 * Plambda0[2]
  
  ## Last year for 1SW   
  lambda_n[Y,1] <- lambda_tot[Y] * Plambda[Y,1] 
 
  for (a in 1:2) {
    for (t in 1:Y) {
      n[t,a] ~ dpois(lambda_n[t,a]) # annual number of fish per sea age
      } #end of loop over years
   } # end of loop over sea age
    
    #################################
    ## CAPTURE AT EU
    ## -------
    ## n_um[t]: Annual number of fish not captured (not marked) at Eu 
    ##################################
    ## Likelihood on trapped fish number
  for (a in 1:2) { 
      for (t in 1:16) {  # from 1984 to 1999
          C_Eu[t,a] ~ dbin(pi_Eu[t,a],n[t,a]) 
          ## Fish number avoiding the trap, not marked fish (n_um). 
          n_um[t,a] <- n[t,a] - C_Eu[t,a]
          } ## End of loop over years
          
      #Year 2000
      p_Eu00_tot[a] <- pi_Eu[17,a] * pi_Eu00[a] # probability in 2000 to be observed is set to be smaller than later
  
      C_Eu[17,a] ~ dbin(p_Eu00_tot[a],n[17,a])   # 1994 is considered to be different.
      n_um[17,a] <- n[17,a] - C_Eu[17,a] ## Fish number avoiding the trap, not marked fish (n_um) in 2000
  
      #Year 2001
      p_Eu01_tot[a] <- pi_Eu[18,a] * pi_Eu01[a] # probability in 2000 to be observed is set to be smaller than later
  
      C_Eu[18,a] ~ dbin(p_Eu01_tot[a],n[18,a])   # 1994 is considered to be different.
      n_um[18,a] <- n[18,a] - C_Eu[18,a] ## Fish number avoiding the trap, not marked fish (n_um) in 2001       
      
      for (t in 19:Y) {  # from 2002 to 2017
        C_Eu[t,a] ~ dbin(pi_Eu[t,a],n[t,a]) 
    
        ## Fish number avoiding the trap, not marked fish (n_um). 
        n_um[t,a] <- n[t,a] - C_Eu[t,a]
        } ## End of loop over years         
      
      
      } # end of loop over sea age
 	    
    #####################################
    ## RECAPTURE OF FISH AT BEAUCHAMPS AFTER REPRODUCTION
    ## -------------------------------------
    ##
  for (a in 1:2) {
      for (t in 1:Y) {
         ## Marked fish
         Cm_B[t,a] ~ dbin(pi_B[t,a],Cm_Eu[t,a])
         # Cm_B[t,a] ~ dbin(pi_B[t,a]*eff_B[t],Cm_Eu[t,a]) # eff_B is the ratio of the number of trapping nights over the mean number of nights of trapping from 1984 to today (data)
         ## Unmarked fish
         Cum_B[t,a] ~ dbin(pi_B[t,a],n_um[t,a])
         #Cum_B[t,a] ~ dbin(pi_B[t,a]*eff_B[t],n_um[t,a]) # eff_B is the ratio of the number of trapping nights over the mean number of nights of trapping from 1984 to today (data)
         } # end of loop over years
      
  } # end of loop over sea age
} # end of the model

