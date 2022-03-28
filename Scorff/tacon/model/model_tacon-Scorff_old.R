####################################################################################################################
###                          Model of CMR data to estimate parr population size                                  ###   
###                          of Salmo salar in Scorff river + tributary rivers                                   ###
###   (Intercalibration between CPUE and successive removals included;  data from Pr?vost & Nihouarn 1999)       ###   
###                                  Sabrina Servanty & Etienne Pr?vost                                          ###
###                                           September 2015                                                     ###
####################################################################################################################


## /!\ Unit of density is rapid equivalent in 100?, not in m?. Very hard to get update otherwise because samplers are sampling very low values 

model {

################################################################################################
# 1/ INTERCALIBRATION BETWEEN CPUE AND SUCCESSIVE REMOVALS ( DATA FROM PREVOST & NIHOUARN 1999)
#  Main differences: - including sites with a width <3m
#                    - considering a decrease in the probability of capture during the second pass
#                    - effect of width is included in the factor of proportionality
################################################################################################
## i: station; 1 to Ninter - from 1 to 52 sampled stations for intercalibration (including river's width <3m)
##############################################################################################################

##############################################################################################################
## DATA
## --------------
## CPUE[i]: number of fish per 5 min of sampling per station
## C1[i]: number of fish captured during the first pass when doing successive removals per station
## C2[i]: number of fish captured during the second pass when doing successive removals per station
## Width[i]: river's width per station
###############################

##############################
# Priors
##############################

### CPUE and density
int_width ~ dunif(-10,10) # constant linked to the relationship with width
int_width_cut <- cut(int_width) ## This parameter is going to be used in the second step of the model

width_coef ~ dunif(-10,10) # factor of proportionality with river's width
width_coef_cut <- cut(width_coef) ## This parameter is going to be used in the second step of the model

p_cpue ~ dbeta(2,2) # this non informative prior is excluding extreme value
rate_lcpu <- p_cpue/(1-p_cpue) # variance of a Bernouilli process
#rate_lcpu ~ dgamma(0.01,0.01) # rate (inverse scale) of the gamma distribution for lambda_cpu
rate_lcpu_cut <- cut(rate_lcpu) ## This parameter is going to be used in the second step of the model

### Successive removal and density
mup_rem ~ dbeta(2,2) #mup_rem ~ dbeta(1,1) # mean of probability of capture during successive removals
lmu_prem <- log(mup_rem/(1-mup_rem)) # logit transformation

sd_prem ~ dunif(0,10) # sd of probability of capture during successive removals
prec_prem <- 1/(sd_prem * sd_prem) # precision of probability of capture during successive removals

eps ~ dunif(0,10) #eps ~ dnorm(0,0.01)I(0,) # decrease in the probability of capture during the second pass

## Density
mu_d ~ dgamma(1,0.1) #mu_d ~ dgamma(1,0.01) ## Mean density 
rate_d ~ dgamma(0.1,0.1)# rate_d ~ dgamma(0.01,0.01) #### Inverse scale of a Gamma distribution for density
shape_d <- mu_d * rate_d # shape parameter of the gamma distribution

for (i in 1:Ninter) {

    #########################
    #### Density
    #########################
    d[i] ~ dgamma(shape_d,rate_d)

    ########################
    ### CPUE and density
    ##########################

    CPUE[i] ~ dpois(lambda_cpu2[i])
    
    lwidth[i] <- log(Width[i]) # log transformation of rivers' width
    k_width[i] <- int_width + width_coef * lwidth[i]  # proportional relationship with width
    k_cpu[i] <- exp(k_width[i])
    
    mul_cpu[i] <- k_cpu[i] * d[i]  # mean lambda_cpu
    
    shape_lcpu[i] <- mul_cpu[i] * rate_lcpu   # shape parameter of the gamma distribution for lambda_cpu
    lambda_cpu[i] ~ dgamma(shape_lcpu[i],rate_lcpu)I(0.001,)
    lambda_cpu2[i] <- max(0.001,lambda_cpu[i]) # try to avoid error on lambda_cpu
    
    ####################################
    ### Successive removals and density
    ###################################
    ### Probability of capture
    lp_rem1[i] ~ dnorm(lmu_prem,prec_prem)
    p_rem1[i] <- exp(lp_rem1[i])/(1+exp(lp_rem1[i]))  # back-transformation on the probability scale: probability of capture for the first pass

    lp_rem2[i] <- lp_rem1[i] - eps  # probability of capture during the second pass is smaller than the first one
    p_rem2[i] <- exp(lp_rem2[i])/(1+exp(lp_rem2[i]))  # back-transformation on the probability scale: probability of capture for the second pass
    
    ntot[i] ~ dpois(lambda_n[i]) # total population size
    lambda_n[i] <- d[i] * S[i]
    
    ## Successive removals
    C1[i] ~ dbin(p_rem1[i],ntot[i]) # first pass
    n1[i] <- ntot[i]-C1[i] # fish not captured during the first pass

    C2[i] ~ dbin(p_rem2[i],n1[i]) # second pass

    } # end of loop over sites sampled during intercalibration

#####################################################################################
### 2/ CMR data to estimate parr population size in Scorff river + tributary rivers                            
######################################################################################

###############################################################################
## DATA:
## Nstation: 53 sites including tributary rivers (site #52 and #53 are already included although sampling begins in 2015. Last year considered now is 2014)
## CPUE_Sc[j,t]: Number of fish captured by CPUE per site and per year. Recurrent sites are the first 40 ones then tributary rivers
## Nyear: Since 1993 to now on
## S_Sc[j,t]: Surface in riffle/rapid equivalent (unit:100m?) per site and per year. 
## W_Sc[j,t]: Width per station and per year. For tributary rivers, the width is not always the same. It has been adjusted depending on how many sites were sampled within a year.
###############################################################################

###############################################
## DENSITY (dj)
##||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## PRIORS
## ------
###  ## Density (on log scale)
sigma_dSc ~ dunif(0,10) ## Overall standard deviation in density in Scorff river
sigma_dSc_cut <- cut(sigma_dSc) # use to predict abundance

var_dSc <- sigma_dSc*sigma_dSc 
tau_dSc <- 1/var_dSc # precision

#### Year effect (on log scale)
int_ydSc ~ dnorm(0,0.01)T(-5,5) ## Intercept for flow effect in mean annual density in Scorff river
log_flow ~ dnorm(0,0.01)T(-5,5)  ## Slope for flow

int_ydSc_cut <- cut(int_ydSc) # use to predict density when taking into account osberved flow during habitat sampling
log_flow_cut <- cut(log_flow) # use to predict density when taking into account osberved flow during habitat sampling

sigma_ySc ~ dunif(0,10) ## Standard deviation of year effect
var_ySc <- sigma_ySc*sigma_ySc 
tau_ySc <- 1/var_ySc # precision

test <- step(log_flow) #is log_flow >=0?

for (t in 1:Nyear) {
    logQ[t] <- log(Q[t]) # ln transformation of flow (calculated with only days when electric fishing is occuring)
    stlogQ[t] <- (logQ[t] - mean(logQ[]))/sd(logQ[]) # standardized covariate  

    mu_ydSc[t] <- int_ydSc + log_flow * stlogQ[t] ## annual mean of density linked to flow
    mu_ydSc_cut[t] <- cut(mu_ydSc[t]) # use to predict the abundance

    year_dSc[t] ~ dnorm(mu_ydSc[t],tau_ySc) ## year effect
    year_dSc_cut[t] <- cut(year_dSc[t]) # use to predict the abundance
    } # end of loop over years

## Site effect (on log scale)
sigma_siteSc ~ dunif(0,10) ## Standard deviation of site effect
var_siteSc <- sigma_siteSc*sigma_siteSc 
tau_siteSc <- 1/var_siteSc

for (j in 1:Nstation) {
    site_Sc[j] ~ dnorm(0,tau_siteSc)
    site_Sc_cut[j] <- cut(site_Sc[j]) # use to predict the abundance
    } # end of loop over sites


## Density per station and per year  
for (j in 1:Nstation) {
    for (t in 1:Nyear) {     
        log_mudSc[j,t] <- year_dSc[t] + site_Sc[j]
        log_dSc[j,t] ~ dnorm(log_mudSc[j,t],tau_dSc)T(-10,10)
        log2_dSc[j,t] <- max(-10,log_dSc[j,t]) # try to avoid error on lambda_cpu  
        d_Sc[j,t] <- exp(log2_dSc[j,t]) # back transformation on natural scale
        eps_dSc[j,t] <- (log_dSc[j,t]-log_mudSc[j,t])/sigma_dSc ## standardized residuals
        eps_dSc_cut[j,t] <- cut(eps_dSc[j,t]) # use to predict the abundance 
        } # end of loop over years
    }# end of loop over sites


########################
### CPUE and density
##########################
for (j in 1:Nstation) {
   for (t in 1:Nyear) {  
        lwidth_Sc[j,t] <- log(W_Sc[j,t]) # log transformation of river' width at each site
        k_widthSc[j,t] <- int_width_cut + width_coef_cut * lwidth_Sc[j,t]  # proportional relationship with width
        k_cpuSc[j,t] <- exp(k_widthSc[j,t])
   
        CPUE_Sc[j,t] ~ dpois(lambdaSc_cpu2[j,t])

        mul_cpuSc[j,t] <- k_cpuSc[j,t] * d_Sc[j,t]  # mean lambda_cpu

        shape_lcpuSc[j,t] <- mul_cpuSc[j,t] * rate_lcpu_cut   # shape parameter of the gamma distribution for lambda_cpu
        lambdaSc_cpu[j,t] ~ dgamma(shape_lcpuSc[j,t],rate_lcpu_cut)I(0.001,)
        lambdaSc_cpu2[j,t] <- max(0.001,lambdaSc_cpu[j,t]) # try to avoid error on lambda_cpu  
        } # end of loop over years
   } # end of loop over sites

##############
# Abundance   
##############
## Correcting density by the observed flow when habitat sampling was done.
for (t in 1:Nyear) {
    eps_ydSc[t] <- year_dSc_cut[t] - mu_ydSc_cut[t] # annual residuals 

    logQ95_10[t] <- log(Q95_10[t]) # ln transformation of flow
    stlogQ95_10[t] <- (logQ95_10[t] - mean(logQ[]))/sd(logQ[]) # standardized covariate with observed flow during sampling as a reference
    
    logQ95[t] <- log(Q95[t]) # ln transformation of flow
    stlogQ95[t] <- (logQ95[t] - mean(logQ[]))/sd(logQ[]) # standardized covariate with observed flow during sampling as a reference
    
    logQ96[t] <- log(Q96[t]) # ln transformation of flows
    stlogQ96[t] <- (logQ96[t] - mean(logQ[]))/sd(logQ[]) # standardized covariate with observed flow during sampling as a reference
    
    y9510_dSc[t] <- int_ydSc_cut + log_flow_cut * stlogQ95_10[t] + eps_ydSc[t]
    y95_dSc[t] <- int_ydSc_cut + log_flow_cut * stlogQ95[t] + eps_ydSc[t]
    y96_dSc[t] <- int_ydSc_cut + log_flow_cut * stlogQ96[t] + eps_ydSc[t]
    } # end of loop over years
    
for (j in 1:40) {  # Habitat in the first 40 sites were sampled in 1995 and in 2010
    for (t in 1:Nyear) {     
        log_dSc_pred[j,t] <- y9510_dSc[t] + site_Sc_cut[j] + eps_dSc_cut[j,t] * sigma_dSc_cut
        d_Sc_pred[j,t] <- exp(log_dSc_pred[j,t]) # back transformation on natural scale
        } # end of loop over years
    }# end of loop over sites    

for (j in 41:43) { # Habitat in site 41, 42 and 43 were only sampled in 1995    
     for (t in 1:Nyear) {     
        log_dSc_pred[j,t] <- y95_dSc[t] + site_Sc_cut[j] + eps_dSc_cut[j,t] * sigma_dSc_cut
        d_Sc_pred[j,t] <- exp(log_dSc_pred[j,t]) # back transformation on natural scale
        } # end of loop over years
    }# end of loop over sites    

for (t in 1:Nyear) {     
    log_dSc_pred[44,t] <- y96_dSc[t] + site_Sc_cut[44] + eps_dSc_cut[44,t] * sigma_dSc_cut  # site 44 was only sampled in 1996
    d_Sc_pred[44,t] <- exp(log_dSc_pred[44,t]) # back transformation on natural scale
    } # end of loop over years

for (j in 45:49) { # Habitat in site 45 to 49 were only sampled in 1995    
     for (t in 1:Nyear) {     
        log_dSc_pred[j,t] <- y95_dSc[t] + site_Sc_cut[j] + eps_dSc_cut[j,t] * sigma_dSc_cut
        d_Sc_pred[j,t] <- exp(log_dSc_pred[j,t]) # back transformation on natural scale
        } # end of loop over years
    }# end of loop over sites    

for (j in 50: Nstation) { # the last 4 sites were only sampled in 1996
    for(t in 1:Nyear) {     
        log_dSc_pred[j,t] <- y96_dSc[t] + site_Sc_cut[j] + eps_dSc_cut[j,t] * sigma_dSc_cut
        d_Sc_pred[j,t] <- exp(log_dSc_pred[j,t]) # back transformation on natural scale
        } # end of loop over years
    }# end of loop over sites    

# Mean of poisson distribution for number of fish per site and per year
# Making distinction before 2010 and after (Seq RR have been updated)
for (j in 1:Nstation) {
    for (t in 1:Nyear) { 
        lambda_nsite[j,t] <- d_Sc_pred[j,t] * S_Sc[j,t] # mean abundance
        }  # end of loop over time
    } # end of loop over sites 

# Using normal law instead of a poisson law to speed up the process  
# Lambda is high enough for a good approximation. Mean = lambda, variance = lambda  
for (t in 1:Nyear) {
   lambda[t] <- sum(lambda_nsite[,t]) 
   tau_lambda[t] <- 1/lambda[t]
   n_Sc[t] ~ dnorm(lambda[t], tau_lambda[t]) 
   ntot_Sc[t] <- round(n_Sc[t]) # total abundance of parr in Scorff + tributary rivers    
   }  #end of loop over years
   
} #end of model

    
