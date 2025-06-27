################################################################################
###  Observation model for electric fishing data of juveniles                ###
###                       Salmo salar - Nivelle                              ###
##                Model from Melanie Brun et al. 2011                        ###
###                  Annotated by Sabrina Servanty  ( February 2014)         ###
################################################################################
# 2023:
# - inclusion intercalibration pulsium (k_inter, k_cpue_Puls)
# - modification de la survie des repeuplements pour ?viter des effectifs de tacons 0+ sup?rieurs aux effectifs d?vers?s
#
model {

#########################################
## Surface per zone in riffles equivalent
### There isn't any zone 1, 2, 6 in the data set
### Beta_dj is a habitat effect. Beta_dj[2] is for run habitat. Beta_dj[1] is for rapid/riffle and is the reference
  for (z in 3:5) {
    Stot_req[z] <- S_tot[z,1]+(S_tot[z,2]*beta_dj[2])
    } ## End of loop over zones
  for (z in 7:8) {
    Stot_req[z] <- S_tot[z,1]+(S_tot[z,2]*beta_dj[2])
    } ## End of loop over zones

#############################################################################################
## Released density of alevin (Dal) or alevins at the resorbed vesicle stage (Drv) in m-2ERR
### Zone 3 is LN, zone 4 is HN, zone 5 is LUR, zone 7 is VHN, zone 8 is LAP
### AL is the number of alevins released per year and per zone
### RV is the number of alevins at the resorbed vesicle stage per year and per zone

### Releases of AL in 1986-1990 (y=3 to 7) in HN and LUR (zone 4 and 5) 
  for (y in 3:7) { 
    for (z in 4:5) {
      Dal[y,z] <- AL[y,z]/Stot_req[z]
      } ## End of loop over zones
    } ## End of loop over years

### Releases of AL in 1989 (y=6) in LAP (zone 8)
  Dal[6,8] <- AL[6,8]/Stot_req[8]

### Releases of AL from 1989 to 1992 (y=6 to 9) in VHN (zone 7)
  for (y in 6:9) { 
    Dal[y,7] <- AL[y,7]/Stot_req[7]
    } ## End of loop over years

### Releases of AL in 1994 (y=11) in VUN (zone 7)
Dal[11,7] <- AL[11,7]/Stot_req[7] 

### Releases of AL in 1994-1995 (y=11,12) in LAP (zone 8)
  for (y in 11:12) { 
    Dal[y,8] <- AL[y,8]/Stot_req[8]
  } ## End of loop over years

### Releases of AL in 1996 (y=13) in LN, HN, LUR (zone 3, 4, 5)
  for (z in 3:5) { 
    Dal[13,z] <- AL[13,z]/Stot_req[z]
    } ## End of loop over zones

### Releases of RV in 1998 and 1999 (y=15 and 16) in LN, HN, LUR (zone 3, 4, 5)
  for (z in 3:5) { 
    for (y in 15:16) {
      Drv[y,z] <- RV[y,z]/Stot_req[z]
      } ## End of loop over years
    } ## End of loop over zones

### Releases of RV in 2000 (y=17) in HN (zone 4)
  Drv[17,4] <- RV[17,4]/Stot_req[4] 

### Releases of RV in 2002 (y=19) in HN and LUR(zone 4 and 5)
  for (z in 4:5) { 
    Drv[19,z] <- RV[19,z]/Stot_req[z]
    } ## End of loop over zones 

### Releases of AL (in zone 4) and RV (in zone 4 and 5) in 2004 (y=21)
  Dal[21,4] <- AL[21,4]/Stot_req[4] 
  Drv[21,4] <- RV[21,4]/Stot_req[4]
  Drv[21,5] <- RV[21,5]/Stot_req[5]

### Releases of RV in 2005 (y=22) in HN and LUR (zone 4 and 5)
  for (z in 4:5) { 
    Drv[22,z] <- RV[22,z]/Stot_req[z]
  } ## End of loop over zones 

### Releases of RV in 2006 (y=23) in HN (zone 4)
  Drv[23,4] <- RV[23,4]/Stot_req[4] 

### Releases of RV in 2007 and 2008 (y=24 and 25) in LN, HN and LUR (zone 3, 4, 5)
  for (z in 3:5) { 
    for (y in 24:25) {
      Drv[y,z] <- RV[y,z]/Stot_req[z]
      } ## End of loop over years
    } ## End of loop over zones

### Releases of RV in 2009 (y=26) in LN (zone 3)
  Drv[26,3] <- RV[26,3]/Stot_req[3] 

#################################################################################
#################################################################################
## SUCCESSIVE REMOVAL EFFICIENCY
##||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## PRIORS
## ------

  mu_p_srem ~ dbeta(1,1) # Mean capture probability by successive removal
  sd_logit_p_srem ~ dunif(0,10) # Standard deviation of the logit transformed capture probability
  epsilon_p ~ dnorm(0,0.001)I(0,) # decrease of efficiency between successive passes

## PROBABILITY DISTRIBUTIONS
## -------------------------
#### logit(p) variance, precision and logit(mu_p)
  v_logit_p_srem <- (sd_logit_p_srem)*(sd_logit_p_srem)
  prec_logit_p_srem <- 1/(v_logit_p_srem)
  logit_mu_p_srem <- log(mu_p_srem/(1-mu_p_srem)) # logit transformation

#### Predictive posterior
  logit_p_srem_pred ~ dnorm(logit_mu_p_srem,prec_logit_p_srem)
  p_srem_pred <- exp(logit_p_srem_pred)/(1+exp(logit_p_srem_pred)) # back-transformation of p-srem on probability scale


###############################################################################
## DENSITY (dj)
##||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## PRIORS
## ------
#### Habitat effect: 1 is for rapid/riffle and is the reference. 2 is for run
  beta_dj[1] <- 1 
  beta_dj[2] ~ dgamma(0.01,0.01) #mean effect for run

### Inverse scale of a Gamma distribution for density per zone
  eta_dj[1] ~ dgamma(0.09,0.003) ; eta_dj[3] ~ dgamma(0.09,0.003)

## Natural recruitment
  mu_dj_nat ~ dgamma(1,5) ## Mean density issued from reproduction

## Shape and inverse scale of a Gamma distribution for year effect
  zeta_alpha_dj ~ dgamma(0.01,0.01) I(0.01,)

# Modifications des hyper-priors zone specifique pour les effets d'interaction zone X year
# Zone 3 is the reference (LN)
# zeta is the shape parameter of a Gamma distribution
# eta is the inverse scale parameter of a Gamma distribution
  zeta_gamma_dj[4] ~ dgamma(0.01,0.01) ; eta_gamma_dj[4] ~ dgamma(0.01,0.01)
  zeta_gamma_dj[5] ~ dgamma(0.01,0.01) ; eta_gamma_dj[5] ~ dgamma(0.01,0.01)

## Alevins survival (without restrictions)
## Survival is different by stages: 1 = rv alevins ; 2 = alevins compensation ; 3 = alevins restocking
  for (s in 1:3) { 
  delta[s] ~ dunif(0.001,1)
  } ## End of loop over stages

## Density dependence effect when doing restocking. 
## The effect also depends on zone (1=Nivelle +LUR, 2=LAP). Zone 1 is the reference
  pi_dj ~ dgamma(0.01,0.01)
  xi_dj[1] <- 1
  #xi_dj[2] ~ dgamma(0.01,0.01)
  xi_dj[2] ~ dgamma(1,0.7) # ep+mb 29/03/2023
# nouveau prior faiblement informatif en lien avec la modif du mod?le apport?e par ailleurs (voir plus bas)
# assure une m?diane de 1 et une gamme de variation de xi entre 0.36 et 5.2
# assure aussi une survie densit? ind?pendante faiblement informative en association avec delta  

## PROBABILITY DISTRIBUTIONS
## -------------------------
## Year & Year*Zone effects for zone 3 (LN) which is the reference
  for (y in 2:Y_last) {   # because zone 1 was accessible to adults from the beginning of the study
  alpha_dj[y] ~ dgamma(zeta_alpha_dj,zeta_alpha_dj) ## Year effect
  gamma_dj[y,3] <- 1 ## Interaction year X zone. Equal to one because zone 3 is the reference
  } ## End of loop over years

  for (y in 8:Y_last) { #from year 1992, zone 4 and 5 became accessible to adults
    for (z in 4:5) { # zone 4 = HN, zone 5 = LUR
# Modif mod?le Brun et al. (2011) : distribution zone sp?cifique de l'interaction year X zone
  gamma_dj[y,z] ~ dgamma(zeta_gamma_dj[z],eta_gamma_dj[z])        
    } ## End of loop over zones
  } ## End of loop over years
  for (y in 41:Y_last) { #from year 2024 zone 7 (Spanish Nivelle) became accessible to adults
# Spanish Nivelle is treated as equivalent to the upper Nivelle: to be revisited with more years# ep-12.3.2025
  gamma_dj[y,6] <- 1 # mb+ep 12.3.2025: need to specify a value/ gamma_dj[3:7] can not be discontinuous
  gamma_dj[y,7] ~ dgamma(zeta_gamma_dj[5],eta_gamma_dj[5])    
  } ## End of loop over years

#####################
## Mean density per type of recruitment, year, zone and habitat (mu_dj[r,y,z,h]) 
## Shape parameter per type of recruitment, year, zone and habitat(zeta_dj[r,y,z,h])
## r is for the type of recruitment (natural = 1, due to compensation = 2, due to restocking= 3 or total = 4)
## y is for year  (from 2 to Y_last)
## z is for zone  (3 is for LN, 4 is for HN and 5 is for LUR)
## h is habitat effect (1 for riffle/rapid, 2 is for run)

### Natural recruitment (r = 1) ###
### LN from 1985 to 1990. After 1990, colonization in zone 4 and 5 was possible after building a fish pass at the dam.
  for (h in 1:2) {
  
    for (y in 2:7) {
      # mean density issued from natural reproduction * year effect * year x zone * hab effect
      mu_dj[1,y,3,h] <- mu_dj_nat*alpha_dj[y]*gamma_dj[y,3]*beta_dj[h]
      # conjugate shape parameter (inverse gamma)
      zeta_dj[1,y,3,h] <- mu_dj[1,y,3,h]*eta_dj[1]
      } ## End of loop over years

  ## LN, HN & LUR from 1991 to last year
    for (z in 3:5) {
      for (y in 8:Y_last) {
      mu_dj[1,y,z,h]<- mu_dj_nat*alpha_dj[y]*gamma_dj[y,z]*beta_dj[h]
      zeta_dj[1,y,z,h] <- mu_dj[1,y,z,h]*eta_dj[1]
      } ## End of loop over years
    } ## End of loop over zones
 ## Spanish Nivelle (+Lap) from 2024 to last year
      for (y in 41:Y_last) {
      mu_dj[1,y,7,h]<- mu_dj_nat*alpha_dj[y]*gamma_dj[y,7]*beta_dj[h]
      zeta_dj[1,y,7,h] <- mu_dj[1,y,7,h]*eta_dj[1]
      } ## End of loop over years

### Compensation (r = 2) ###
  ## 1996 : LN, HN & LUR = Alevins
    for (z in 3:5) {
      # released alevin density * survival when doing compensation releases * habitat effect
      mu_dj[2,13,z,h] <- Dal[13,z]*delta[2]*beta_dj[h]
      # mean total density (adding density issued by natural reproduction and compensation releases)
      mu_dj[4,13,z,h] <- mu_dj[1,13,z,h]+mu_dj[2,13,z,h]
      } ## End of loop over zones

  ## 1998-1999 : LN, HN & LUR = Alevins RV
    for (z in 3:5) {
      for (y in 15:16) {
        # released alevin resorbed vesicle density * survival of alevin with resorbed vesicle * habitat effect
        mu_dj[2,y,z,h] <- Drv[y,z]*delta[1]*beta_dj[h]
        # mean total density (adding density issued by natural reproduction and compensation releases)
        mu_dj[4,y,z,h] <- mu_dj[1,y,z,h]+mu_dj[2,y,z,h]
        } ## End of loop over years
      } ## End of loop over zones

  ## 2000 : HN = alevins RV
    mu_dj[2,17,4,h] <- Drv[17,4]*delta[1]*beta_dj[h]
    mu_dj[4,17,4,h] <- mu_dj[1,17,4,h]+mu_dj[2,17,4,h]

  ## 2002 : HN & LUR = alevins RV
    for (z in 4:5) {
      mu_dj[2,19,z,h] <- Drv[19,z]*delta[1]*beta_dj[h]
      mu_dj[4,19,z,h] <- mu_dj[1,19,z,h]+mu_dj[2,19,z,h]
      } ## End of loop over zones 

  ## 2004 : HN = alevins + alevins RV ; LUR = alevins RV
    mu_dj[2,21,4,h] <- ((Dal[21,4]*delta[2])+(Drv[21,4]*delta[1]))*beta_dj[h]
    mu_dj[2,21,5,h] <- Drv[21,5]*delta[1]*beta_dj[h]
    mu_dj[4,21,4,h] <- mu_dj[1,21,4,h]+mu_dj[2,21,4,h]
    mu_dj[4,21,5,h] <- mu_dj[1,21,5,h]+mu_dj[2,21,5,h]

  ## 2005 : HN & LUR = alevins RV
    for (z in 4:5) {
      mu_dj[2,22,z,h] <- Drv[22,z]*delta[1]*beta_dj[h]
      mu_dj[4,22,z,h] <- mu_dj[1,22,z,h]+mu_dj[2,22,z,h]
      } ## End of loop over zones 

  ## 2006 : HN = alevins RV
    mu_dj[2,23,4,h] <- Drv[23,4]*delta[1]*beta_dj[h]
    mu_dj[4,23,4,h] <- mu_dj[1,23,4,h]+mu_dj[2,23,4,h]

  ## 2007 & 2008 : LN, HN & LUR = alevins RV
    for (z in 3:5) {
      for (y in 24:25) {
        mu_dj[2,y,z,h] <- Drv[y,z]*delta[1]*beta_dj[h]
        mu_dj[4,y,z,h] <- mu_dj[1,y,z,h]+mu_dj[2,y,z,h]
        } ## End of loop over years
      } ## End of loop over zones

  ## 2009 : LN = alevins RV
    mu_dj[2,26,3,h] <- Drv[26,3]*delta[1]*beta_dj[h]
    mu_dj[4,26,3,h] <- mu_dj[1,26,3,h]+mu_dj[2,26,3,h]

### Restocking (r = 3) ###
  ## HN & LUR from 1986 to 1990
    for (y in 3:7) {
      for (z in 4:5) {
        # density of alevins restocking * survival restocking * density depence^(density restocking) * zone effect Niv+LUR
        ## * habitat effect
        #mu_dj[3,y,z,h] <-Dal[y,z]*delta[3]*pow(pi_dj,Dal[y,z])*xi_dj[1]*beta_dj[h]
        mu_dj[3,y,z,h] <-Dal[y,z]*delta[3]*pow(pi_dj,Dal[y,z])*(xi_dj[1]/(1+(xi_dj[1]-1)* delta[3]))*beta_dj[h] # ep-30.03.2023
        # modification pour s'assurer que la survie des d?versement est inf?rieure ? 1
        # et que les effectifs de tacons 0+ issus des deversement ne soient pas sup?rieurs aux effectifs d?vers?s
        # pour xi_dj[1] = 1 cette formulation est ?quivalent ? l'ancienne
        zeta_dj[3,y,z,h] <- mu_dj[3,y,z,h]*eta_dj[3]
        } ## End of loop over zones
      } ## End of loop over years

  ## LAP in 1989
    #mu_dj[3,6,8,h] <- Dal[6,8]*delta[3]*pow(pi_dj,Dal[6,8])*xi_dj[2]*beta_dj[h]
    mu_dj[3,6,8,h] <- Dal[6,8]*delta[3]*pow(pi_dj,Dal[6,8])*(xi_dj[2]/(1+( xi_dj[2]-1)* delta[3]))*beta_dj[h] # ep-30.03.2023
        # modification pour s'assurer que la survie des d?versement est inf?rieure ? 1
        # et que les effectifs de tacons 0+ issus des deversement ne soient pas sup?rieurs aux effectifs d?vers?s
    zeta_dj[3,6,8,h] <- mu_dj[3,6,8,h]*eta_dj[3]

  ## VHN from 1989 to 1992
    for (y in 6:9) {
      #mu_dj[3,y,7,h] <- Dal[y,7]*delta[3]*pow(pi_dj,Dal[y,7])*xi_dj[1]*beta_dj[h]
      mu_dj[3,y,7,h] <- Dal[y,7]*delta[3]*pow(pi_dj,Dal[y,7])*(xi_dj[1]/(1+( xi_dj[1]-1)* delta[3]))*beta_dj[h] # ep-30.03.2023
        # modification pour s'assurer que la survie des d?versement est inf?rieure ? 1
        # et que les effectifs de tacons 0+ issus des deversement ne soient pas sup?rieurs aux effectifs d?vers?s
      zeta_dj[3,y,7,h] <- mu_dj[3,y,7,h]*eta_dj[3]
      } ## End of loop over years

  ## VHN in 1994
    #mu_dj[3,11,7,h] <- Dal[11,7]*delta[3]*pow(pi_dj,Dal[11,7])*xi_dj[1]*beta_dj
    mu_dj[3,11,7,h] <- Dal[11,7]*delta[3]*pow(pi_dj,Dal[11,7])* (xi_dj[1]/(1+( xi_dj[1]-1)* delta[3]))*beta_dj[h] # ep-30.03.2023
        # modification pour s'assurer que la survie des d?versement est inf?rieure ? 1
        # et que les effectifs de tacons 0+ issus des deversement ne soient pas sup?rieurs aux effectifs d?vers?s
    zeta_dj[3,11,7,h] <- mu_dj[3,11,7,h]*eta_dj[3] 

  ## LAP in 1994 & 1995
    for (y in 11:12) {
     #mu_dj[3,y,8,h] <- Dal[y,8]*delta[3]*pow(pi_dj,Dal[y,8])*xi_dj[2]*beta_dj[h]
     mu_dj[3,y,8,h] <- Dal[y,8]*delta[3]*pow(pi_dj,Dal[y,8])* (xi_dj[2]/(1+( xi_dj[2]-1)* delta[3]))*beta_dj[h] # ep-30.03.2023
        # modification pour s'assurer que la survie des d?versement est inf?rieure ? 1
        # et que les effectifs de tacons 0+ issus des deversement ne soient pas sup?rieurs aux effectifs d?vers?s
     zeta_dj[3,y,8,h] <- mu_dj[3,y,8,h]*eta_dj[3]
     } ## End of loop over years

  } ## End of loop over habitats

#################################################################################
#############################################################################
## LOOP OVER i sites, nb SITES = I sampled. Sorted by Tech,R,Z,Y,H,station
##||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## CPUE PROCESS
## -----------------------------------------------------------------------------
## PRIORS
## ------

   k_cpue ~ dgamma(1,0.01) #  coefficient of proportionality between mean CPUE and density
# k_cpue corresponds to the MP electrofishing used until 2020
# From 2021, this is changed to Pulsium : see new code added in 2023 below  
   rho_s ~ dgamma(1,1) # nonlinear relationshihp between flow and surface
   p_cpue ~ dbeta(2,2) # this non informative prior is excluding extreme value
   eta_cpue <- p_cpue/(1-p_cpue) # inverse scale of NegBin process of capture in CPUE
   sd_s_rec ~ dunif(0,10)
   v_s_rec <- sd_s_rec * sd_s_rec # variance of the recent surface
   prec_s_rec <- 1/v_s_rec # precision

############################
## PROBABILITY DISTRIBUTIONS
## -------------------------
###############################
### Successive removal only ###
  for (i in 1:422) { # until 2002
  ## Probability distributions on capture probability for each pass
  logit_p_1[i] ~ dnorm(logit_mu_p_srem,prec_logit_p_srem)I(-5,5)
  p_1[i] <- exp(logit_p_1[i])/(1+exp(logit_p_1[i])) # back-transformation on a probability scale

  logit_p_2[i] <- logit_p_1[i]-epsilon_p
  p_2[i] <- exp(logit_p_2[i])/(1+exp(logit_p_2[i]))
  
  logit_p_3[i] <- logit_p_2[i]-epsilon_p
  p_3[i] <- exp(logit_p_3[i])/(1+exp(logit_p_3[i]))
  
  logit_p_4[i] <- logit_p_3[i]-epsilon_p
  p_4[i] <- exp(logit_p_4[i])/(1+exp(logit_p_4[i]))


  ## Passes
  ## C is the number of captured individuals for a given pass (1 to 4)
  C1[i] ~ dbin(p_1[i],n1[i]) ; n2[i] <- n1[i]-C1[i]
  C2[i] ~ dbin(p_2[i],n2[i])I(minC2[i],) ; n3[i] <- n2[i]-C2[i]
  C3[i] ~ dbin(p_3[i],n3[i])I(minC3[i],) ; n4[i] <- n3[i]-C3[i]
  C4[i] ~ dbin(p_4[i],n4[i])
  } ## End of loop over sites

## Probability distributions on juvenile number
  for (i in 1:247) { # no compensation or restocking
  lambda_n1[i] <- dj[i]*S[i] # mean of the Poisson distributed count is proportionnal to the density and the surface of the site
  n1[i] ~ dpois(lambda_n1[i]) # number of juveniles
  } ## End of loop over sites
  
  for (i in 248:336) { # when compensation is occuring 
  lambda_n1_nat[i] <- dj_nat[i]*S[i] # mean of the Poisson distributed count when natural reproduction is occuring
  lambda_n1_comp[i] <- dj_comp[i]*S[i] # mean of the Poisson distributed count when compensation is occuring 
  n1_nat[i] ~ dpois(lambda_n1_nat[i]) 
  n1_comp[i] ~ dpois(lambda_n1_comp[i])
  n1[i] <- n1_nat[i]+n1_comp[i]
  } # End of loop over sites
  
  for (i in 337:422) { # when restocking is occuring
  lambda_n1[i] <- dj[i]*S[i]
  n1[i] ~ dpois(lambda_n1[i])
  } ## End of loop over site
  
######################################  
### Successive removal + CPUE ###
  for (i in 423:445) {  # from 2003 to 2005
  ## Probability distributions on capture probability
  logit_p_1[i] ~ dnorm(logit_mu_p_srem,prec_logit_p_srem)I(-5,5)
  p_1[i] <- exp(logit_p_1[i])/(1+exp(logit_p_1[i])) # back-transformation on a probability scale

  logit_p_2[i] <- logit_p_1[i]-epsilon_p
  p_2[i] <- exp(logit_p_2[i])/(1+exp(logit_p_2[i]))
  
  logit_p_3[i] <- logit_p_2[i]-epsilon_p
  p_3[i] <- exp(logit_p_3[i])/(1+exp(logit_p_3[i]))

  ## Recent density
  ## S_rec is the surface of the station
  dj_rec[i] <- (dj[i]*S[i])/S_rec[i]

  ## Link between recent surface and surface in 1985
  ## Q is the flow measured the day of sampling at each station
  LS_rec[i] ~ dnorm(Lmu_s_rec[i],prec_s_rec)
  LS_rec[i] <- log(S_rec[i])
  #S_rec[i] <- exp(LS_rec[i]) # back transformation of LS_rec
  Lmu_s_rec[i] <- log(Q[i]/Q_85[i])*rho_s + log(S[i]) 

  ## Passes
  C1[i] ~ dbin(p_1[i],n1[i]) ; n2[i] <- n1[i]-C1[i]
  C2[i] ~ dbin(p_2[i],n2[i]) ; n3[i] <- n2[i]-C2[i]
  C3[i] ~ dbin(p_3[i],n3[i])

  ## CPUE Pass
  CPUE[i] ~ dpois(lambda_cpue[i])
  lambda_cpue[i] ~ dgamma(zeta_cpue[i],eta_cpue)I(0.001,)
  zeta_cpue[i] <- k_cpue*dj_rec[i]*eta_cpue
  } ## End of loop over sites

  ## Probability distributions on juvenile number
     for (i in 423:434) { # no compensation
         lambda_n1[i] <- dj[i]*S[i] ; n1[i] ~ dpois(lambda_n1[i])
         } ## End of loop over sites
         
     for (i in 435:445) { # when compensation is occuring
         lambda_n1_nat[i] <- dj_nat[i]*S[i]
         lambda_n1_comp[i] <- dj_comp[i]*S[i]
         n1_nat[i] ~ dpois(lambda_n1_nat[i]) 
         n1_comp[i] ~ dpois(lambda_n1_comp[i])
         n1[i] <- n1_nat[i]+n1_comp[i]
         } ## End of loop over sites

##################
### CPUE only ###
  for (i in 446:715) {
  
  ## Pass
  CPUE[i] ~ dpois(lambda_cpue[i])
  lambda_cpue[i] ~ dgamma(zeta_cpue[i],eta_cpue)I(0.001,)
  zeta_cpue[i] <- k_cpue*dj_rec[i]*eta_cpue

  ## Recent density
  dj_rec[i] <- (dj[i]*S[i])/S_rec[i]

  ## Link between recent surface and surface in 1985
  LS_rec[i] ~ dnorm(Lmu_s_rec[i],prec_s_rec)
  S_rec[i] <- exp(LS_rec[i])
  Lmu_s_rec[i] <- log(Q[i]/Q_85[i])*rho_s + log(S[i]) 
  } ## End of loop over sites

# From 2021 to end, new electrofishing gear MP -> Pulsium
# kcpue changes to k_cpue_Puls
  k_cpue_Puls <- k_cpue * k_inter ; k_inter <- exp(log_k_inter); log_k_inter ~ dnorm(0,1) # changed in 2025 from dunif(-10,10) 
  for (i in 716:I) {
  
  ## Pass
  CPUE[i] ~ dpois(lambda_cpue[i])
  lambda_cpue[i] ~ dgamma(zeta_cpue[i],eta_cpue)I(0.001,)
  zeta_cpue[i] <- k_cpue_Puls*dj_rec[i]*eta_cpue

  ## Recent density
  dj_rec[i] <- (dj[i]*S[i])/S_rec[i]

  ## Link between recent surface and surface in 1985
  LS_rec[i] ~ dnorm(Lmu_s_rec[i],prec_s_rec)
  S_rec[i] <- exp(LS_rec[i])
  Lmu_s_rec[i] <- log(Q[i]/Q_85[i])*rho_s + log(S[i]) 
  } ## End of loop over sites

# In 2021 and to 2022, 4 sites were electrofished every year with the former gear (MP) for intercalibration
# The 4 sites are :  Olha, Betrienea, Conf Sorrimenta, Zahara
# No intercalibration in 2023 and 2024 du to high flows
  r[1] <- 4; r[2] <- 9; r[3] <- 11; r[4] <- 13 # Sites numbers
  for (y in 1:2) { # incrementer pour chaque annee d'intercalibration
       for (s in 1:4) {
  ## Pass
  CPUE_inter[y,s] ~ dpois(lambda_cpue_inter[y,s])
  lambda_cpue_inter[y,s] ~ dgamma(zeta_cpue_inter[y,s],eta_cpue)I(0.001,)
  k[y,s] <- 715 + (y-1) *17 + r[s]
  zeta_cpue_inter[y,s] <- k_cpue*dj_rec[k[y,s]]*eta_cpue # calculated with k_cpue for MP
  } # End of loop over s
  } # end loop y

## DENSITY
## -----------------------------------------------------------------------------
### Natural recruitment ###
    ## Successive removals
    ## ...................
    for (i in 1:247) {
        dj[i] ~ dgamma(zeta_dj[1,Y[i],Z[i],H[i]],eta_dj[1])I(0.001,)
        } ## End of loop over sites

    ## Successive removals + CPUE
    ## ..........................
    for (i in 423:434) {
        dj[i] ~ dgamma(zeta_dj[1,Y[i],Z[i],H[i]],eta_dj[1])I(0.001,)
        } ## End of loop over sites

    ## CPUE
    ## ....
    for (i in 446:458) {
        dj[i] ~ dgamma(zeta_dj[1,Y[i],Z[i],H[i]],eta_dj[1])I(0.001,)
        } ## End of loop over sites
        
    for (i in 516:I) {
        dj[i] ~ dgamma(zeta_dj[1,Y[i],Z[i],H[i]],eta_dj[1])I(0.001,)
        } ## End of loop over sites

### Natural recruitment + Compensation ###
    ## Successive removals
    ## ...................
    for (i in 248:336) {
        dj_nat[i] ~ dgamma(zeta_dj[1,Y[i],Z[i],H[i]],eta_dj[1])I(0.001,)
        dj_comp[i] <- max(0.001,mu_dj[2,Y[i],Z[i],H[i]]) # trick to avoid error
        dj[i] <- dj_nat[i]+dj_comp[i]
        } ## End of loop over sites

    ## Successive removals + CPUE
    ## ..........................
    for (i in 435:445) {
        dj_nat[i] ~ dgamma(zeta_dj[1,Y[i],Z[i],H[i]],eta_dj[1])I(0.001,)
        dj_comp[i] <- max(0.001,mu_dj[2,Y[i],Z[i],H[i]])
        dj[i] <- dj_nat[i]+dj_comp[i]
        } ## End of loop over sites

    ## CPUE
    ## ....
    for (i in 459:515) {
        dj_nat[i] ~ dgamma(zeta_dj[1,Y[i],Z[i],H[i]],eta_dj[1])I(0.001,)
        dj_comp[i] <- max(0.001,mu_dj[2,Y[i],Z[i],H[i]])
        dj[i] <- dj_nat[i]+dj_comp[i]
        } ## End of loop over sites

### Restocking ###
  for (i in 337:422) {
      dj[i] ~ dgamma(zeta_dj[3,Y[i],Z[i],H[i]],eta_dj[3])I(0.001,)
      } ## End of loop over sites

## EXTRAPOLATION
## |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
## NOT SAMPLED SITES
## -----------------------------------------------------------------------------
## lambda_j_ns_riff is the mean of the Poisson distributed count per year (y) and per zone (z) for riffle habitat when only natural recruitment occurs
## lambda_jnat_ns_riff is the mean of the Poisson distributed count per year (y) and per zone (z) for riffle habitat produced by natural recruitment when both natural recruitment and compensation occur.
## lambda_jcomp_ns_riff is the mean of the Poisson distributed count per year (y) and per zone (z) for riffle habitat produced by released individuals when both natural recruitment and compensation occur.
## jnat_ns_riff is the number of juveniles issued from natural recruitment in riffle per year and per zone
## jcomp_ns_riff is the number of juveniles produced by released individuals when doing compensation in riffle per year and per zone
## jres_ns_riff is the number of juveniles produced by individuals when doing restocking in riffle per year and per zone
## jnat_ns is the total number of juveniles produced by natural recruitment (ie, sum of individuals produced in riffles and in runs)
## j_ns is the total number of juveniles produced (sum of natural recruitment and released individuals in riffles and runs)
## jres_ns is the total number of juveniles produced when doing restocking (ie, sum of individuals produced in riffles and in runs)
#########################
## Natural recruitment
## ------------------
  for (y in 2:7) { ## LN from 1985 to 1990
  ## Riffles (habitat = 1)
     lambda_j_ns_riff[y,3] <- mu_dj[1,y,3,1]*Stot_ns_riff[y,3]
     jnat_ns_riff[y,3] ~ dpois(lambda_j_ns_riff[y,3])
  ## Runs (habitat = 2)
     lambda_j_ns_runs[y,3] <- mu_dj[1,y,3,2]*Stot_ns_runs[y,3]
     jnat_ns_runs[y,3] ~ dpois(lambda_j_ns_runs[y,3])

     jnat_ns[y,3] <- jnat_ns_riff[y,3] + jnat_ns_runs[y,3]
     } ## End of loop over years

  for (z in 3:5) { ## LN+HN+LUR from 1991 to 1995
    for (y in 8:12) {
    ## Riffles
       lambda_j_ns_riff[y,z] <- mu_dj[1,y,z,1]*Stot_ns_riff[y,z]
       jnat_ns_riff[y,z] ~ dpois(lambda_j_ns_riff[y,z])
    ## Runs
       lambda_j_ns_runs[y,z] <- mu_dj[1,y,z,2]*Stot_ns_runs[y,z]
       jnat_ns_runs[y,z] ~ dpois(lambda_j_ns_runs[y,z])

       jnat_ns[y,z] <- jnat_ns_riff[y,z] + jnat_ns_runs[y,z]
       } ## End of loop over years
       
    ## Riffles for years: 1997, 2001, 2003
    lambda_j_ns_riff[14,z] <- mu_dj[1,14,z,1]*Stot_ns_riff[14,z]
    jnat_ns_riff[14,z] ~ dpois(lambda_j_ns_riff[14,z])
    lambda_j_ns_riff[18,z] <- mu_dj[1,18,z,1]*Stot_ns_riff[18,z]
    jnat_ns_riff[18,z] ~ dpois(lambda_j_ns_riff[18,z])
    lambda_j_ns_riff[20,z] <- mu_dj[1,20,z,1]*Stot_ns_riff[20,z]
    jnat_ns_riff[20,z] ~ dpois(lambda_j_ns_riff[20,z])
    ## Runs for years: 1997, 2001, 2003
    lambda_j_ns_runs[14,z] <- mu_dj[1,14,z,2]*Stot_ns_runs[14,z]
    jnat_ns_runs[14,z] ~ dpois(lambda_j_ns_runs[14,z])
    lambda_j_ns_runs[18,z] <- mu_dj[1,18,z,2]*Stot_ns_runs[18,z]
    jnat_ns_runs[18,z] ~ dpois(lambda_j_ns_runs[18,z])
    lambda_j_ns_runs[20,z] <- mu_dj[1,20,z,2]*Stot_ns_runs[20,z]
    jnat_ns_runs[20,z] ~ dpois(lambda_j_ns_runs[20,z])
    
    jnat_ns[14,z] <- jnat_ns_riff[14,z] + jnat_ns_runs[14,z]
    jnat_ns[18,z] <- jnat_ns_riff[18,z] + jnat_ns_runs[18,z]
    jnat_ns[20,z] <- jnat_ns_riff[20,z] + jnat_ns_runs[20,z]
    
    for (y in 27:Y_last) { # from 2010 to now on
        ## Riffles
        lambda_j_ns_riff[y,z] <- mu_dj[1,y,z,1]*Stot_ns_riff[y,z]
        jnat_ns_riff[y,z] ~ dpois(lambda_j_ns_riff[y,z])
        ## Runs
        lambda_j_ns_runs[y,z] <- mu_dj[1,y,z,2]*Stot_ns_runs[y,z]
        jnat_ns_runs[y,z] ~ dpois(lambda_j_ns_runs[y,z])

        jnat_ns[y,z] <- jnat_ns_riff[y,z]+jnat_ns_runs[y,z]
        } ## End of loop over years
    } ## End of loop over zones

  for (y in 21:23) { # LN from 2004 to 2006
    ## Riffles
    lambda_j_ns_riff[y,3] <- mu_dj[1,y,3,1]*Stot_ns_riff[y,3]
    jnat_ns_riff[y,3] ~ dpois(lambda_j_ns_riff[y,3])
    ## Runs
    lambda_j_ns_runs[y,3] <- mu_dj[1,y,3,2]*Stot_ns_runs[y,3]
    jnat_ns_runs[y,3] ~ dpois(lambda_j_ns_runs[y,3])

    jnat_ns[y,3] <- jnat_ns_riff[y,3]+jnat_ns_runs[y,3]
    } ## End of loop over years
    
  ## Riffles
  ## LN: Year 2000
  lambda_j_ns_riff[17,3] <- mu_dj[1,17,3,1]*Stot_ns_riff[17,3]
  jnat_ns_riff[17,3] ~ dpois(lambda_j_ns_riff[17,3])
  ## LUR: Year 2000
  lambda_j_ns_riff[17,5] <- mu_dj[1,17,5,1]*Stot_ns_riff[17,5]
  jnat_ns_riff[17,5] ~ dpois(lambda_j_ns_riff[17,5])
  ## LN: Year 2002
  lambda_j_ns_riff[19,3] <- mu_dj[1,19,3,1]*Stot_ns_riff[19,3]
  jnat_ns_riff[19,3] ~ dpois(lambda_j_ns_riff[19,3])
  # LUR: Year 2002
  lambda_j_ns_riff[23,5] <- mu_dj[1,23,5,1]*Stot_ns_riff[23,5]
  jnat_ns_riff[23,5] ~ dpois(lambda_j_ns_riff[23,5])
  # HN: Year 2009 
  lambda_j_ns_riff[26,4] <- mu_dj[1,26,4,1]*Stot_ns_riff[26,4]
  jnat_ns_riff[26,4] ~ dpois(lambda_j_ns_riff[26,4])
  # LUR: Year 2009
  lambda_j_ns_riff[26,5] <- mu_dj[1,26,5,1]*Stot_ns_riff[26,5]
  jnat_ns_riff[26,5] ~ dpois(lambda_j_ns_riff[26,5])
  
  ## Runs
  ## LN: Year 2000
  lambda_j_ns_runs[17,3] <- mu_dj[1,17,3,2]*Stot_ns_runs[17,3]
  jnat_ns_runs[17,3] ~ dpois(lambda_j_ns_runs[17,3])
  ## LUR: Year 2000 
  lambda_j_ns_runs[17,5] <- mu_dj[1,17,5,2]*Stot_ns_runs[17,5]
  jnat_ns_runs[17,5] ~ dpois(lambda_j_ns_runs[17,5])
  ## LN: Year 2002
  lambda_j_ns_runs[19,3] <- mu_dj[1,19,3,2]*Stot_ns_runs[19,3]
  jnat_ns_runs[19,3] ~ dpois(lambda_j_ns_runs[19,3])
  # LUR: Year 2002 
  lambda_j_ns_runs[23,5] <- mu_dj[1,23,5,2]*Stot_ns_runs[23,5]
  jnat_ns_runs[23,5] ~ dpois(lambda_j_ns_runs[23,5])
  # HN: Year 2009
  lambda_j_ns_runs[26,4] <- mu_dj[1,26,4,2]*Stot_ns_runs[26,4]
  jnat_ns_runs[26,4] ~ dpois(lambda_j_ns_runs[26,4])
  # LUR: Year 2009 
  lambda_j_ns_runs[26,5] <- mu_dj[1,26,5,2]*Stot_ns_runs[26,5]
  jnat_ns_runs[26,5] ~ dpois(lambda_j_ns_runs[26,5])
  
  jnat_ns[17,3] <- jnat_ns_riff[17,3]+jnat_ns_runs[17,3]
  jnat_ns[17,5] <- jnat_ns_riff[17,5]+jnat_ns_runs[17,5]
  jnat_ns[19,3] <- jnat_ns_riff[19,3]+jnat_ns_runs[19,3]
  jnat_ns[23,5] <- jnat_ns_riff[23,5]+jnat_ns_runs[23,5]
  jnat_ns[26,4] <- jnat_ns_riff[26,4]+jnat_ns_runs[26,4]
  jnat_ns[26,5] <- jnat_ns_riff[26,5]+jnat_ns_runs[26,5]

    for (y in 41:Y_last) { # from 2024 to now on the Spanish Nivelle contributes to natural recruitment
        ## Riffles
        lambda_j_ns_riff[y,7] <- mu_dj[1,y,7,1]*Stot_ns_riff[y,7]
        jnat_ns_riff[y,7] ~ dpois(lambda_j_ns_riff[y,7])
        ## Runs
        lambda_j_ns_runs[y,7] <- mu_dj[1,y,7,2]*Stot_ns_runs[y,7]
        jnat_ns_runs[y,7] ~ dpois(lambda_j_ns_runs[y,7])

        jnat_ns[y,7] <- jnat_ns_riff[y,7]+jnat_ns_runs[y,7]
        } ## End of loop over years

#########################################
## Natural recruitment + Compensation
## ----------------------------------
  for (z in 3:5) { # LN, HN, LUR
      ## Riffles, year 1996
      lambda_jnat_ns_riff[13,z] <- mu_dj[1,13,z,1]*Stot_ns_riff[13,z]
      jnat_ns_riff[13,z] ~ dpois(lambda_jnat_ns_riff[13,z])
      lambda_jcomp_ns_riff[13,z] <- mu_dj[2,13,z,1]*Stot_ns_riff[13,z]
      jcomp_ns_riff[13,z] ~ dpois(lambda_jcomp_ns_riff[13,z])
      ## Runs, year 1996 
      lambda_jnat_ns_runs[13,z] <- mu_dj[1,13,z,2]*Stot_ns_runs[13,z]
      jnat_ns_runs[13,z] ~ dpois(lambda_jnat_ns_runs[13,z])
      lambda_jcomp_ns_runs[13,z] <- mu_dj[2,13,z,2]*Stot_ns_runs[13,z]
      jcomp_ns_runs[13,z] ~ dpois(lambda_jcomp_ns_runs[13,z])
      
      j_nat_ns[13,z] <- jnat_ns_riff[13,z]+jnat_ns_runs[13,z]
      j_comp_ns[13,z] <- jcomp_ns_riff[13,z]+jcomp_ns_runs[13,z]
      j_ns[13,z] <- j_nat_ns[13,z]+j_comp_ns[13,z]
      
      for (y in 15:16) { # Year 1998 & 1999
          ## Riffles
          lambda_jnat_ns_riff[y,z] <- mu_dj[1,y,z,1]*Stot_ns_riff[y,z]
          jnat_ns_riff[y,z] ~ dpois(lambda_jnat_ns_riff[y,z])
          lambda_jcomp_ns_riff[y,z] <- mu_dj[2,y,z,1]*Stot_ns_riff[y,z]
          jcomp_ns_riff[y,z] ~ dpois(lambda_jcomp_ns_riff[y,z])
          ## Runs
          lambda_jnat_ns_runs[y,z] <- mu_dj[1,y,z,2]*Stot_ns_runs[y,z]
          jnat_ns_runs[y,z] ~ dpois(lambda_jnat_ns_runs[y,z])
          lambda_jcomp_ns_runs[y,z] <- mu_dj[2,y,z,2]*Stot_ns_runs[y,z]
          jcomp_ns_runs[y,z] ~ dpois(lambda_jcomp_ns_runs[y,z])

          j_nat_ns[y,z] <- jnat_ns_riff[y,z]+jnat_ns_runs[y,z]
          j_comp_ns[y,z] <- jcomp_ns_riff[y,z]+jcomp_ns_runs[y,z]
          j_ns[y,z] <- j_nat_ns[y,z]+j_comp_ns[y,z]
          } ## End of loop over years
          
      for (y in 24:25) { # Year 2007 & 2008
          ## Riffles
          lambda_jnat_ns_riff[y,z] <- mu_dj[1,y,z,1]*Stot_ns_riff[y,z]
          jnat_ns_riff[y,z] ~ dpois(lambda_jnat_ns_riff[y,z])
          lambda_jcomp_ns_riff[y,z] <- mu_dj[2,y,z,1]*Stot_ns_riff[y,z]
          jcomp_ns_riff[y,z] ~ dpois(lambda_jcomp_ns_riff[y,z])
          ## Runs
          lambda_jnat_ns_runs[y,z] <- mu_dj[1,y,z,2]*Stot_ns_runs[y,z]
          jnat_ns_runs[y,z] ~ dpois(lambda_jnat_ns_runs[y,z])
          lambda_jcomp_ns_runs[y,z] <- mu_dj[2,y,z,2]*Stot_ns_runs[y,z]
          jcomp_ns_runs[y,z] ~ dpois(lambda_jcomp_ns_runs[y,z])

          j_nat_ns[y,z] <- jnat_ns_riff[y,z]+jnat_ns_runs[y,z]
          j_comp_ns[y,z] <- jcomp_ns_riff[y,z]+jcomp_ns_runs[y,z]
          j_ns[y,z] <- j_nat_ns[y,z]+j_comp_ns[y,z]
          } ## End of loop over years
  } ## End of loop over zones

  ## Riffles for HN: Year 2000
  lambda_jnat_ns_riff[17,4] <- mu_dj[1,17,4,1]*Stot_ns_riff[17,4]
  jnat_ns_riff[17,4] ~ dpois(lambda_jnat_ns_riff[17,4])
  lambda_jcomp_ns_riff[17,4] <- mu_dj[2,17,4,1]*Stot_ns_riff[17,4]
  jcomp_ns_riff[17,4] ~ dpois(lambda_jcomp_ns_riff[17,4])
  ## Runs for HN: Year 2000
  lambda_jnat_ns_runs[17,4] <- mu_dj[1,17,4,2]*Stot_ns_runs[17,4]
  jnat_ns_runs[17,4] ~ dpois(lambda_jnat_ns_runs[17,4])
  lambda_jcomp_ns_runs[17,4] <- mu_dj[2,17,4,2]*Stot_ns_runs[17,4]
  jcomp_ns_runs[17,4] ~ dpois(lambda_jcomp_ns_runs[17,4])

  j_nat_ns[17,4] <- jnat_ns_riff[17,4]+jnat_ns_runs[17,4]
  j_comp_ns[17,4] <- jcomp_ns_riff[17,4]+jcomp_ns_runs[17,4]
  j_ns[17,4] <- j_nat_ns[17,4]+j_comp_ns[17,4]
  
  for (z in 4:5) { # HN & LUR Year 2002
      ## Riffles
      lambda_jnat_ns_riff[19,z] <- mu_dj[1,19,z,1]*Stot_ns_riff[19,z]
      jnat_ns_riff[19,z] ~ dpois(lambda_jnat_ns_riff[19,z])
      lambda_jcomp_ns_riff[19,z] <- mu_dj[2,19,z,1]*Stot_ns_riff[19,z]
      jcomp_ns_riff[19,z] ~ dpois(lambda_jcomp_ns_riff[19,z]) 
      ## Runs
      lambda_jnat_ns_runs[19,z] <- mu_dj[1,19,z,2]*Stot_ns_runs[19,z]
      jnat_ns_runs[19,z] ~ dpois(lambda_jnat_ns_runs[19,z]) 
      lambda_jcomp_ns_runs[19,z] <- mu_dj[2,19,z,2]*Stot_ns_runs[19,z]
      jcomp_ns_runs[19,z] ~ dpois(lambda_jcomp_ns_runs[19,z]) 

      j_nat_ns[19,z] <- jnat_ns_riff[19,z]+jnat_ns_runs[19,z]
      j_comp_ns[19,z] <- jcomp_ns_riff[19,z]+jcomp_ns_runs[19,z]
      j_ns[19,z] <- j_nat_ns[19,z]+j_comp_ns[19,z]
      
      for (y in 21:22) { # Year 2004 & 2005
          ## Riffles
          lambda_jnat_ns_riff[y,z] <- mu_dj[1,y,z,1]*Stot_ns_riff[y,z]
          jnat_ns_riff[y,z] ~ dpois(lambda_jnat_ns_riff[y,z])
          lambda_jcomp_ns_riff[y,z] <- mu_dj[2,y,z,1]*Stot_ns_riff[y,z]
          jcomp_ns_riff[y,z] ~ dpois(lambda_jcomp_ns_riff[y,z])
          ## Runs
          lambda_jnat_ns_runs[y,z] <- mu_dj[1,y,z,2]*Stot_ns_runs[y,z]
          jnat_ns_runs[y,z] ~ dpois(lambda_jnat_ns_runs[y,z])
          lambda_jcomp_ns_runs[y,z] <- mu_dj[2,y,z,2]*Stot_ns_runs[y,z]
          jcomp_ns_runs[y,z] ~ dpois(lambda_jcomp_ns_runs[y,z])

          j_nat_ns[y,z] <- jnat_ns_riff[y,z]+jnat_ns_runs[y,z]
          j_comp_ns[y,z] <- jcomp_ns_riff[y,z]+jcomp_ns_runs[y,z]
          j_ns[y,z] <- j_nat_ns[y,z]+j_comp_ns[y,z]
          } ## End of loop over years
  } ## End of loop over zones

  ## Riffles for HN, Year 2006 & 2009
  lambda_jnat_ns_riff[23,4] <- mu_dj[1,23,4,1]*Stot_ns_riff[23,4]
  jnat_ns_riff[23,4] ~ dpois(lambda_jnat_ns_riff[23,4])
  lambda_jcomp_ns_riff[23,4] <- mu_dj[2,23,4,1]*Stot_ns_riff[23,4]
  jcomp_ns_riff[23,4] ~ dpois(lambda_jcomp_ns_riff[23,4])
  lambda_jnat_ns_riff[26,3] <- mu_dj[1,26,3,1]*Stot_ns_riff[26,3]
  jnat_ns_riff[26,3] ~ dpois(lambda_jnat_ns_riff[26,3])
  lambda_jcomp_ns_riff[26,3] <- mu_dj[2,26,3,1]*Stot_ns_riff[26,3]
  jcomp_ns_riff[26,3] ~ dpois(lambda_jcomp_ns_riff[26,3])
  ## Runs
  lambda_jnat_ns_runs[23,4] <- mu_dj[1,23,4,2]*Stot_ns_runs[23,4]
  jnat_ns_runs[23,4] ~ dpois(lambda_jnat_ns_runs[23,4])
  lambda_jcomp_ns_runs[23,4] <- mu_dj[2,23,4,2]*Stot_ns_runs[23,4]
  jcomp_ns_runs[23,4] ~ dpois(lambda_jcomp_ns_runs[23,4])
  lambda_jnat_ns_runs[26,3] <- mu_dj[1,26,3,2]*Stot_ns_runs[26,3]
  jnat_ns_runs[26,3] ~ dpois(lambda_jnat_ns_runs[26,3])
  lambda_jcomp_ns_runs[26,3] <- mu_dj[2,26,3,2]*Stot_ns_runs[26,3]
  jcomp_ns_runs[26,3] ~ dpois(lambda_jcomp_ns_runs[26,3])

  j_nat_ns[23,4] <- jnat_ns_riff[23,4]+jnat_ns_runs[23,4]
  j_comp_ns[23,4] <- jcomp_ns_riff[23,4]+jcomp_ns_runs[23,4]
  j_ns[23,4] <- j_nat_ns[23,4]+j_comp_ns[23,4]
  j_nat_ns[26,3] <- jnat_ns_riff[26,3]+jnat_ns_runs[26,3]
  j_comp_ns[26,3] <- jcomp_ns_riff[26,3]+jcomp_ns_runs[26,3]
  j_ns[26,3] <- j_nat_ns[26,3]+j_comp_ns[26,3]

###################  
## Stocking
## ----------
  for (z in 4:5) { ## HN + LUR
    for (y in 3:7) { # Year 1986 to 1990
        ## Riffles
        lambda_j_ns_riff[y,z] <- mu_dj[3,y,z,1]*Stot_ns_riff[y,z]
        jres_ns_riff[y,z] ~ dpois(lambda_j_ns_riff[y,z])
        ## Runs
        lambda_j_ns_runs[y,z] <- mu_dj[3,y,z,2]*Stot_ns_runs[y,z]
        jres_ns_runs[y,z] ~ dpois(lambda_j_ns_runs[y,z])

        jres_ns[y,z] <- jres_ns_riff[y,z]+jres_ns_runs[y,z]
        } ## End of loop over years
  } ## End of loop over zones

  ## VHN
  for (y in 6:9) { # Year 1989 to 1992
      ## Riffles
      lambda_j_ns_riff[y,7] <- mu_dj[3,y,7,1]*Stot_ns_riff[y,7]
      jres_ns_riff[y,7] ~ dpois(lambda_j_ns_riff[y,7])
      ## Runs
      lambda_j_ns_runs[y,7] <- mu_dj[3,y,7,2]*Stot_ns_runs[y,7]
      jres_ns_runs[y,7] ~ dpois(lambda_j_ns_runs[y,7])

      jres_ns[y,7] <- jres_ns_riff[y,7]+jres_ns_runs[y,7]
      } ## End of loop over years
      
  ## Riffles for VHN, year 1994
  lambda_j_ns_riff[11,7] <- mu_dj[3,11,7,1]*Stot_ns_riff[11,7]
  jres_ns_riff[11,7] ~ dpois(lambda_j_ns_riff[11,7])
  ## Runs for VHN, year 1994 
  lambda_j_ns_runs[11,7] <- mu_dj[3,11,7,2]*Stot_ns_runs[11,7]
  jres_ns_runs[11,7] ~ dpois(lambda_j_ns_runs[11,7])

  jres_ns[11,7] <- jres_ns_riff[11,7]+jres_ns_runs[11,7]

  ## LAP for 1994 & 1995
  for (y in 11:12) {
      ## Riffles
      lambda_j_ns_riff[y,8] <- mu_dj[3,y,8,1]*Stot_ns_riff[y,8]
      jres_ns_riff[y,8] ~ dpois(lambda_j_ns_riff[y,8])
      ## Runs
      lambda_j_ns_runs[y,8] <- mu_dj[3,y,8,2]*Stot_ns_runs[y,8]
      jres_ns_runs[y,8] ~ dpois(lambda_j_ns_runs[y,8])

      jres_ns[y,8] <- jres_ns_riff[y,8]+jres_ns_runs[y,8]
      } ## End of loop over years
      
  ## Riffles for LAP year 1989
  lambda_j_ns_riff[6,8] <- mu_dj[3,6,8,1]*Stot_ns_riff[6,8]
  jres_ns_riff[6,8] ~ dpois(lambda_j_ns_riff[6,8])
  ## Runs
  lambda_j_ns_runs[6,8] <- mu_dj[3,6,8,2]*Stot_ns_runs[6,8]
  jres_ns_runs[6,8] ~ dpois(lambda_j_ns_runs[6,8])

  jres_ns[6,8] <- jres_ns_riff[6,8]+jres_ns_runs[6,8]
} ## Fin du model  
