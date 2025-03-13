################################################################################
###           Model of CMR data to estimate parr population size             ###
###                 of Salmo salar in Oir river + tributary rivers.          ###
###   (Intercalibration between CPUE and successive removals included)       ###
###                  Sabrina Servanty & Etienne Pr?vost                      ###
###                          November 2015                                   ###
################################################################################

################################################################################
## This model is higly constrained 
## There was huge trouble to get appropriate estimates and convergence for probability of capture during successive removals.
## This was due to: high variance in probability of capture due to the fact that p_rem1 is very low for sites where generally 2 passes are done (not so many fishes captured) whereas usually when only one pass is done, we've got a high number of fish caught. 
## But we know that is not true for salmonids to get a low probability of capture by electrofishing. => Constraining variance: half cauchy * 0.25
## Need at the end to estimate a probability of capture by group and year (not possible to get an estimate by site)
## Log Density is constrained to avoid too low and too high value
## Year effect in density is constrained too avoid too high value
## /!\ Unit of density and of surface of rapids or runs are in 100m?, not in m?. Very hard to get update otherwise because samplers are sampling very low values 
################################################################################

################################################################################
## DATA:
## Nyear: Number of years since the beginning of the study in 1987
## NPE1: Total number of samples by PE or PTE (number of lines) for the first pass
## NIAno: Total number of samples by CPUE for sites not used for intercalibration (number of lines)
## NIAinter: Total number of samples by CPUE for sites used for intercalibration (number of lines)
## Ngroup: Number of groups considered within the river and its tributary rivers (13 groups)
## NIA: Number of sites sampled by CPUE (11 considered)
## Ninter: Number of sites used to built the relationship between successive removal and CPUE (6 considered)
## C1[i]: Number of 0+ captured by electric fishing during the first pass
## C2[i]: Number of 0+ captured by electric fishing during the second pass
## Site[i]: Index of sites sampled by successive removal
## Group[i]: Index of group for sites sampled by successive removal
## Lieu[i]: Localisation of sites sampled by successive removal (Oir or tributary rivers)
## Srr[i]: Surface of rapid for each site sampled by successive removal in 100m?
## Spl[i]: Surface of run (plat courant et plat lent) for each site sampled by successive removal (in 100m?)
## CPUE_IAno[j]: Number of 0+ captured by CPUE for sites not used to build the relationship between successive removal and CPUE
## Site_IA[j]: index of sites sampled by CPUE but not used to build the relationship among all sampled sites (whatever the sampling method is)
## IAno_num[j]: index of sites sampled by CPUE but not used to build the relationship among sites sampled by CPUE
## Srr_IA[j]: Surface of rapid for each site sampled by CPUE but not used to  build the relationship (in 100m?)  
## Group_IA[j]: Index of group for sites sampled by CPUE but not used to  build the relationship
## CPUE_inter[m]: Number of 0+ captured by CPUE for sites used to build the relationship between successive removal and CPUE
## IAinter_num[m]: index of sites sampled by CPUE and used to build the relationship among sites sampled by CPUE 
## Site_IAinter[m]: index of sites sampled by CPUE and used to build the relationship among all sampled sites (whatever the sampling method is) 
## Srr_IAinter[m]: Surface of rapid for each site sampled by CPUE and used to  build the relationship (in 100m?) 
## Lieu_IAinter[m]: Localisation of sites sampled by CPUE and used to build the relationship(Oir or tributary rivers)
## Group_IAinter[m]: Index of group for sites sampled by CPUE and used to  build the relationship 
## W_Oir[k]: Mean width of sites sampled by CPUE
## StotPC[g]: surface of run (in 100m?) for each group
## StotRR[g]: surface of rapids (in 100m?) for each group
## Srr_inter[]: surface of rapids (in 100m?) of sites sampled by successive removal and used to built the relationship between successive removal and CPUE 
################################################################################## 
## Used indices:
## r: river (1: Oir, 2: La Roche, 3: Pont Levesque, 4: Moulin du Bois)
## g: group
## t : year
## i: sites sampled by successive removal
## j: sites sampled by CPUE and not used in intercalibration
## m: sites sampled by CPUE and used in intercalibration
## k: sites sampled by CPUE (used or not in intercalibration)
################################################################################## 

model {

###############################
# HYPERPARAMETER
##############################
###############################
# Successive removal (on logit scale): Priors are used to induce a uniform prior between 0 and 1 on the probability scale. 
cauchy ~ dt(0,1,1)  # Cauchy distribution
sd_prem <- abs(cauchy) * 0.25  # half-Cauchy distribution: overall sd of probability of capture during successive removals
tau_prem <- 1/(sd_prem * sd_prem) # precision of probability of capture during successive removals

mup_rem ~ dbeta(2,1) # mean of probability of capture during successive removals (probability scale).
lmup_rem <- log(mup_rem/(1-mup_rem))# logit transformation

### CPUE
int_width ~ dunif(-10,10) # constant linked to the relationship with width
int_width_cut <- cut(int_width) ## This parameter is going to be used to link density with CPUE for sites only sampled by CPUE

width_coef ~ dunif(-10,10) # factor of proportionality with river's width
width_coef_cut <- cut(width_coef) ## This parameter is going to be used to link density with CPUE for sites only sampled by CPUE

rate_lcpu ~ dgamma(0.01,0.01) # rate (inverse scale) of the gamma distribution for lambda_cpu
rate_lcpu_cut <- cut(rate_lcpu) ## This parameter is going to be used to link density with CPUE for sites only sampled by CPUE

########################
## Density
sigma_dOir ~ dunif(0,5) ## Overall standard deviation in density in Oir and tributary rivers
var_dOir <- sigma_dOir*sigma_dOir
tau_dOir <- 1/var_dOir

#### Year effect (on log scale)
sigma_yOir ~ dunif(0,5) ## Standard deviation of year effect
var_yOir <- sigma_yOir*sigma_yOir
tau_yOir <- 1/var_yOir

for (r in 1:4) { # 1: Oir, 2: La Roche, 3: Pont Levesque, 4: Moulin du Bois
    mu_ydOir[r] ~ dunif(-10,10) ## Mean annual density
    } # end of looop over Oir river and tributary rivers  
    
for (t in 1:Nyear) {
    year_dOir[t] ~ dnorm(0,tau_yOir)I(,2.3) ## year effect. Setting up a constraint to not get log(10) times the mean density. 
    } # end of loop over years

# Statistics
diff1 <- mu_ydOir[1] - mu_ydOir[2] # is mean density in Oir river superior to mean density in La Roche
diff2 <- mu_ydOir[1] - mu_ydOir[3] # is mean density in Oir river superior to mean density in Pont Levesque
diff3 <- mu_ydOir[1] - mu_ydOir[4] # is mean density in Oir river superior to mean density in Moulin du Bois
diff4 <- mu_ydOir[2] - mu_ydOir[3] # is mean density in La Roche superior to mean density in Pont Levesque
diff5 <- mu_ydOir[2] - mu_ydOir[4] # is mean density in La Roche superior to mean density in Moulin du Bois
diff6 <- mu_ydOir[3] - mu_ydOir[4] # is mean density in Pont Levesque superior to mean density in Moulin du Bois

test[1] <- step(diff1) # equal to one if superior to 0
test[2] <- step(diff2)
test[3] <- step(diff3) 
test[4] <- step(diff4)
test[5] <- step(diff5) 
test[6] <- step(diff6)

## Interaction term between group of sites and year
sigma_gryrOir ~ dunif(0,5) ## Standard deviation 
var_gryrOir <- sigma_gryrOir*sigma_gryrOir
tau_gryrOir <- 1/var_gryrOir

for (g in 1:(Ngroup*Nyear)) {
    gryr_Oir[g] ~ dnorm(0,tau_gryrOir)
    } # end of loop over sites

###############################
## Density per group and per year
## Building matrix for interaction between group and year (/!\ Here CPUE sites are included)
for (t in 1:Nyear) {
    Group_an[1,t] <- t   # Group 1: Oir1.2, 1.3, 1.4, IAS02
    Group_an[2,t] <- Nyear + t # Group 2: Oir2.1, PTE03, IAS03
    Group_an[3,t] <- (Nyear * 2) + t  # Group 3: Oir2.5, Oir 2.6, Oir2.7, Oir 2.8, PTE04, IAS04
    Group_an[4,t] <- (Nyear * 3) + t # Group 4: Oir3.2, Oir3.3'
    Group_an[5,t] <- (Nyear * 4) + t # Group 5: Oir3.4', Oir 3.5
    Group_an[6,t] <- (Nyear * 5) + t # Group 6: Oir4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, IAS05, IS06
    Group_an[7,t] <- (Nyear * 6) + t # Group 7: Oir5.2a, IAS07
    Group_an[8,t] <- (Nyear * 7) + t # Group 8: Oir5.2b, 5.3, 5.4, 5.5, 5.6, 5.7, IAS08
    Group_an[9,t] <- (Nyear * 8) + t  # Group 9: PTE05, IAS09
    Group_an[10,t] <- (Nyear * 9) + t # Group 10: IAS10
    Group_an[11,t] <- (Nyear * 10) + t  # Group 11: Moulin du bois (mbam1, mbam2, mbav1, mbav2)
    Group_an[12,t] <- (Nyear * 11) + t  #Group 12: Pont Levesque: plam1, plam2, plav1, plav2, IAS11
    Group_an[13,t] <- (Nyear * 12) + t #Group 13: La Roche RR01 ? RR20
    } # end of loop over years

########################
### Coef_PC is the coefficient of proportionality for run.
coef_PC ~ dunif(0,1)
coef_PC_cut <- cut(coef_PC) # to use it for extrapolation to the whole rivers (Oir and tributary) 

##############################
# Density per station and per year
####################################
for (i in 1:NPE1) { # density for sites sampled by successive removal
    log_mudOir[Site[i],Year[i]] <- mu_ydOir[Lieu[i]] + year_dOir[Year[i]] + gryr_Oir[Group_an[Group[i],Year[i]]]
    log_dOir[Site[i],Year[i]] ~ dnorm(log_mudOir[Site[i],Year[i]],tau_dOir)I(-10,6)
        
    # Back transformation on natural scale and transformation in equivalent rapid
    dRR_Oir[Site[i],Year[i]] <- exp(log_dOir[Site[i],Year[i]]) # density in rapid equivalent
    dobs_Oir[Site[i],Year[i]] <- dRR_Oir[Site[i],Year[i]] * ((Srr[i] + coef_PC * Spl[i])/(Srr[i] + Spl[i])) # observed density (for both habitat in the site)
        
    eps_dOir[Site[i],Year[i]] <- (log_dOir[Site[i],Year[i]]-log_mudOir[Site[i],Year[i]])/sigma_dOir ## standardized residuals on observed density in equivalent rapid

    ## Abundance
    ntot[Site[i],Year[i]] ~ dpois(lambda_n[Site[i],Year[i]])
    lambda_n[Site[i],Year[i]] <- dobs_Oir[Site[i],Year[i]] * (Srr[i]+Spl[i])
    } # end of loop over PE or PTE samples 

for (j in 1: NIAno) {  # density for sites sampled by IA and not used in the intercalibration
    log_mudOir[Site_IA[j],Year_IA[j]] <- mu_ydOir[1] + year_dOir[Year_IA[j]] + gryr_Oir[Group_an[Group_IA[j],Year_IA[j]]]
    log_dOir[Site_IA[j],Year_IA[j]] ~ dnorm(log_mudOir[Site_IA[j],Year_IA[j]],tau_dOir)I(-10,6)
        
    # Back transformation on natural scale and transformation in equivalent rapid
    dRR_Oir[Site_IA[j],Year_IA[j]] <- exp(log_dOir[Site_IA[j],Year_IA[j]]) # density in rapid equivalent
    dIA_Oir[IAno_num[j],Year_IA[j]] <- dRR_Oir[Site_IA[j],Year_IA[j]] ## to use in the intercalibration

    eps_dOir[Site_IA[j],Year_IA[j]] <- (log_dOir[Site_IA[j],Year_IA[j]]-log_mudOir[Site_IA[j],Year_IA[j]])/sigma_dOir ## standardized residuals on observed density in equivalent rapid

    ## Abundance
    ntot[Site_IA[j],Year_IA[j]] ~ dpois(lambda_n[Site_IA[j],Year_IA[j]])
    lambda_n[Site_IA[j],Year_IA[j]] <- dRR_Oir[Site_IA[j],Year_IA[j]] * Srr_IA[j]
    } # end of loop over CPUE for sites not used in intercalibration

for (m in 76:NIAinter) { # from 2009 until now, density for sites sampled by IA and that were used for intercalibration.
    log_mudOir[Site_IAinter[m],Year_IAinter[m]] <- mu_ydOir[Lieu_IAinter[m]]+ year_dOir[Year_IAinter[m]] + gryr_Oir[Group_an[Group_IAinter[m],Year_IAinter[m]]]
    log_dOir[Site_IAinter[m],Year_IAinter[m]] ~ dnorm(log_mudOir[Site_IAinter[m],Year_IAinter[m]],tau_dOir)I(-10,6)

    # Back transformation on natural scale and transformation in equivalent rapid
    dRR_Oir[Site_IAinter[m],Year_IAinter[m]] <- exp(log_dOir[Site_IAinter[m],Year_IAinter[m]]) # density in rapid equivalent
    dIA_Oir[IAinter_num[m],Year_IAinter[m]] <- dRR_Oir[Site_IAinter[m],Year_IAinter[m]] ## to use in the intercalibration

    eps_dOir[Site_IAinter[m],Year_IAinter[m]] <- (log_dOir[Site_IAinter[m],Year_IAinter[m]]-log_mudOir[Site_IAinter[m],Year_IAinter[m]])/sigma_dOir ## standardized residuals on observed density in equivalent rapid
    
    # Abundance
    ntot[Site_IAinter[m],Year_IAinter[m]] ~ dpois(lambda_n[Site_IAinter[m],Year_IAinter[m]])
    lambda_n[Site_IAinter[m],Year_IAinter[m]] <- dIA_Oir[IAinter_num[m],Year_IAinter[m]] * Srr_IAinter[m]
    } # end of loop over CPUE for sites used in intercalibration
    
###################################
# Successive removal 
#################################
### Probability of capture (group effect)
for (g in 1:Ngroup) { # Group of sites (Oir river + tributary rivers)
    for (t in 1:Nyear) { 
    lp_remgr[g,t] ~ dnorm(lmup_rem,tau_prem)I(-3,) # constrained to avoid very low probability of capture
    p_remgr[g,t] <- exp(lp_remgr[g,t])/(1+exp(lp_remgr[g,t])) # back-transformation on the probability scale 
    } # end of loop over years
} # end of loop over groups
   
## Successive removals
for (i in 1:NPE1) { 
    C1[i] ~ dbin(p_remgr[Group[i],Year[i]],ntot[Site[i],Year[i]]) # first pass
    n2[Site[i],Year[i]] <- ntot[Site[i],Year[i]]-C1[i] # fish not captured during the first pass
    
    C2[i] ~ dbin(p_remgr[Group[i],Year[i]],n2[Site[i],Year[i]]) # second pass    
    } # end of loop over NPE1

########################
### Intercalibration CPUE and density
##########################
## Calculating predicted density for CPUE using density predicted at sites sampled by successive removals if habitat is only rapids
## Average mean of density   
########
### IAS04
for (t in 8:11) { # from 1994 to 1997
    dIA_Oir[6,t] <- dRR_Oir[5,t] # IAS04 = Oir2.8
    dIA_Oir[7,t] <- ((dRR_Oir[17,t] * Srr_inter[1]) + (dRR_Oir[13,t] * Srr_inter[2]) + (dRR_Oir[18,t]*0.42*Srr_inter[3])) / (Srr_inter[1] + Srr_inter[2] + (0.42*Srr_inter[3]))  # IAS05 = Oir4.5+ Oir4.6 + Oir4.7 (42% des RR du secteur 4.7) 
    } #end of loop over years
    
#############    
## IAS02, IAS08, IAS11
## 1994
dIA_Oir[8,8] <- ((dRR_Oir[3,8] * Srr_inter[4]) + (dRR_Oir[1,8] * Srr_inter[5])) / (Srr_inter[4] + Srr_inter[5]) # IAS02 = Oir1.2 + Oir1.3        
dIA_Oir[9,8] <- ((dRR_Oir[24,8] * (1-0.97) * Srr_inter[6]) + (dRR_Oir[22,8] * Srr_inter[7]) + (dRR_Oir[23,8] * Srr_inter[8]) + (dRR_Oir[21,8] * Srr_inter[9]) + (dRR_Oir[25,8]* Srr_inter[10])) / ((Srr_inter[6] * (1-0.97)) + Srr_inter[7] + Srr_inter[8] + Srr_inter[9] + Srr_inter[10]) # IAS08 = Oir5.2b (3% des RR) + Oir5.3 + Oir5.4 + Oir5.5 + Oir5.6
dIA_Oir[10,8] <- ((dRR_Oir[33,8] * Srr_inter[11]) + (dRR_Oir[34,8] * Srr_inter[12]))/(Srr_inter[11] + Srr_inter[12]) # IAS11 = plav1 + plav2    

##1996 and 1997
for (t in 10:11) { 
    dIA_Oir[8,t] <- ((dRR_Oir[3,t] * Srr_inter[4]) + (dRR_Oir[1,t] * Srr_inter[5])) / (Srr_inter[4] + Srr_inter[5]) # IAS02 = Oir1.2 + Oir1.3
    dIA_Oir[9,t] <- ((dRR_Oir[24,t] * (1-0.97) * Srr_inter[6]) + (dRR_Oir[22,t] * Srr_inter[7]) + (dRR_Oir[23,t] * Srr_inter[8]) + (dRR_Oir[21,t] * Srr_inter[9]) + (dRR_Oir[25,t]* Srr_inter[10])) / ((Srr_inter[6] * (1-0.97)) + Srr_inter[7] + Srr_inter[8] + Srr_inter[9] + Srr_inter[10]) # IAS08 = Oir5.2b (3% des RR) + Oir5.3 + Oir5.4 + Oir5.5 + Oir5.6    
        }  # end of loop over years

#############
### IAS12
for (t in 8:10) { #from 1994 to 1996 
    dIA_Oir[11,t] <- dRR_Oir[45,t]  # IAS12 = RR11
    } # end of loop over years

##############
## IAS08, IAS11
##1999
dIA_Oir[9,13] <- ((dRR_Oir[24,13] * (1-0.97) * Srr_inter[6]) + (dRR_Oir[22,13] * Srr_inter[7]) + (dRR_Oir[23,13] * Srr_inter[8]) + (dRR_Oir[21,13] * Srr_inter[9]) + (dRR_Oir[25,13]* Srr_inter[10])) / ((Srr_inter[6] * (1-0.97)) + Srr_inter[7] + Srr_inter[8] + Srr_inter[9] + Srr_inter[10]) # IAS08 = Oir5.2b (3% des RR) + Oir5.3 + Oir5.4 + Oir5.5 + Oir5.6
dIA_Oir[10,13] <- ((dRR_Oir[33,13] * Srr_inter[11]) + (dRR_Oir[34,13] * Srr_inter[12]))/(Srr_inter[11] + Srr_inter[12]) # IAS11 = plav1 + plav2    

## From 2001 until 2008 (last year of succesive removal)
for (t in 15:22){
    dIA_Oir[9,t] <- ((dRR_Oir[24,t] * (1-0.97) * Srr_inter[6]) + (dRR_Oir[22,t] * Srr_inter[7]) + (dRR_Oir[23,t] * Srr_inter[8]) + (dRR_Oir[21,t] * Srr_inter[9]) + (dRR_Oir[25,t]* Srr_inter[10])) / ((Srr_inter[6] * (1-0.97)) + Srr_inter[7] + Srr_inter[8] + Srr_inter[9] + Srr_inter[10]) # IAS08 = Oir5.2b (3% des RR) + Oir5.3 + Oir5.4 + Oir5.5 + Oir5.6   
    dIA_Oir[10,t] <- ((dRR_Oir[33,t] * Srr_inter[11]) + (dRR_Oir[34,t] * Srr_inter[12]))/(Srr_inter[11] + Srr_inter[12]) # IAS11 = plav1 + plav2      
        } # end of loop over years
    
### From 1999 until 2008 (last year of successive removal)
for(t in 13:22) {
      dIA_Oir[6,t] <- dRR_Oir[5,t] # IAS04 = Oir2.8         
      dIA_Oir[7,t] <- ((dRR_Oir[17,t] * Srr_inter[1]) + (dRR_Oir[13,t] * Srr_inter[2]) + (dRR_Oir[18,t]*0.42*Srr_inter[3])) / (Srr_inter[1] + Srr_inter[2] + (0.42*Srr_inter[3]))  # IAS05 = Oir4.5+ Oir4.6 + Oir4.7 (42% des RR du secteur 4.7) 
      dIA_Oir[8,t] <- ((dRR_Oir[3,t] * Srr_inter[4]) + (dRR_Oir[1,t] * Srr_inter[5])) / (Srr_inter[4] + Srr_inter[5]) # IAS02 = Oir1.2 + Oir1.3
      dIA_Oir[11,t] <- dRR_Oir[45,t]  # IAS12 = RR11
      } # end of loop over years
    
#####################
### Relationship with width       
for (k in 1:(NIA-Ninter-1)) { # here it is prediction for CPUE only (in total 5 sites: IAS03, IAS09, IAS06, IAS07, IAS10)
    lwidth_Oir[k] <- log(W_Oir[k]) # log transformation of river' width at each site
    k_widthOir[k] <- int_width_cut + width_coef_cut * lwidth_Oir[k]  # proportional relationship with width
    k_cpuOir[k] <- exp(k_widthOir[k])
    } # end of loop over sites not used for intercalibration

# new station 2020
lwidth_Oir[12] <- log(W_Oir[12]) # log transformation of river' width at each site
k_widthOir[12] <- int_width_cut + width_coef_cut * lwidth_Oir[12]  # proportional relationship with width
k_cpuOir[12] <- exp(k_widthOir[12])
    
for (k in (NIA-Ninter): (NIA-1)) { # sites sampled both by CPUE and successive removals
     lwidth_Oir[k] <- log(W_Oir[k]) # log transformation of river' width at each site
     k_widthOir[k] <- int_width + width_coef * lwidth_Oir[k]  # proportional relationship with width
     k_cpuOir[k] <- exp(k_widthOir[k])
     } # end of loop over sites used for intercalibration     

k_inter <- exp(log_k_inter); log_k_inter ~ dunif(-10,10) # coeffcient intercalibration pulsium introduit a partir de 2019 (année 33)
########################################
### CPUE only (not used in intercalibration)
for (j in 1: NIAno) {     
    CPUE_IAno[j] ~ dpois(lambdaOir_cpu2[IAno_num[j],Year_IA[j]]) ## observation process 
    I_Puls[j] <- step(Year_IA[j]-32.5) # indicatrice d'utilisation du Pulsium à partir de l'année 2019 (33)
    mul_cpuOir[IAno_num[j],Year_IA[j]] <- k_cpuOir[IAno_num[j]] * dIA_Oir[IAno_num[j],Year_IA[j]] * pow(k_inter,I_Puls[j])  # mean lambda_cpu
        
    eps_CPUE[IAno_num[j],Year_IA[j]] <- log(lambdaOir_cpu2[IAno_num[j],Year_IA[j]])/log(mul_cpuOir[IAno_num[j],Year_IA[j]]) # residuals of the relationship between IA and coefficient of proportionnality

    shape_lcpuOir[IAno_num[j],Year_IA[j]] <- mul_cpuOir[IAno_num[j],Year_IA[j]] * rate_lcpu_cut * pow(k_inter,I_Puls[j])  # shape parameter of the gamma distribution for lambda_cpu
    lambdaOir_cpu[IAno_num[j],Year_IA[j]] ~ dgamma(shape_lcpuOir[IAno_num[j],Year_IA[j]],rate_lcpu_cut)I(0.001,)
    lambdaOir_cpu2[IAno_num[j],Year_IA[j]] <- max(0.001,lambdaOir_cpu[IAno_num[j],Year_IA[j]]) # try to avoid error on lambda_cpu  
    } # end of loop over sites not used for intercalibration

################################################################
### CPUE used for intercalibration
for (m in 1:NIAinter) {
    CPUE_inter[m] ~ dpois(lambdaOir_cpu2[IAinter_num[m],Year_IAinter[m]]) ## observation process
    I_Puls[j] <- step(Year_IA[j]-32.5) # indicatrice d'utilisation du Pulsium à partir de l'année 2019 (33)

    mul_cpuOir[IAinter_num[m],Year_IAinter[m]] <- k_cpuOir[IAinter_num[m]] * dIA_Oir[IAinter_num[m],Year_IAinter[m]]  # mean lambda_cpu

    eps_CPUE[IAinter_num[m],Year_IAinter[m]] <- log(lambdaOir_cpu2[IAinter_num[m],Year_IAinter[m]])/log(mul_cpuOir[IAinter_num[m],Year_IAinter[m]]) # residuals of the relationship between IA and coefficient of proportionnality

    shape_lcpuOir[IAinter_num[m],Year_IAinter[m]] <- mul_cpuOir[IAinter_num[m],Year_IAinter[m]] * rate_lcpu  # shape parameter of the gamma distribution for lambda_cpu
    lambdaOir_cpu[IAinter_num[m],Year_IAinter[m]] ~ dgamma(shape_lcpuOir[IAinter_num[m],Year_IAinter[m]],rate_lcpu)I(0.001,)
    lambdaOir_cpu2[IAinter_num[m],Year_IAinter[m]] <- max(0.001,lambdaOir_cpu[IAinter_num[m],Year_IAinter[m]]) # try to avoid error on lambda_cpu  
     
    } # end of loop over sites used for intercalibration

# From 2021 to 2024, 3 sites are electrofished with MP gear for intercalibartion with Pulsium
r[1] <- 4; r[2] <- 9; r[3] <- 2 # Index (IAno_num ou IAinter_num) of sites used for intercalibartion  
for (m in 1:3) {
     for (l in 1:4) {   # This must be updated every year to accomodate new inetrcalibartion data
        y[m,l] <- l + 34 # index of year of intercalibration
   
        CPUE_MP[m,l] ~ dpois(lambdaOir_cpu_inter[r[m],y[m,l]])

        mul_cpuOir_inter[m,l] <- k_cpuOir[r[m]] * dIA_Oir[r[m],y[m,l]] # mean lambda_cpu modified for MP
        shape_lcpuOir_inter[m,l] <- mul_cpuOir_inter[m,l] * rate_lcpu   # shape parameter of the gamma distribution for lambda_cpu
        lambdaOir_cpu_inter[m,l] ~ dgamma(shape_lcpuOir_inter[m,l],rate_lcpu)I(0.001,)

        } # end of loop over years
   } # end of loop over sites

#########################################
## Extrapolating abundance for the whole river Oir  
##########################################  
# Mean per year and per river
for (t in 1:Nyear) {
  tmp[t] <- t - 33.5
    for (g in 1:10) { # Group within Oir river 
        lambda_ynOir_gr[g,t] <- exp(mu_ydOir[1] + year_dOir[t] + gryr_Oir[Group_an[g,t]]) * ((StotRR[g]+step(tmp[t])*StotRR[14]) + coef_PC_cut * (StotPC[g]+step(tmp[t])*StotPC[14]))
        n_Oir_gr[g,t] ~ dpois(lambda_ynOir_gr[g,t])
        } # end of loop over Oir river
} # end of loop over years

for (t in 1:Nyear) {
    # Oir
    n_Oir[t] <- sum(n_Oir_gr[,t])
    
    #  Moulin du Bois
    lambda_ynMB[t] <- (exp(mu_ydOir[4] + year_dOir[t] + gryr_Oir[Group_an[11,t]]) * (StotRR[11] + coef_PC_cut * StotPC[11]))
    n_MB[t] ~ dpois(lambda_ynMB[t])
    
    # Pont Levesque
    lambda_ynPL[t] <- (exp(mu_ydOir[3] + year_dOir[t] + gryr_Oir[Group_an[12,t]]) * (StotRR[12] + coef_PC_cut * StotPC[12]))
    n_PL[t] ~ dpois(lambda_ynPL[t])
    
    # La Roche
    lambda_ynLR[t] <- (exp(mu_ydOir[2] + year_dOir[t] + gryr_Oir[Group_an[13,t]]) * (StotRR[13] + coef_PC_cut * StotPC[13]))
    n_LR[t] ~ dpois(lambda_ynLR[t]) 
    
    ## Global over the whole river
    ntot_Oir[t] <- n_Oir[t] + n_MB[t] + n_PL[t] + n_LR[t] 
    } # end of loop over years


} # end of model






















