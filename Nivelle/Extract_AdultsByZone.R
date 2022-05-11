

dir <- "/media/hdd4To/mbuoro/ORE-DiaPFC/Abundance/Nivelle/adult/results/"
load(paste0(dir,"Results_adult_2021.RData"))

fit.mcmc <- as.mcmc(fit) # using bugs

Y = 38 # 2021

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


  ## BETWEEN ASCAIN AND UXONDOA (LN1)
  ## --------------------------------
  #n_11[t,g] : annual number of fish per breeding category staying in LN1
  #n_n11[t,g] : annual number of fish per breeding category not staying in LN1 (migrating upstream)
  #e_11_tot[t] : annual escapement for LN1
  
### Extract posterior values and quantiles
# annual number of fish per breeding category staying in LN1
n_11 <- fit$median$n_11; colnames(n_11)<-c("Male_1SW","Female_1SW","Male_MSW","Female_MSW")
# annual escapment per breeding category staying in LN1
e_11_tot <- fit$sims.list$e_11_tot
e_11_tot_stats <- apply(e_11_tot,2,quantile, probs=c(0.025, 0.5,0.975)); colnames(e_11_tot_stats)<-1984:2021
#median_e_11_tot <- fit$median$e_11_tot




  ## CAPTURE AT UXONDOA
  ## -------
  ## n_um[t,g]: Annual number of fish not captured (not marked) at Uxondoa per breeding category
  ##################################
  



  ## BETWEEN UXONDOA AND OLHA
  ## ------------------------
  #n_12[t,g] : annual number of fish per breeding category staying in LN2 since 1992
  #e_12_tot[t] : annual escapement for LN2
### Extract posterior values and quantiles 
# annual number of fish per breeding category staying in LN2
n_12 <- fit$median$n_12; colnames(n_12)<-c("Male_1SW","Female_1SW","Male_MSW","Female_MSW")
# annual escapment per breeding category staying in LN2
e_12_tot <- fit$sims.list$e_12_tot
e_12_tot_stats <- apply(e_12_tot,2,quantile, probs=c(0.025, 0.5,0.975)); colnames(e_12_tot_stats)<-1984:2021
  

  ## CAPTURE AT UXONDOA
  ## -------
  ## n_um[t,g]: Annual number of fish not captured (not marked) at Uxondoa per breeding category
  ##################################
  
  
  ## UPSTREAM OLHA
  ## -------------
  ## e_2[t]: annual escapment for HC zone. Unknown in 2000 and since 2012
  #e_21_tot[t]: annual escapement for UN
  #e_22_tot[t]: annual escapement for LUR
### Extract posterior values and quantiles 

#e_2 <- fit$sims.list$e_2
#e_2_stats <- apply(e_2,2,quantile, probs=c(0.025, 0.5,0.975)); colnames(e_2_stats)<-1990:2021

e_21_tot <- fit$sims.list$e_21_tot
e_21_tot_stats <- apply(e_21_tot,2,quantile, probs=c(0.025, 0.5,0.975)); colnames(e_21_tot_stats)<-1990:2021

e_22_tot <- fit$sims.list$e_22_tot
e_22_tot_stats <- apply(e_22_tot,2,quantile, probs=c(0.025, 0.5,0.975)); colnames(e_22_tot_stats)<-1990:2021




  # Breeding escapment per breeding category and per zone
  # -----------------------------------------------------
  #eggs_11[t] : annual number of eggs produced between Ascain and Uxondoa (LN1): fixed number of 4500 eggs/female 1SW and 7200 eggs/female MSW
  #eggs_12[t] : annual number of eggs produced between Uxondoa and Olha (LN2)
  #eggs_21[t] : annual number of eggs produced in Upper Nivelle
  #eggs_22[t] : annual number of eggs produced in Lurgorrieta
  #eggs_tot[t] : annual number of eggs produced
  
  # ,"pi_U" # annual probability to be captured at Uxondoa since 1984 given sea age (1:1SW vs 2:MSW
  # ,"eps_U" # standardized residuals of probability of capture at Uxondoa
  #,"pi_U_eff" # decreased probability of capture at Uxondoda since 2012
  #,"pi_EF" # Probabilities to be Re-captured by EF, angling or found dead
  #,"p_n12" # Probabilities to move from LN2 
  #,"eps_12" # standardized residuals in probability of moving from LN2
  #,"pi_Ol" # annual probability to be captured at Ohla since 1984 given sea age (1:1SW vs 2:MSW

save(n_11, n_12, e_11_tot_stats,e_12_tot_stats,e_21_tot_stats,e_22_tot_stats, file=paste0(dir,"Escapement_Nivelle_Adults.Rdata"))



pdf(paste0(dir,'Escapement_Nivelle_Adults.pdf'))
#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%hyperparameters)])
#traplot(fit.mcmc,"pi_Eu00")


# traplot(fit.mcmc, paste0("eps_U[",1:data$Y,",1]"))
par(mfrow=c(1,1))

caterplot(fit.mcmc,paste0("e_11_tot[",1:Y,"]"),collapse=TRUE, reorder = FALSE, horizontal=FALSE, style=c("plain")
          ,labels = 1984:2021, las=2, cex.labels=0.7
          )
axis(side=3,line=1, at= 19,labels = "Annual escapement between ASCAIN and UXONDOA (LN1)")

par(mfrow=c(1,1))
caterplot(fit.mcmc,paste0("e_12_tot[",1:Y,"]"),collapse=TRUE, reorder = FALSE, horizontal=FALSE, style=c("plain")
          ,labels = 1984:2021, las=2, cex.labels=0.7
)
axis(side=3,line=1, at= 19,labels = "Annual escapement between UXONDOA AND OLHA (LN2)")

par(mfrow=c(1,1))
caterplot(fit.mcmc,paste0("e_21_tot[",1:Y,"]"),collapse=TRUE, reorder = FALSE, horizontal=FALSE, style=c("plain")
          ,labels = 1990:2021, las=2, cex.labels=0.7
)
axis(side=3,line=1, at= 19,labels = "Annual escapement UPSTREAM OLHA (Upper Nivelle)")

par(mfrow=c(1,1))
caterplot(fit.mcmc,paste0("e_22_tot[",1:Y,"]"),collapse=TRUE, reorder = FALSE, horizontal=FALSE, style=c("plain")
          ,labels = 1990:2021, las=2, cex.labels=0.7
)
axis(side=3,line=1, at= 19,labels = "Annual escapement UPSTREAM OLHA (Lurgorrieta)")


# par(mfrow=c(1,1))
# caterplot(fit.mcmc,paste0("e_12[",1:Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain")
#           ,labels = "")
# caterplot(fit.mcmc,paste0("e_12[",1:Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain"),
#           collapse=TRUE, add=TRUE, cat.shift=0.3,col="tomato"
#           ,labels = 1984:2021, las=2, cex.labels=0.7)
# axis(side=3,line=1, at= 19,labels = "Annual escapement 1SW per breeding category staying in upstream Lower Nivelle (LN2)")
# 
# par(mfrow=c(1,1))
# caterplot(fit.mcmc,paste0("e_12[",1:Y,",3]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
# caterplot(fit.mcmc,paste0("e_12[",1:Y,",4]"), reorder = FALSE, horizontal=FALSE, style=c("plain"),
#           collapse=TRUE, add=TRUE, cat.shift=-0.3,col="tomato"
# )
# axis(side=3,line=1, at= 19,labels = "Annual escapement MSW per breeding category staying in upstream Lower Nivelle (LN2)")

dev.off()
