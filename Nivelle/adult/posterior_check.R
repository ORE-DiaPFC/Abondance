#fit.mcmc <- as.mcmc(fit)

##### BRESLE

#### ADU
hyperparameters <-c(
  "mup_11_1" # from 1984 to 1991
  ,"sigmap_11_1" 
  ,"mup_11_2" # from 1992 to now on
  ,"sigmap_11_2"
  
  # Mean and standard deviation of the probabilities to be captured at Uxondoa
  ,"mupi_U"
  ,"sigmapi_U"
  
  # standard deviation of the probabilities to be captured at Ohla
  ,"sigmapi_Ol"
 # ,"eps_Ol" # standardized residuals in probability to be captured at Ohla
  
  # Mean and standard deviation of the probabilities to be Re-captured by EF, angling or found dead
  ,"mupi_EF"
  ,"sigmapi_EF"
  
  # Mean and standard deviation of the probabilities to move from LN2
  ,"mup_n12"
  ,"sigmap_12"
  
  #Mean and standard deviation of the probabilities to stay in UN
  ,"mup_21"
  ,"sigmap_21"
  
  
 # ,"pi_U" # annual probability to be captured at Uxondoa since 1984 given sea age (1:1SW vs 2:MSW
 # ,"eps_U" # standardized residuals of probability of capture at Uxondoa
  ,"pi_U_eff" # decreased probability of capture at Uxondoda since 2012
  
  ,"pi_EF" # Probabilities to be Re-captured by EF, angling or found dead
  
  ,"p_n12" # Probabilities to move from LN2 
  #,"eps_12" # standardized residuals in probability of moving from LN2
  
  ,"k_1"
  ,"k_2"
  #,"alpha_1"
  #,"alpha_2"
  ,"eta_1"
  ,"eta_2"
  
  ,"rho"
  
  ,"shape_lambda"
  ,"rate_lambda"
  ,"shape_prec"
  ,"rate_prec"
 ,"mean_prec"
  ,"s"
  
  ,"lambda_tot0"
  ,"Plambda0"
  ,"a_1.1SW"
  ,"a_2.1SW"
  ,"a_MSW"
  
  ,"d_pi_Ol"
  ,"d_pi_U"
  
  #,"pi_Ol" # annual probability to be captured at Ohla since 1984 given sea age (1:1SW vs 2:MSW
  
  
  ### TESTS for probability of moving from LN2
  # 1: Male 1SW vs Female 1SW
  # 2: Male 1SW vs Male MSW
  # 3: Male 1SW vs Female MSW
  # 4: Female 1SW vs Male MSW
  # 5: Female 1SW vs Female MSW
  # 6: Male MSW vs Female MSW
  #,"test_p_12" # is there a difference in the mean probability
  
) 


pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%hyperparameters)])
#traplot(fit.mcmc,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit.mcmc,par) 
  denplot(fit.mcmc,par) 
}

# traplot(fit.mcmc, paste0("eps_U[",1:data$Y,",1]"))

par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("alpha_1[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit.mcmc,paste0("alpha_2[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("eps_12[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit.mcmc,paste0("eps_12[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("eps_12[",1:data$Y,",3]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit.mcmc,paste0("eps_12[",1:data$Y,",4]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("eps_U[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit.mcmc,paste0("eps_U[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit.mcmc,paste0("eps_Ol[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

dev.off()

