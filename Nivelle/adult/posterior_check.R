#fit <- as.mcmc(fit)

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
  
  
  ,"pi_U" # annual probability to be captured at Uxondoa since 1984 given sea age (1:1SW vs 2:MSW
 #,"pi_Ol"
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
#traplot(fit[,which(varnames(fit)%in%hyperparameters)])
#traplot(fit,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit,par) 
  denplot(fit,par) 
}

# traplot(fit, paste0("eps_U[",1:data$Y,",1]"))

par(mfrow=c(1,1))
caterplot(fit,"n_tot", reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("ntot")

par(mfrow=c(2,1))
caterplot(fit,"n_1SW", reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("n_1SW")
caterplot(fit,"n_MSW", reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("n_MSW")

par(mfrow=c(2,1))
caterplot(fit,paste0("sex_ratio_1SW"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("sex_ratio_1SW")
caterplot(fit,paste0("sex_ratio_MSW"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("sex_ratio_MSW")

par(mfrow=c(2,1))
caterplot(fit,paste0("pmale_1SW"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("pmale_1SW")
caterplot(fit,paste0("pmale_MSW"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("pmale_MSW")


par(mfrow=c(2,1))
# Generate caterpillar plot
caterplot(fit, paste0("pi_U[", 1:data$Y, ",1]"), 
          reorder = FALSE, horizontal = FALSE, style = "plain",labels = 1984:year)
# Add second caterplot overlay
caterplot(fit, paste0("pi_U[", 1:data$Y, ",2]"), 
          reorder = FALSE, horizontal = FALSE, style = "plain", 
          add = TRUE, col = "tomato",labels=NA)
# Add title
title("Proba. capture Uxondoa")
# Add legend
legend("topright", legend = c("1HM", "PHM"), fill = c("blue", "tomato"), bty = "n")

caterplot(fit,paste0("pi_Ol[",29:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"),labels = 2012:year)
title("Proba. capture Olha")


par(mfrow=c(2,1))
caterplot(fit,paste0("alpha_1[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit,paste0("alpha_2[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit,paste0("eps_12[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit,paste0("eps_12[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit,paste0("eps_12[",1:data$Y,",3]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit,paste0("eps_12[",1:data$Y,",4]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit,paste0("eps_U[",1:data$Y,",1]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
caterplot(fit,paste0("eps_U[",1:data$Y,",2]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit,paste0("eps_Ol[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))

#par(mfrow=c(2,1))
caterplot(fit,paste0("eggs_tot[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("eggs_tot")

par(mfrow=c(2,1))
caterplot(fit,paste0("P_1SW[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:year)
title("Proportion de 1SW / cohorte")
caterplot(fit,paste0("P_MSW[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), labels = 1984:(year-1))
title("Proportion de MSW / cohorte")


par(mfrow=c(2,1))
caterplot(fit,paste0("p_11_1[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
title("Proba. de rester aval Uxondoa")
caterplot(fit,paste0("p_11_2[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"), col="tomato")
title("Proba. de rester aval Uxondoa")

# Set plotting layout
par(mfrow = c(2, 2))

# Define offsets to separate caterpillars
offsets <- c(-0.2, -0.1, 0.1, 0.2)  # Adjust for spacing

# Generate caterpillar plots with spacing
caterplot(fit, paste0("p_n12[", 1:data$Y, ",1]"), 
          reorder = FALSE, horizontal = FALSE, style = "plain", 
          labels = 1992:year, offsets = offsets[1])

caterplot(fit, paste0("p_n12[", 1:data$Y, ",2]"), 
          reorder = FALSE, horizontal = FALSE, style = "plain", 
          labels = 1992:year,  col = 2, offsets = offsets[2])

caterplot(fit, paste0("p_n12[", 1:data$Y, ",3]"), 
          reorder = FALSE, horizontal = FALSE, style = "plain", 
          labels = 1992:year,  col = 3, offsets = offsets[3])

caterplot(fit, paste0("p_n12[", 1:data$Y, ",4]"), 
          reorder = FALSE, horizontal = FALSE, style = "plain", 
          labels = 1992:year,  col = 5, offsets = offsets[4])
title("Proba. de passer dans la haute nivelle")

par(mfrow=c(1,1))
caterplot(fit,paste0("p_21[",1:data$Y,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain"))
title("Proba. de rester Haute nivelle")

dev.off()

