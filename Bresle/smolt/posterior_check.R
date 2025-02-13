
#fit <- as.mcmc(fit)


### JUV
hyperparameters <-c(
## HYPER PARAMETERS 
"mu_B" # mean probability of capture in Beauchamps
,"sigmap_B" # standard deviation in probability of capture in Beauchamps

,"logit_int_Eu" # intercept in probability of capture in Eu
,"logit_flow_Eu" # slope effect of flow in probability of capture in Eu
,"sigmap_Eu" # # standard deviation in probability of capture in Eu

## PROBABILITIES
,"p_B" # probabilities of capture in Beauchamps (without the decrease
,"p_B95" # decrease in probabilities of capture in Beauchamps in 1995
,"p_B96" # decrease in probabilities of capture in Beauchamps in 1996
,"p_B99" # decrease in probabilities of capture in Beauchamps in 1999
#,"p_B00" # decrease in probabilities of capture in Beauchamps in 2000
,"p_B02" # decrease in probabilities of capture in Beauchamps in 2002
,"p_B20" # decrease in probabilities of capture in Beauchamps in 2020 (COVID)
,"p_Btot" # global probabilities of capture in Beauchamps
#,"epsilon_B" # standardized residuals in probability of capture in Beauchamps

,"p_Eu" # probabilites of capture in Eu
#,"epsilon_Eu" # standardized residuals in probability of capture in Eu


## POPULATION
#,"Ntot" # Number of smolt 

,"shape_lambda" # Shape parameter of gamma distribution
,"rate_lambda" # Rate parameter of gamma distribution
,"mean_gamma" # Mean parameter of gamma distribution
,"var_gamma" # Variance parameter of gamma distribution
,"p_keep"
)

pdf(paste('results/Posterior_check_',site,"_",stade,"_",year,'.pdf',sep=""))
#traplot(fit[,which(varnames(fit)%in%hyperparameters)])
#traplot(fit,"pi_Eu00")
for (par in hyperparameters){
  traplot(fit,par) 
  denplot(fit,par) 
}

par(mfrow=c(2,1))
caterplot(fit,paste0("epsilon_Eu[",1:data$NEu,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 
caterplot(fit,paste0("epsilon_B[",1:data$NBeau,"]"), reorder = FALSE, horizontal=FALSE, style=c("plain")) 


dev.off()


