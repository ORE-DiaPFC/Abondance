#rm(list=ls())   # Clear memory
#library(rjags) # require to use "read.bugsdata" function

##-----------------------------FUNCTIONS ----------------------------------##
invlogit<-function(x) {1/(1+exp(-(x)))}
logit<-function(x) {log(x/(1-x))}

##-----------------------------DATA ----------------------------------##
#year <- 2016
#site <- "Bresle"
#stade <- "adult"

## WORKING DIRECTORY:
#work.dir<-paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/',site,stade,sep="/")
#setwd(work.dir)

load(paste('data/data_',stade,"_",year,'.Rdata',sep=""))

for (c in 1:2){ # 2 chains
#------------------------INITS----------------------------------##
# inits<-function(){
#   list(
#     beta=rnorm(1,0,1)
#   )}

#inits1 <- read.bugsdata(paste("inits/init-",site,"-",stade,year,".txt",sep=""))
#inits<-list(inits1)#inits2,inits3)


  
###################################################
# NO UPDATE
###################################################
inits_fix <- list(
  lambda_tot0 = runif(1,90,120),#104.2,
  logit_flow_Eu = runif(2,-1,0),#c( -0.4262,-0.7079),
  lflow_fall_Eu = runif(2,-1,0),#c(-0.5,-0.5),
  logit_int_Eu = runif(2,0,1),#c( 0.5082,0.8304),
  mupi_B = runif(2,0,.5),#c( 0.0827,0.1032),
  pi_Eu00 = runif(2,0.1,.9),#c( 0.5871,0.5992),
  pi_Eu01 = runif(2,0.1,.9),#c( 0.2006,0.7994),
  rate_lambda = runif(1,0,.1),#0.04238,
  s = runif(2,2,20),#c(15.67,3.914),
  shape_lambda = runif(1,3,7),#5.995,
  #sigmapi_B = c(0.644,0.6969),
  #sigmapi_Eu = c(1.019,0.8865)
  varpi_B = runif(2,0.1,.9),#c(0.414,0.485),
  varpi_Eu = runif(2,0.5,1.5)#c(1.038,0.8865)
)



###################################################
# TO UPDATE
###################################################
# METTRE A JOUR
# lambda_tot = c(
#   101.8,137.8,207.4,185.3,129.9,
#   207.0,84.98,198.1,165.3,136.1,
#   71.26,135.3,65.63,66.28,279.3,
#   87.58,48.08,131.9,128.1,84.58,
#   161.4,357.2,134.2,151.9,180.1,
#   117.4,184.8,146.0,198.6,179.2,
#   180, 180,
#   180 # 2016
#   )
lambda_tot = (data$Cm_Eu[,1] + data$Cm_Eu[,2]) * ((rowSums(data$Cm_B) + rowSums(data$Cum_B))/rowSums(data$Cm_B))
lambda_tot = ifelse(is.na(lambda_tot),mean(lambda_tot,na.rm=TRUE),lambda_tot)

# METTRE A JOUR /!\ TAILLE MATRICE
  # logit_pi_B = structure(.Data = c(
  #   -2.496,-3.38,
  #   -1.372,-2.32,-2.6,
  #   -2.831,-2.956,-2.549,-3.518,-3.105,
  #   NA,            NA,-2.793,-3.107,-3.062,
  #   -2.998,-2.493,-3.884,            NA,            NA,
  #   -3.226,-2.017,-2.034,-1.316,-2.486,
  #   -1.573,-3.139,-3.074,-2.49,-2.94,
  #   -2.965,-1.942,            NA,            NA,            NA,
  #   NA,-1.371,-1.124,-3.252,-3.075,
  #   -2.032,-2.273,-1.121,-1.827,-1.782,
  #   -2.796,-2.925,-3.714,-2.054,-1.113,
  #   -1.612,-2.486,-2.649,-2.862,-2.107,
  #   -2.132,-2.473,-2.559,-2.798,-2.454,
  #   -2.132,-2.473,-2.559,-2.798,
  #   NA, NA # 2016
  #   ),
  #   .Dim = c(33,2))
logit_pi_B <- cbind(
                  logit( ((data$Cm_B[,1]) + (data$Cum_B[,1])) / lambda_tot),
                  logit( ((data$Cm_B[,2]) + (data$Cum_B[,2])) / lambda_tot)
)
logit_pi_B <- ifelse(logit_pi_B=="-Inf",NA,logit_pi_B)

# METTRE A JOUR /!\ TAILLE MATRICE
# logit_pi_Eu = structure(.Data = c(
#   0.343,1.078,1.487,0.906,0.9426,
#   1.136,0.2599,0.8924,-0.0344,1.519,
#   1.249,1.272,1.719,0.7151,1.138,
#   0.4976,1.947,2.237,-0.7839,0.2312,
#   -0.4747,0.7478,-1.035,0.3989,-1.214,
#   1.034,0.02703,1.256,1.021,0.9162,
#   -0.8177,0.1278,-0.742,-1.017,-1.053,
#   -2.073,0.2711,-0.2967,-1.547,0.4636,
#   -0.6245,1.732,0.5033,0.681,0.3818,
#   0.8959,-1.375,0.4715,0.4871,-0.9009,
#   1.564,1.294,1.121,0.4898,2.645,
#   1.779,1.479,1.005,0.09753,0.7002,
#   1.779,1.479,1.005,0.09753,
#   1, 1 #2016
#   ),
#   .Dim = c(33,2))
logit_pi_Eu <- cbind(
  logit( ((data$C_Eu[,1]) + (data$C_Eu[,1])) / lambda_tot),
  logit( ((data$C_Eu[,2]) + (data$C_Eu[,2])) / lambda_tot)
)
logit_pi_Eu <- ifelse(logit_pi_Eu=="NaN",NA,logit_pi_Eu)

# METTRE A JOUR /!\ TAILLE MATRICE
# n = structure(.Data = c(
#   74.0,33.0,104.0,23.0,165.0,
#   27.0,168.0,41.0,97.0,13.0,
#   163.0,29.0,55.0,45.0,161.0,
#   25.0,139.0,35.0,120.0,37.0,
#   59.0,14.0,115.0,2.0,58.0,
#   21.0,49.0,13.0,240.0,14.0,
#   47.0,31.0,35.0,21.0,109.0,
#   14.0,112.0,24.0,65.0,22.0,
#   124.0,12.0,249.0,50.0,116.0,
#   102.0,129.0,19.0,129.0,10.0,
#   85.0,40.0,157.0,40.0,129.0,
#   28.0,120.0,33.0,147.0,69.0,
#   120.0,33.0,147.0,69.0,
#   140, 70 # 2016
#   ),
#   .Dim = c(33,2))
n <- cbind(
  lambda_tot * (data$C_Eu[,1]/(data$C_Eu[,1]+data$C_Eu[,2])),
  lambda_tot * (data$C_Eu[,2]/(data$C_Eu[,1]+data$C_Eu[,2]))
)
n <- ceiling(n)

##Proprotion sampling for sexing
p_smp=array(,dim=c(data$Y,2))
p_smp[,1] <-rbeta(data$Y,1, 5) # 1SW
p_smp[,2] <-rbeta(data$Y,5, 1) # MSW

# proportion male by sea age
p_male=array(,dim=c(data$Y,2))
p_male[,1] <- 0.5
p_male[,2] <- 0.5

inits_updated <- list(
  lambda_tot = lambda_tot
  ,logit_pi_Eu=logit_pi_Eu
  ,logit_pi_B=logit_pi_B
  ,n=n
  ,p_male=p_male
  ,p_smp=p_smp
)

inits <- list(c( inits_fix,inits_updated))

#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
#bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,'.txt',sep=""))
bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,"_",c,'.txt',sep=""))
} #end loop c

