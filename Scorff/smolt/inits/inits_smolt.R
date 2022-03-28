#rm(list=ls())   # Clear memory

##-----------------------------DATA ----------------------------------##
# year <- 2015
# site <- "Scorff"
# stade <- "smolt"

## WORKING DIRECTORY:
# work.dir<-paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/',site,sep="")
# setwd(work.dir)

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
    logit_int = runif(2,.9,1.1),#c(1,1),
    logit_flow = runif(2,.9,1.1),#c(1,1),
    sigmap = runif(2,.4,.8),#c(0.7357,0.4225),
    rho=runif(1,.1,.8),#0.3,
   # l_ML_dim =runif(1,.01,.02),# 0.01136,
   # junk = runif(1,-2,-1),#-1.1,
   mu_p1c = runif(1,0.1,0.9),# rbeta(1,2,2),
   alpha =runif(1,0.1,0.25), # protection against too low values for s1 and s2
    rate_lambda = 1.574E-4,
    shape_lambda = runif(1,1,2)#1.688
)



###################################################
# TO UPDATE
###################################################

# METTRE A JOUR
# Ntot = c(
#   6433.0,1841.0,9901.0,4478.0,2435.0,
#   2917.0,11070.0,1352.0,12290.0,8043.0,
#   11100.0,7222.0,7402.0,11070.0,14430.0,
#   10820.0,8665.0,7866.0,11125,13559,
#   9000)
# Cm_ML: Annual number of smolt marked and released downstream Moulin de Lesl?
# Cm_MP: Annual number of marked smolt captured at Moulin des Princes
# Cum_MP: # Annual number of unmarked smolt captured at Moulin des Princes
Ntot =(data$Cm_ML * (data$Cm_MP + data$Cum_MP)) / data$Cm_MP
Ntot[1:2] = (data$Cm_ML[1:2] * data$C_MP) / data$Cm_MP[1:2]
Ntot[26]<-mean(Ntot, na.rm=TRUE) # COVID
N <- as.integer(Ntot) # /!!!\ 21.03.2022: N est different de Ntot; à reprendre!!!!


# METTRE A JOUR
# lambda = c(
#   6380.0,1750.0,9927.0,4553.0,2417.0,
#   2897.0,10970.0,1296.0,12320.0,8083.0,
#   11120.0,7377.0,7370.0,1.1E+4,14400.0,
#   11010.0,8637.0,7853.0,11075,13000,
#  9000)
lambda = N

#shape_lambda.inits <- 3.8
#rate_lambda.inits <- 0.0005

# METTRE A JOUR /!\ TAILLE MATRICE
logit<-function(x) {log(x/(1-x))}
logit_pi <- array(, dim=c(data$Nyears,2))
# Proba capture au Moulin des Princes
logit_pi[,1] <- logit(data$Cm_MP / data$Cm_ML)
# Proba capture au Moulin du Leslé
logit_pi[,2] <- logit(data$C_ML / N)

logit_pi[26,] <- logit_pi[25,] # COVID / replace with values from year before to initialize

Cm_MP.inits <- rep(NA,data$Nyears)
Cm_MP.inits[26]<- 0 # COVID, no capture

inits_updated <- list(
  N = N
  , lambda = lambda
  , logit_pi = logit_pi
  ,Cm_MP = Cm_MP.inits
  #, shape_lambda=shape_lambda.inits
  #,rate_lambda=rate_lambda.inits
  
)

inits <- list(c( inits_fix,inits_updated))

#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
#bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,'.txt',sep=""))
bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,"_",c,'.txt',sep=""))
} #end loop c
