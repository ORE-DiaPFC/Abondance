##-----------------------------DATA ----------------------------------##

load(paste('data/data_',stade,"_",year,'.Rdata',sep=""))

#------------------------INITS----------------------------------##


for (c in 1:2){ # 2 chains
###################################################
# NO UPDATE
###################################################
inits_fix <- list(
  ## NO UPDATE
  log_cess_MC = -0.2,#runif(1,-0.3,0),#-0.2224,
  rate_lambda = 0.003,#runif(1,0.001,0.01),#0.00248,
  shape_lambda = 2.8,#runif(1,2,3),#2.893,
  logit_flow_MC = -0.25,#runif(1,-.7,0),#-0.55,
  logit_int_MC = 0.5,#runif(1,.7,0.9),#0.7
  l1 = rbeta(1,1,2), 
  l2 = rbeta(1,1,2)
)

inits0 <- read.bugsdata(paste("inits/init-",site,"-",stade,as.numeric(year)-1,"_",c,".txt",sep=""))
#load("/media/hdd4To/mbuoro/ORE-DiaPFC/Abundance/Oir/smolt/results/Results_smolt_2022.RData")
###################################################
# TO UPDATE
###################################################
#inits_updated <- list(


## METTRE A JOUR
# p_MC = c(
#   0.6704,0.8023,0.6877,0.6906,0.8445,
#   0.6361,0.8376,0.8768,0.6882,0.818,
#   0.6731,0.4599,0.4611,0.7612,0.8879,
#   0.4909,0.8072,0.6636,0.7157,0.816,
#   0.6614,0.7451,0.5876,0.5282,0.4533,
#   0.4882,0.7201,0.4379,0.4570,0.45,
#   0.25)
# )
p_MC = (data$Cm_R / data$Cm_MC)
#p_MC <- fit$median$p_MC
#p_MC <- rep(0.5,data$Nyears)#c(inits0$p_MC, mean(inits0$p_MC)) #rep(0.5,data$Nyears)

  ## METTRE A JOUR
  # Ntot = c(
  #   792.0,365.0,433.0,780.0,886.0,
  #   237.0,698.0,237.0,468.0,770.0,
  #   1133.0,482.0,1111.0,255.0,2059.0,
  #   1413.0,2350.0,1242.0,1159.0,809.0,
  #   1321.0,962.0,2228.0,3502.0,1544.0,
  #   1952.0,1358.0,1358.0,1647.0,2500,
  #   2000),
#smolt numbers by year of migration
Ntot = as.integer(data$C_MC / p_MC)

# Proportion smolt 1+ par cohorte
p1c = rep(0.8,data$Nyears)



## METTRE A JOUR

  # Decomposition par cohorte
  # Distributing smolt into age classes
  #Nc1 <-rbinom(data$Nyears,N, p1c)
  Nc1 <- as.integer(Ntot*p1c) # smolt 1+ de la cohorte
  Nc2 <- Ntot - Nc1 # smolt2+ de l'année
  #N2c <- c(N2[2:data$Nyears],N1c[data$Nyears]*0.2) # déalage des smolts 2+ par cohorte
  #N <- as.integer(N1c + N2c)
  Nc=matrix(NA, nrow=data$Nyears,ncol=2)
  Nc[1,1]<-Nc1[1]
  for (t in 2:data$Nyears) {
  Nc[t,1]<-Nc1[t]
  Nc[t-1,2]<-Nc2[t]
  }
  N <- as.integer(Nc1/p1c)
  
  # lambda = c(
  #   762.7,361.5,488.6,749.6,885.7,
  #   231.4,723.4,225.5,440.7,761.1,
  #   1089.0,489.0,1092.0,277.8,2064.0,
  #   1448.0,2313.0,1284.0,1145.0,820.3,
  #   1371.0,962.5,2208.0,3465.0,1588.0,
  #   1955.0,1409.0,1369.0,1650.0,2500,
  #   2000)
  lambda=N


  lambda0 <- inits0$lambda[1]#rgamma(1,inits_fix$shape_lambda,inits_fix$rate_lambda) 
  N0 <- inits0$lambda[1]#rpois(1,lambda0)
  p10c <- 0.8
  N01c <-rbinom(1, N0,p10c)
  N02c <- N0-N01c 	
  #Ntot <- c(N01c+N02c, Ntot)
  
  Nc[,2]<-NA # for inits

inits_updated <- list(
  #Ntot = Ntot
  N=N
  , lambda0=lambda0
  , lambda = lambda
  #, p_MC = p_MC
  , p1c = p1c
  , Nc=Nc
)

inits <- list(c( inits_fix,inits_updated))

#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
#bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,'.txt',sep=""))
bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,"_",c,'.txt',sep=""))
} #end loop c
