#rm(list=ls())   # Clear memory

##-----------------------------DATA ----------------------------------##
# year <- 2015
# site <- "Bresle"
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
  ## NO UPDATE
  mu_B = runif(1,0.1,0.5),#0.5,
  sigmap_B =runif(1,0,2),#1,
  logit_int_Eu = runif(1,0,2),#,
  logit_flow_Eu =runif(1,0,2),#,
  sigmap_Eu=runif(1,0,2),#,
  shape_lambda =2.436, #runif(1,1,3),#,
  rate_lambda = 0.001, #runif(1,0.01,0.05)#0.01
  
  #mb-21.03.2022
  p_B95 = runif(1,0.1,0.2), # decrease for year 1995
  p_B96 = runif(1,0.1,0.2), # decrease for year 1996
  p_B99 = runif(1,0.1,0.2), # decrease for year 1999
  # p_B00 ~ dbeta(1,1) # decrease for year 2000
  p_B02 = runif(1,0.1,0.2), # decrease for year 2002
  p_B20 = runif(1,0.1,0.2) # decrease for year 2020
)



###################################################
# TO UPDATE
###################################################
## METTRE A JOUR
# Ntot = c(3115	,
#          3128	,
#          2750	,
#          3530	,
#          2747	,
#          2400	,
#          NA	,
#          NA	,
#          NA	,
#          NA	,
#          2415	,
#          2565	,
#          2941	,
#          2050	,
#          2042	,
#          4594	,
#          2800	,
#          2040	,
#          2263	,
#          NA	,
#          2063	,
#          2774	,
#          6321	,
#          4110	,
#          3074	,
#          4215	,
#          4155	,
#          2317	,
#          3135	,
#          3829	,
#          3938	,
#          2523	,
#          2424	,
#          8000 )
Ntot <- (data$Cum_Eu / (data$Cm_Eu/data$Cm_B)) + data$Cum_B + data$Cm_B
Ntot <- as.integer(Ntot)



## METTRE A JOUR
# lambda = c(3105	,
#            3118	,
#            2740	,
#            3520	,
#            2737	,
#            2390	,
#            NA	,
#            NA	,
#            NA	,
#            NA	,
#            2405	,
#            2555	,
#            2931	,
#            2040	,
#            2032	,
#            4584	,
#            2790	,
#            2030	,
#            2253	,
#            NA	,
#            2053	,
#            2764	,
#            6311	,
#            4100	,
#            3064	,
#            4205	,
#            4145	,
#            2307	,
#            3125	,
#            3819	,
#            3928	,
#            2513	,
#            2414	,
#            8000)
#lambda <- Ntot -10

#mb-21.03.2022
lambda <-rgamma(data$Nyears,inits_fix$shape_lambda,inits_fix$rate_lambda)
lambda <- as.integer(lambda)
lambda <- ifelse(lambda>Ntot,Ntot-10,lambda)
lambda[c(7:10,20)]<-NA




## METTRE A JOUR
# logit_pi_B = c(0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5	,
#                0.5)
#logit_pi_B <- rep(0.5, data$NBeau)

#mb-21.03.2022
varp_B <- (inits_fix$sigmap_B)*(inits_fix$sigmap_B)
precp_B <- 1/(varp_B)
logit_mupi_B <- log(inits_fix$mu_B/(1-inits_fix$mu_B)) #logit transformation
#logit_pi_B <-rnorm(data$NBeau,logit_mupi_B,precp_B)
#logit_pi_B <-rnorm(data$Nyears,logit_mupi_B,precp_B)

# p_B <- exp(logit_pi_B)/(1+exp(logit_pi_B))  # back-transformation on the probability scale
# p_Btot <- p_B
# data$C_B/p_Btot
# mb 9-3-2025
# C_B[t] ~ dbin (p_Btot[t],Ntot[t])
# p_B[t] <- exp(logit_pi_B[t])/(1+exp(logit_pi_B[t])) 
p_Btot <- data$C_B/Ntot
logit_pi_B <- logit(p_Btot) # logit transformation

## METTRE A JOUR
# logit_pi_Eu = c(
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5	,
#   0.5)
#logit_pi_Eu <- rep(0.5, data$NEu)

#mb-21.03.2022
varp_Eu <- (inits_fix$sigmap_Eu)*(inits_fix$sigmap_Eu)
precp_Eu <- 1/(varp_Eu) # precision
logit_mupi_Eu <- inits_fix$logit_int_Eu + inits_fix$logit_flow_Eu * data$stlogQ_Eu
logit_pi_Eu <-rnorm(data$Nyears,logit_mupi_Eu,precp_Eu)
#logit_pi_Eu <-rnorm(data$Nyears,logit_mupi_Eu,precp_Eu)

inits_updated <- list(
  Ntot = Ntot
  , lambda = lambda
  , logit_pi_B = logit_pi_B
  , logit_pi_Eu=logit_pi_Eu
)

inits <- list(c( inits_fix,inits_updated))

#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
#bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,'.txt',sep=""))
bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,"_",c,'.txt',sep=""))
} #end loop c

