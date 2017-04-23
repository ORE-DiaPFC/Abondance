#rm(list=ls())   # Clear memory

##-----------------------------DATA ----------------------------------##
# year <- 2015
# site <- "Bresle"
# stade <- "smolt"

## WORKING DIRECTORY:
# work.dir<-paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/',site,sep="")
# setwd(work.dir)

load(paste('data/data_',stade,"_",year,'.Rdata',sep=""))

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
  mu_B = 0.5,
  sigmap_B =1,
  logit_int_Eu = 1,
  logit_flow_Eu =1,
  sigmap_Eu=1,
  shape_lambda =2.5,
  rate_lambda = 0.01
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
lambda <- Ntot -10

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
logit_pi_B <- rep(0.5, data$NBeau)

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
logit_pi_Eu <- rep(0.5, data$NEu)

inits_updated <- list(
  Ntot = Ntot
  , lambda = lambda
  , logit_pi_B = logit_pi_B
  , logit_pi_Eu=logit_pi_Eu
)

inits <- list(c( inits_fix,inits_updated))

#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,'.txt',sep=""))


