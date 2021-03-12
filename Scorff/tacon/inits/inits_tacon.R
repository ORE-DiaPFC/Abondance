#rm(list=ls())   # Clear memory

##-----------------------------DATA ----------------------------------##
# year <- 2015
# site <- "Scorff"
# stade <- "tacon"

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

              int_width = runif(1,-.4,-.1)#-0.262
              ,width_coef = runif(1,.4,1)#0.4537
              ,eps = runif(1,.9,1.1)#0.9869
              ,int_ydSc = runif(1,1.5,2)#1.809
              ,log_flow = runif(1,-.9,-.1)#-0.4612
              ,mu_d = runif(1,18,21)#19.84
              ,mup_rem = runif(1,.2,.8)#0.5931
              ,rate_d = runif(1,.02,.03)#0.0261
              #,rate_lcpu = runif(1,.2,.5)#0.3905
              ,p_lcpu = 0.3905
              ,sd_prem = runif(1,.2,.8)#0.7431
              ,sigma_dSc = runif(1,.2,.5)#0.3269
              ,sigma_siteSc = runif(1,.5,.9)#0.7041
              ,sigma_ySc = runif(1,.3,.5)#0.4286
              

              ,d = c(
                39.75,34.32,3.624,0.2629,27.16,
                2.052,2.907,40.31,2.794,9.298,
                5.261E-7,2.525,26.41,1.638,19.16,
                47.33,5.247,1.308,3.92,14.24,
                4.787,32.26,4.449,0.3377,30.03,
                1.535,12.09,13.34,6.233,59.35,
                5.753,8.556,16.14,29.86,26.47,
                45.91,1.901,0.535,2.445,7.7,
                24.3,15.11,5.246,44.17,11.39,
                3.866,10.35,24.42,0.08364,5.737,
                2.741,5.808)
              
              
              ,lambda_cpu = c(
                43.45,41.96,2.652,0.001464,19.23,
                1.425,1.55,27.39,5.362,32.91,
                2.06,5.221,30.08,1.608,18.54,
                45.97,5.392,1.133,2.975,16.45,
                14.76,46.62,7.642,2.61,52.07,
                4.362,26.46,21.13,18.41,110.1,
                23.23,19.56,28.5,41.08,84.05,
                97.4,3.394,5.176,6.185,29.77,
                64.34,16.34,15.46,103.0,38.19,
                23.33,19.05,52.25,0.001932,19.35,
                8.078,13.4)
              
              ,lp_rem1 = c(
                2.095,0.3624,-0.2053,1.165,0.6667,
                0.736,1.195,1.196,0.4064,0.7506,
                -0.2303,-0.7727,0.1187,0.3288,-0.5529,
                0.2799,0.5595,0.7186,0.2992,-0.1047,
                1.235,0.4132,-0.09523,1.543,0.7419,
                -0.2769,0.4575,0.6653,0.06978,1.95,
                0.1219,0.9618,0.9716,-0.6366,-0.2456,
                0.09661,-0.6028,1.755,0.05051,-0.5201,
                -1.768,1.247,0.3404,0.724,1.028,
                1.317,0.5202,0.5515,-0.1839,0.6589,
                1.589,0.7238)
              
             ,ntot = c(
                71.0,71.0,12.0,0.0,54.0,
                3.0,4.0,61.0,3.0,21.0,
                0.0,15.0,130.0,6.0,79.0,
                136.0,28.0,4.0,13.0,49.0,
                18.0,114.0,32.0,3.0,235.0,
                13.0,105.0,59.0,50.0,178.0,
                54.0,67.0,86.0,150.0,248.0,
                360.0,16.0,6.0,26.0,42.0,
                182.0,101.0,33.0,205.0,88.0,
                55.0,71.0,283.0,0.0,102.0,
                51.0,91.0)
              
              ,site_Sc = c(
                -0.6541,-0.1702,0.2946,0.5068,0.2637,
                0.08276,-0.4455,-0.3391,-0.209,-0.1708,
                0.3616,0.1306,0.2901,0.9349,0.3897,
                0.3805,-0.003242,0.1208,0.6526,0.154,
                0.03381,-0.0646,-0.5649,-0.3685,-0.5592,
                -0.1355,0.1112,-0.2493,0.4073,0.6159,
                0.5929,-0.3192,0.5004,0.06181,0.4938,
                -0.3144,0.3643,-0.9695,-0.4139,-0.4127,
                -0.4359,0.08051,-0.6267,-0.06078,-1.032,
                -1.247,-0.1988,-1.924,-1.769,-1.193,
                -0.4942,0.339,-0.1661)
)

###################################################
# TO UPDATE
###################################################

## CPUE_Sc (CPUE observée)
## METTRE A JOUR (53 VALEURS PAR AN) /!\ TAILLE MATRICE
CPUE_Sc_inits <- data$CPUE_Sc#[,data$Nyear]
lambda <- apply(data$CPUE_Sc, 2, mean, na.rm=TRUE) # moyenne des CPUE / année
for (y in 1: data$Nyear){
  CPUE_Sc_inits[,y] <- ifelse(is.na(data$CPUE_Sc[,y]), rpois(nrow(data$CPUE_Sc), lambda[y]),NA) # si donnée observée <- NA, sinon on tire la cpue dans une loi de poisson de moyenne (lambda, moyyene des autres stations)
}


## lambdaSc_cpu (IA +1)
## METTRE A JOUR (53 VALEURS PAR AN) /!\ TAILLE MATRICE
lambdaSc_cpu_inits <- CPUE_Sc_inits +1

## log_dSc (log de la densité locale)
## METTRE A JOUR (53 VALEURS PAR AN) /!\ TAILLE MATRICE
CPUE_Sc_tmp <-data$CPUE_Sc
CPUE_Sc_tmp[is.na(CPUE_Sc_tmp)] = CPUE_Sc_inits[is.na(CPUE_Sc_tmp)]

## METTRE A JOUR (53 VALEURS PAR AN) /!\ TAILLE MATRICE
#log_dSc <- array(, dim=dim(CPUE_Sc_tmp))
#for (y in 1: data$Nyear){
log_dSc <- log( (CPUE_Sc_tmp + 1) / exp(inits_fix$int_width + inits_fix$width_coef * log(data$W_Sc)))
#}

# METTRE A JOUR: 1 VALEUR PAR AN
#year_dSc <- c( 0.7332,1.235,1.156,2.056,2.081,0.4959,0.5732,1.877,0.4347,2.133,2.706,2.004,2.515,2.196,1.86,2.112,2.17,2.241,2.627,2.07,2.331,2.184)
year_dSc <- 0.7332
for (y in 2: data$Nyear){
year_dSc <- c(year_dSc, year_dSc[y-1] + log(lambda[y]/lambda[y-1]))
}
year_dSc <- as.vector(year_dSc)

# METTRE A JOUR: 1 VALEUR PAR AN
#n_Sc <- c(4475, 7808, 6657, 18115, 17751	,3760	,4008	,16509	,3236	,19583	,37338	,18040	,26942	,20460	,15879	,19288	,19863	, 20254	, 32371	,19118	, 25301	, 20833	)
n_Sc <- 4475
for (y in 2: data$Nyear){
n_Sc <- c(n_Sc, exp(year_dSc[y])*2100)
}
n_Sc <- as.vector(n_Sc)


inits_updated <- list(
  CPUE_Sc = CPUE_Sc_inits
  , lambdaSc_cpu = lambdaSc_cpu_inits
  , log_dSc = log_dSc
  , year_dSc = year_dSc
  , n_Sc = n_Sc
)

inits <- list(c( inits_fix,inits_updated))

#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
#bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,'.txt',sep=""))
bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,"_",c,'.txt',sep=""))
} #end loop c
