#rm(list=ls())   # Clear memory

##-----------------------------DATA ----------------------------------##
# year <- 2015
# site <- "Scorff"
# stade <- "adult"

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
#inits_fix <- list(
  # NO UPDATE
  lambda_tot0 = runif(1,400,500)#415.7
  logit_effort_R = runif(2,.1,.2)#c( 0.1897,0.1621)
  logit_flow_MP = runif(2,-.4,-.01)#c( -0.06312,-0.3232)
  logit_flow_R = runif(2,-.4,-.01)##c( -0.3844,-0.142)
  logit_int_MP = c( 0.7415,-0.4103)
  logit_int_R = runif(2,-2.4,-2)#c(-2.184,-2.036)
  # logit_pi_oF = cbind(
  #   c(0.2227,0.7493,-0.4883,0.9541,1.074, 0.7177,1.445,0.4209,0.7487),
  #   c(0.2227,0.7493,-0.4883,0.9541,1.074, 0.7177,1.445,0.4209,0.7487))
  #mupi_oF = runif(1,.4,.7)#0.5933
  pi_MP94 = runif(2,.2,.5)#c( 0.4432,0.322)
  pi_MP20 = c(NA,runif(1,.01,.2))# COVID
  pi_oD = runif(1,.1,.3)#0.1511
  rate_lambda = runif(1,0.005,.1)#0.01217
  rho_D = runif(1,0.4,.8)#.5#c(  -0.5855,0.9674),
  rho_F = runif(1,-.8,-.4)#-.5#c( -0.7123,-0.0143),
  s = c( 22.23,5.283)
  shape_lambda = runif(1,5,10)#7.159
  sigmapi_D = runif(2,1,2)#c( 1.651,1.651)
  sigmapi_F = runif(1,0,1)#0.719#c( 0.719,1.118),
  sigmapi_MP = runif(1,0,1)#0.278
  sigmapi_R = runif(1,0,1)#0.1693
  #sigmapi_oF = runif(1,0,1)#0.5865
  
  m_F = structure(.Data = c(
    5,0,53,4,68,
    3,19,4,64,2,
    21,2,22,2,24,
    1,13,0),
    .Dim = c(9,2))

  um_F = structure(.Data = c(
    36,20,22,7,24,
    8,16,4,6,
    3, 4,5,19,8,7,5,11,1),
    .Dim = c(9,2))

  mupi_D = structure(.Data = c(
    0.007185,0.009917,0.3953,0.1231),
    .Dim = c(2,2))
  
  mupi_F = structure(.Data = c(
    0.06538,0.1012,0.1096,0.1179),
    .Dim = c(2,2))
  
  # Cmuo_F= structure(.Data= c(12, 1, 13, 3, 30, 2, 5, 2, 7, 1, 2, 2, 2, 2, 6, 1, 6, 1),
  #                   .Dim=c(9, 2))
#  )
m_F[]<-as.integer(m_F)
um_F[]<-as.integer(um_F)
#Cmuo_F[]<-as.integer(Cmuo_F)
# storage.mode(m_F) <- "integer"
# storage.mode(um_F) <- "integer"

###################################################
# TO UPDATE
###################################################
# METTRE A JOUR /!\ TAILLE MATRICE
# n = structure(.Data = c(
#   533.0,82.0,769.0,92.0,726.0,
#   78.0,476.0,73.0,594.0,25.0,
#   251.0,127.0,265.0,47.0,369.0,
#   41.0,563.0,19.0,234.0,68.0,
#   1145.0,83.0,434.0,118.0,887.0,
#   123.0,456.0,107.0,276.0,125.0,
#   247.0,117.0,816.0,75.0,403.0,
#   215.0,395.0,156.0,595.0,100.0,
#   693.0,172.0),
#   .Dim = c(21,2))
#int(C_MP[t,a]*(Cm_R[t,a]+Cum_R[t,a])/Cm_R[t,a]) 
n <- array(,dim=c(data$Y,2))
n[,1] = as.integer(((C_MP[,1]*(Cm_R[,1]+Cum_R[,1]))/(Cm_R[,1]+1)) + 50)
n[,2] = as.integer(((C_MP[,2]*(Cm_R[,2]+Cum_R[,2]))/(Cm_R[,2]+1)) + 50)

# METTRE A JOUR
# lambda_tot = c(
#   657.7,858.0,804.5,531.5,685.3,
#   300.6,297.1,377.1,598.9,325.5,
#   1213.0,543.5,953.4,564.7,428.0,
#   323.3,1021.0,560.0,509.3,757.1,
#   704.7)
#lambda_tot <- n[t,1]+n[t,2]
lambda_tot <- as.integer(rowSums(n)) 

# METTRE A JOUR /!\ TAILLE MATRICE
# logit_piD_1SW = structure(.Data = c(
#   -5.505,-4.538,-2.152,-4.821,-5.811,
#   -4.212,-3.23,-4.832,-4.401,-4.521,
#   -3.453,-4.725,-3.666,-4.722,-4.19,
#   -4.923,-7.506,-4.15,-4.464,-4.885,
#   -5.013,-4.597,-5.011,-5.009,-3.422,
#   -4.719,-6.424,-4.257,-6.555,-4.69,
#   -4.386,-4.582,-5.61,-4.633,-4.481,
#   -4.751,-4.474,-4.743,-6.857,-4.279,
#   -5.203,-4.779),
#   .Dim = c(21,2))
logit<-function(x) {log(x/(1-x))}
logit_piD_1SW <- array( ,dim=c(data$Y,2))
logit_piD_1SW[,1] <- logit(((data$Cm_D[,1]+1)*1)/n[,1])
logit_piD_1SW[,2] <- logit(((data$Cum_D[,1]+1)*1)/n[,1])

# METTRE A JOUR /!\ TAILLE MATRICE
# logit_piD_MSW = structure(.Data = c(
#   -1.171,-2.608,1.803,-0.888,-0.8695,
#   -2.472,2.289,-1.474,-5.237,-6.006,
#   -0.5674,-1.276,3.036,0.4868,-2.076,
#   -4.467,-4.266,-2.78,-1.235,-2.244,
#   -2.393,-3.572,-0.8831,-0.5852,-11.2,
#   -8.947,-1.008,-2.702,-4.242,-5.694,
#   -3.505,-2.917,-6.716,-5.11,-3.543,
#   -4.515,-5.506,-4.365,-7.82,-6.54,
#   -11.08,-8.13),
#   .Dim = c(21,2))
logit_piD_MSW <- array( ,dim=c(data$Y,2))
logit_piD_MSW[,1] <- logit(((data$Cm_D[,2]+1)*1)/n[,2])
logit_piD_MSW[,2] <- logit(((data$Cum_D[,2]+1)*1)/n[,2])
logit_piD_MSW[is.na(logit_piD_MSW)] <- -2 # si NA, mettre -2

# METTRE A JOUR /!\ TAILLE MATRICE
# logit_piF_1SW = structure(.Data = c(
#   -2.886,-2.263,-2.169,-2.712,-1.944,
#   -2.162,-2.69,-1.634,-1.701,-3.674,
#   -2.527,-2.959,-1.46,-1.485,-1.865,
#   -2.729,-3.657,-2.465,            NA,            NA,
#   -2.539,-2.265,-2.814,-1.942,-2.498,
#   -2.08,-2.771,-1.562,-3.154,-3.343,
#   -3.192,-3.431,-2.862,-2.528,-2.382,
#   -3.078,-2.785,-2.242,-3.16,-2.265,
#   -3.184,-1.315),
#   .Dim = c(21,2))
logit_piF_1SW <- array( ,dim=c(data$Y,2))
t = 1:9
logit_piF_1SW[t,1] <- logit(((m_F[t,1]+1)*1)/n[t,1])
logit_piF_1SW[t,2] <- logit(((um_F[t,1]+1)*1)/n[t,1])
t = 11:data$Y
logit_piF_1SW[t,1] <- logit(((data$Cm_F[t,1]+1)*1)/n[t,1])
logit_piF_1SW[t,2] <- logit(((data$Cum_F[t,1]+1)*1)/n[t,1])
#logit_piF_1SW[is.na(logit_piF_1SW)] <- -2 # si NA, mettre -2
#logit_piF_1SW[10,]<- NA

# METTRE A JOUR /!\ TAILLE MATRICE
# logit_piF_MSW = structure(.Data = c(
#   -2.914,-1.244,-2.183,-1.675,-2.262,
#   -1.685,-2.429,-1.44,-2.687,-1.19,
#   -2.247,-3.04,-2.325,-1.321,-2.725,
#   -1.433,-1.215,-1.968,-1.833,-1.435,
#   -2.482,-1.557,-1.599,-1.192,-1.898,
#   -0.9972,-1.858,-1.94,-2.109,-1.917,
#   -2.173,-1.977,-2.244,-1.87,-2.757,
#   -2.847,-2.202,-1.681,-2.741,-2.932,
#   -2.266,-2.322),
#   .Dim = c(21,2))
# t:1->9, logit((m_F[t,2]+1)*10/n[t,2]),logit((um_F[t,2]+1)*10/n[t,2])  
# t:10->Y, logit((Cm_F[t,2]+1)*10/n[t,2]),logit((Cum_F[t,2]+1)*10/n[t,2]) 
logit_piF_MSW <- array(NA ,dim=c(data$Y,2))
t = 1:9
logit_piF_MSW[t,1] <- logit(((m_F[t,2]+1)*1)/n[t,2])
logit_piF_MSW[t,2] <- logit(((um_F[t,2]+1)*1)/n[t,2])
t = 10:data$Y
logit_piF_MSW[t,1] <- logit(((data$Cm_F[t,2]+1)*1)/n[t,2])
logit_piF_MSW[t,2] <- logit(((data$Cum_F[t,2]+1)*1)/n[t,2])
#logit_piF_MSW[is.na(logit_piF_MSW)] <- -2 # si NA, mettre -2

# t = 1:9
# logit_piF_MSW[t,] <- logit(um_F[t,]/(n[t,] - data$C_MP[t,] + data$Cum_MP[t,]))
# t = 10:data$Y
# logit_piF_MSW[t,] <- logit(data$Cum_F[t,]/(n[t,] - data$C_MP[t,] + data$Cum_MP[t,]))


# METTRE A JOUR /!\ TAILLE MATRICE
# logit_pi_MP = structure(.Data = c(
#   0.6621,-0.7564,0.6431,-0.312,0.7835,
#   -0.3964,0.699,0.07303,1.169,-0.5294,
#   0.8838,-0.9131,0.4053,-0.8713,0.438,
#   -0.8825,0.9323,-0.6142,0.2761,-0.4666,
#   0.6087,-0.4572,1.255,0.3555,1.031,
#   -0.8378,0.8297,-0.2528,0.8335,-1.024,
#   1.017,-0.05127,0.7326,-0.2741,0.9695,
#   0.5971,0.3776,-0.562,0.6769,-0.5389,
#   0.517,-0.3414),
#   .Dim = c(21,2))
logit_pi_MP <- array( ,dim=c(data$Y,2))
logit_pi_MP[,1] <- logit(data$Cm_MP[,1]/n[,1]) 
logit_pi_MP[,2] <- logit(data$Cm_MP[,2]/n[,2]) 
# logit_pi_MP[is.na(logit_pi_MP)] <- 0 # si NA, mettre 0
# logit_pi_MP[logit_pi_MP =="Inf"] <- 0 # si Inf, mettre 0


# METTRE A JOUR /!\ TAILLE MATRICE
# m_D = structure(.Data = c(
#   2.0,2.0,45.0,31.0,2.0,
#   9.0,10.0,32.0,1.0,0.0,
#   1.0,13.0,3.0,9.0,1.0,
#   5.0,0.0,0.0,3.0,5.0,
#   6.0,3.0,3.0,17.0,15.0,
#   0.0,0.0,8.0,0.0,0.0,
#   3.0,0.0,1.0,0.0,4.0,
#   8.0,1.0,1.0,0.0,0.0,
#   1.0,0.0),
#   .Dim = c(21,2))
#Cm_D*10
m_D <- array( ,dim=c(data$Y,2))
m_D[,1] <- data$Cm_D[,1]*1
m_D[,2] <- data$Cm_D[,2]*1



# METTRE A JOUR /!\ TAILLE MATRICE
# um_D = structure(.Data = c(
#   1.0,4.0,0.0,13.0,5.0,
#   4.0,4.0,6.0,2.0,0.0,
#   1.0,21.0,1.0,17.0,1.0,
#   2.0,2.0,0.0,0.0,4.0,
#   3.0,0.0,0.0,6.0,5.0,
#   0.0,5.0,5.0,1.0,0.0,
#   0.0,1.0,4.0,0.0,0.0,
#   2.0,0.0,3.0,3.0,0.0,
#   0.0,0.0),
#   .Dim = c(21,2))
um_D <- array( ,dim=c(data$Y,2))
um_D[,1] <- data$Cum_D[,1]*1 
um_D[,2] <- data$Cum_D[,2]*1


# METTRE A JOUR /!\ TAILLE MATRICE
# Additional inits needed since 2022 
Cum_Fb <- data$Cum_F; Cum_Fb[10:26,]<-NA
Cm_Fb <- data$C_F - data$Cum_F

# METTRE A JOUR /!\ TAILLE MATRICE
# logit_pi_R = structure(.Data = c(
#   -3.075,-2.512,-2.06,-1.641,-2.104,
#   -1.635,-2.66,-2.219,-1.937,-2.097,
#   -1.898,-2.007,-3.372,-2.739,-1.616,
#   -1.638,-2.259,-2.253,-1.294,-1.576,
#   -1.443,-1.742,-1.81,-1.602,-2.305,
#   -1.894,-1.764,-1.866,-1.846,-1.924,
#   -2.545,-2.277,-1.725,-2.27,-2.689,
#   -2.323,-2.893,-2.391,-2.007,-1.876,
#   -2.049,-2.03),
#   .Dim = c(21,2))
#logit(Cm_R/n) V?rifier si ?a marche
logit_pi_R <- array( ,dim=c(data$Y,2))
# logit_pi_R[,1] <- logit((data$Cm_R[,1]+1)/(n[,1]/10))
# logit_pi_R[,2] <- logit((data$Cm_R[,2]+1)/(n[,2]/10))
em <- ((data$Cm_MP - data$Cm_F) - m_D)
logit_pi_R <- logit(data$Cm_R /em)
logit_pi_R[is.na(logit_pi_R)] <- -1.5 # si NA, mettre -1.5
logit_pi_R[logit_pi_R =="-Inf"] <- -1.5 # si NA, mettre -1.5


pi_R_pulsium=array(,dim=c(data$Y,2))
for (t in 27:data$Y) {
   pi_R_pulsium[t,1] <- 0.1    
   pi_R_pulsium[t,2] <- 0.1 
}

# Proprotion sampling for sexing
p_smp=array(,dim=c(data$Y,2))
p_smp[,1] <-rbeta(data$Y,1, 5) # 1SW
p_smp[,2] <-rbeta(data$Y,5, 1) # MSW

# proportion male by sea age
p_male=array(,dim=c(data$Y,2))
p_male[,1] <-0.5
p_male[,2] <- 0.2

inits_fix <- list(
  lambda_tot0 = lambda_tot0,
  logit_effort_R = logit_effort_R,
  logit_flow_MP = logit_flow_MP,
  logit_flow_R = logit_flow_R,
  logit_int_MP = logit_int_MP,
  logit_int_R = logit_int_R,
 # logit_pi_oF = logit_pi_oF,
 # mupi_oF = mupi_oF,
  pi_MP94 = pi_MP94,
  pi_oD = pi_oD,
  rate_lambda = rate_lambda,
  rho_D = rho_D,
  rho_F = rho_F,
  s = s,
  shape_lambda = shape_lambda,
  sigmapi_D = sigmapi_D,
  sigmapi_F = sigmapi_F,
  sigmapi_MP = sigmapi_MP,
  sigmapi_R = sigmapi_R,
#  sigmapi_oF = sigmapi_oF,
  
  #m_F=m_F,
  #um_F=um_F,
  #mupi_D = mupi_D,
  mupi_F = mupi_F
  #Cmuo_F = Cmuo_F
)

inits_updated <- list(
  lambda_tot = lambda_tot
  ,logit_piD_1SW=logit_piD_1SW
  ,logit_piD_MSW=logit_piD_MSW
  ,logit_piF_1SW=logit_piF_1SW
  ,logit_piF_MSW=logit_piF_MSW
  ,logit_pi_MP=logit_pi_MP
  ,logit_pi_R=logit_pi_R
  ,m_D=m_D
  ,n=n
  ,um_D=um_D
  ,Cum_Fb=Cum_Fb,Cm_Fb=Cm_Fb
  ,pi_R_pulsium=pi_R_pulsium
  ,p_smp=p_smp
  ,p_male=p_male
)

inits <- list(c( inits_fix,inits_updated))

#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))

bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,"_",c,'.txt',sep=""))
} #end loop c

