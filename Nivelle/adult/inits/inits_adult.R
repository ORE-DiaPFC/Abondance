#rm(list=ls())   # Clear memory

##-----------------------------DATA ----------------------------------##
#year <- 2015
#site <- "Nivelle"
#stade <- "adult"

## WORKING DIRECTORY:
# work.dir<-paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/',site,sep="")
# setwd(work.dir)

load(paste('data/data_',stade,"_",year,'.Rdata',sep=""))


for (c in 1:2){ # 2 chains
#------------------------INITS----------------------------------##
inits0 <- read.bugsdata(paste("inits/init-",site,"-",stade,as.numeric(year)-1,"_",c,".txt",sep=""))
#save(inits0,file=paste('inits/inits_',stade,as.numeric(year)-1,'.Rdata',sep=""))
#load(paste('inits/inits_',stade,as.numeric(year)-1,'.Rdata',sep=""))


###################################################
# NO UPDATE
###################################################
inits_fix <- list(
  ## NO UPDATE
  shape_lambda = runif(1,2,7),#5,
  rate_lambda = runif(1,0,0.1),#0.01,
  lambda_tot0 = 289.39+runif(1,-5,5),
  a_1.1SW = runif(1,2,5),#4.2,
  a_1.1SW = runif(1,0,.1),#0.08,
  a_MSW = runif(5,0,1),#c( 0.4,0.9059,2.516,0.455,0.3037),
  eta_1 = runif(1,0,2),#1.35,
  eta_2 = runif(1,4,6),#5.5,
  k_1 = runif(1,.6,1),#0.87,
  k_2 = runif(1,1,2),#1.46,
  logit_p_11_1 = c( -1.42,-1.42,-1.42,-1.42,-1.42, -1.42,-1.42,-1.42)+runif(1,-.2,.2),
  logit_pi_EF = c( -1.68,-1.68,-1.68,-1.68,-1.68,-1.68,-1.68,-1.68)+runif(1,-.2,.2),
  mup_11_1 = 0.21,
  mup_11_2 = 0.18,
  mup_21 = 0.42,
  mupi_EF = 0.16,
  mupi_U = runif(2,.5,1),#c(0.8689,0.8146),
  rho = runif(1,0.5,1),#0.79,
  precp_11_1 = 1,
  precp_11_2 = 1,
  precp_12 = c( 1, 1, 1, 1),
  precp_21 = 1,
  precpi_EF = 1,
  precpi_U = c( 1, 1),
  precpi_Ol = 1,
  #shape_prec = 30,
  #rate_prec = 10,
  mean_var = runif(1,2,7),#5,
  rate_prec = runif(1,1,3),#2,
  d_pi_Ol = 0,
  d_pi_U = c(0, 0),
  logit_pi_Ol = log(data$eff_Ol / (1 - data$eff_Ol))
)



###################################################
# TO UPDATE
###################################################

#inits_updated <- list(
# Les p_MSW ne sont pas initialis?es, ? dessein
# initialisation par BUGS pours assurer une somme ?gale ? 1

  ## METTRE A JOUR
  # alpha_1 = c(
  #   2.96,1.06,1.07,0.63,1.09,
  #   0.31,0.39,0.58,0.21,0.79,
  #   0.68,0.56,0.52,0.71,0.72,
  #   0.43,0.77,0.48,1.47,1.5,
  #   1.39,1.26,1.37,1.24,1.0,
  #   1.0,1.0,1.0, 1.0,1,
  #   1,1),
  alpha_1 <- c(inits0$alpha_1, 1) # ajouter 1
  
  ## METTRE A JOUR
  # alpha_2 = c(
  #   NA,            NA,            NA,            NA,            NA,
  #   NA,0.81,0.97,1.18,2.08,
  #   1.77,1.22,1.31,1.62,1.5,
  #   1.41,1.0,0.93,2.76,2.13,
  #   2.44,1.67,2.28,2.22,1.0,
  #   1.0,1.0,1.0,1.0,1,
  #   1,1),
  alpha_2 <- c(inits0$alpha_2, 1) # ajouter 1
  
  
  
  # nm_2 <- array(NA,dim=c(data$Y,4))
  # nm_2[c(29:data$Y),] <- data$Cm_U[c(29:data$Y),]
  # nm_2[41,2]<-NA
  
  ## METTRE A JOUR /!\ TAILLE MATRICE
  # e_21 = structure(.Data = c(
  #   NA,            NA,            NA,            NA,            NA,
  #   NA,            NA,            NA,            NA,            NA,
  #   NA,            NA,            NA,            NA,            NA,
  #   NA,            NA,            NA,            NA,            NA,
  #   NA,            NA,            NA,            NA,20.0,
  #   23.0,1.0,3.0,19.0,11.0,
  #   1.0,6.0,31.0,28.0,1.0,
  #   7.0,76.0,67.0,1.0,6.0,
  #   39.0,32.0,1.0,6.0,32.0,
  #   30.0,2.0,3.0,19.0,38.0,
  #   1.0,12.0,10.0,22.0,1.0,
  #   3.0,27.0,26.0,1.0,4.0,
  #   26.0,26.0,3.0,4.0,19.0,
  #   14.0,NA,3.0,42.0,30.0,
  #   1.0,8.0,55.0,60.0,1.0,
  #   8.0,1.0,5.0,5.0,10.0,
  #   7.0,5.0,            NA,2.0,11.0,
  #   11.0,            NA,3.0,5.0,3.0,
  #   2.0,5.0,10.0,7.0,2.0,
  #   3.0,6.0,5.0,            NA,7.0,
  #   15.0,6.0,            NA,6.0,30.0,
  #   25.0,5.0,5.0,5.0,5.0,
  #   7.0,8.0,
  #   3.0,9.0,1.0,3.0,
  #   3.0,9.0,1.0,3.0,
  #   3.0,9.0,1.0,3.0,
  #   3.0,2.0,1.0,3.0),
  #   .Dim = c(32,4)),

  ## nm_2[t,g]: Annual number of marked fish captured at Olha per breeding category (when trapping is total, globally from 92 to 2011)
  ## num_2[t,g]: Annual number of unmarked fish captured at Olha per breeding category (when trapping is total, globally from 92 to 2011)
  # num_2.tmp <- data$C_U #cbind(rowSums(data$C_U[,1:2]), rowSums(data$C_U[,3:4]))
  # num_2 <- array(NA,dim=c(data$Y,4))
  # num_2[c(29:data$Y),] <- num_2.tmp[c(29:data$Y),]

  

e_2.tmp = ((data$Cm_O[data$Y, ] + data$Cum_O[data$Y, ])/ tail(data$eff_Ol,n=1)) + data$NB[data$Y, ] +1
e_21.tmp = ceiling(e_2.tmp / 2)
e_21 = rbind(inits0$e_21, e_21.tmp)
#e_21 = e_21.tmp
#ifelse(e_21==0, NA, e_21) # get error if = 0
e_21[33,3]<-1 # e_21[33,3] cannot be 0!!!
e_21[41,2]<-NA

num_2 <- array(NA,dim=c(data$Y,4))
num_2[c(29:data$Y),] <- e_21[c(29:data$Y),]
num_2[41,2]<-NA #mb-11.4.2025

 
  ## METTRE A JOUR
  # logit_p_11_2 = c(
  #   NA,            NA,            NA,            NA,            NA,
  #   NA,            NA,            NA,-1.98,-1.98,
  #   -1.98,-1.98,-1.98,-1.98,-1.98,
  #   -1.98,-1.98,-1.98,-1.98,-1.98,
  #   -1.98,-1.98,-1.98,-1.98,-1.98,
  #   -1.98,-1.98,-1.98,-1.98,-1.98,
  #   -1.98,-1.98),
  logit_p_11_2 <- c(inits0$logit_p_11_2, -1.98) # ajouter 1
  
  ## METTRE A JOUR
  # logit_p_21 = c(
  #   NA,            NA,            NA,            NA,            NA,
  #   NA,0.28,0.28,0.28,0.28,
  #   0.28,0.28,0.28,0.28,0.28,
  #   0.28,0.28,0.28,0.28,0.28,
  #   0.28,0.28,0.28,0.28,0.28,
  #   0.28,0.28,0.28,0.28,0.28,
  #   0.28,0.28),
  logit_p_21 <- c(inits0$logit_p_21, 0.28) # ajouter 1
  
  ## METTRE A JOUR /!\ TAILLE MATRICE
  # logit_pi_U = structure(.Data = c(
  #   1.77,0.98,1.77,0.98,1.77,
  #   0.98,1.77,0.98,1.77,0.98,
  #   1.77,0.98,1.77,0.98,1.77,
  #   0.98,1.77,0.98,1.77,0.98,
  #   1.77,0.98,1.77,0.98,1.77,
  #   0.98,1.77,0.98,1.77,0.98,
  #   1.77,0.98,1.77,0.98,1.77,
  #   0.98,1.77,0.98,1.77,0.98,
  #   1.77,0.98,1.77,0.98,1.77,
  #   0.98,1.77,0.98,1.77,0.98,
  #   1.77,0.98,1.77,0.98,1.77,
  #   0.98,1.77,0.98,1.77,0.98,
  #   0.98,1.77,0.98,1.77),
  #   .Dim = c(32,2)),
  logit_pi_U <- rbind(inits0$logit_pi_U, c(1.77, 0.98))

  
  ## METTRE A JOUR /!\ TAILLE MATRICE
  # logit_p_n12 = structure(.Data = c(
  #   NA	,	NA	,	NA	,	NA	,
  #   NA	,	NA	,	NA	,	NA	,
  #   NA	,	NA	,	NA	,	NA	,
  #   NA	,	NA	,	NA	,	NA	,
  #   NA	,	NA	,	NA	,	NA	,
  #   NA	,	NA	,	NA	,	NA	,
  #   NA	,	NA	,	NA	,	NA	,
  #   NA	,	NA	,	NA	,	NA	,
  #   0.7145	,	0.5695	,	0.375	,	0.3009	,
  #   0.8807	,	0.5645	,	0.3835	,	0.4143	,
  #   0.6828	,	0.4388	,	0.7579	,	0.3558	,
  #   0.9001	,	0.6123	,	0.4701	,	0.2255	,
  #   0.6797	,	0.8035	,	0.5837	,	0.5936	,
  #   0.6256	,	0.7106	,	0.662	,	0.4649	,
  #   0.6674	,	0.7371	,	0.6386	,	0.584	,
  #   0.8121	,	0.7681	,	0.6276	,	0.4326	,
  #   0.6753	,	0.62	,	0.5349	,	0.3694	,
  #   0.8184	,	0.6875	,	0.4171	,	0.599	,
  #   0.5361	,	0.6362	,	0.4277	,	0.4703	,
  #   0.6011	,	0.6919	,	0.7274	,	0.4548	,
  #   0.4886	,	0.4174	,	0.3246	,	0.3723	,
  #   0.7504	,	0.6154	,	0.5458	,	0.4552	,
  #   0.5781	,	0.5544	,	0.6497	,	0.4266	,
  #   0.7847	,	0.6114	,	0.6467	,	0.4577	,
  #   0.5891	,	0.4394	,	0.3231	,	0.5606	,
  #   0.7605	,	0.6754	,	0.5468	,	0.5673	,
  #   0.7693	,	0.6373	,	0.7643	,	0.5382	,
  #   0.7233	,	0.5082	,	0.8754	,	0.4834	,
  #   0.6227	,	0.6204	,	0.6583	,	0.3513	,
  #   0.5049	,	0.5467	,	0.4616	,	0.6162	,
  #   0.5049	,	0.5467	,	0.4616	,	0.6162	,
  #   0.5049	,	0.5467	,	0.4616	,	0.6162),
  #   .Dim = c(32,4)),
  logit_p_n12.tmp = c(0.5049	,	0.5467	,	0.4616	,	0.6162)
  logit_p_n12 = rbind(inits0$logit_p_n12, logit_p_n12.tmp)
  
  ## METTRE A JOUR /!\ TAILLE MATRICE
  # n = structure(.Data = c(
  #   71.0,101.0,8.0,15.0,42.0,
  #   35.0,9.0,38.0,154.0,158.0,
  #   11.0,30.0,78.0,134.0,10.0,
  #   41.0,84.0,58.0,4.0,41.0,
  #   104.0,104.0,18.0,49.0,141.0,
  #   150.0,11.0,29.0,68.0,75.0,
  #   14.0,49.0,104.0,114.0,10.0,
  #   46.0,265.0,324.0,8.0,55.0,
  #   181.0,217.0,6.0,50.0,91.0,
  #   104.0,17.0,43.0,78.0,110.0,
  #   7.0,52.0,45.0,80.0,3.0,
  #   12.0,90.0,82.0,5.0,14.0,
  #   81.0,78.0,19.0,28.0,74.0,
  #   59.0,7.0,24.0,95.0,89.0,
  #   5.0,32.0,212.0,200.0,5.0,
  #   36.0,13.0,18.0,13.0,49.0,
  #   43.0,46.0,4.0,17.0,39.0,
  #   48.0,2.0,13.0,25.0,21.0,
  #   7.0,27.0,34.0,31.0,6.0,
  #   15.0,25.0,30.0,5.0,25.0,
  #   35.0,20.0,2.0,20.0,70.0,
  #   60.0,10.0,15.0,15.0,25.0,
  #   15.0,30.0,
  #   75, 167,  96,  83,
  #   95, 187,  103,  93,
  #   104.0,114.0,10.0,46.0,
  #   25, 15, 10, 25),
  #   .Dim = c(32,4)),
  
  n.tmp = data$C_U[data$Y,] / 0.1
  #n.tmp = data$C_U / 0.1
  n = rbind(inits0$n, ceiling(n.tmp))
  n[33,2]<-20
  n[33,3]<- n[33,3] + 10 # need to be > to escapment
  n[41,2]<- n[41,2] + 1 #mb-11.4.2025: cannot be 0
  
  
  ## METTRE A JOUR /!\ TAILLE MATRICE
  # n_11 = structure(.Data = c(
  #   9.0,18.0,0.0,2.0,6.0,
  #   5.0,1.0,6.0,28.0,29.0,
  #   1.0,5.0,11.0,19.0,1.0,
  #   5.0,9.0,7.0,0.0,4.0,
  #   21.0,22.0,2.0,8.0,18.0,
  #   26.0,1.0,4.0,12.0,14.0,
  #   2.0,6.0,16.0,18.0,1.0,
  #   7.0,34.0,48.0,0.0,6.0,
  #   25.0,31.0,0.0,6.0,13.0,
  #   16.0,1.0,6.0,11.0,16.0,
  #   1.0,6.0,6.0,11.0,0.0,
  #   1.0,13.0,12.0,0.0,1.0,
  #   12.0,11.0,1.0,3.0,12.0,
  #   9.0,0.0,3.0,2.0,14.0,
  #   0.0,3.0,35.0,32.0,0.0,
  #   4.0,1.0,2.0,1.0,7.0,
  #   6.0,7.0,0.0,2.0,5.0,
  #   7.0,0.0,1.0,3.0,2.0,
  #   0.0,3.0,4.0,4.0,0.0,
  #   1.0,1.0,1.0,1.0,1.0,
  #   1.0,1.0,1.0,1.0,1.0,
  #   1.0,1.0,1.0,1.0,1.0,
  #   1.0,1.0,
  #   1.0,1.0,1.0,1.0,
  #   1.0,1.0,1.0,1.0,
  #   1.0,1.0,1.0,1.0,
  #   1.0,1.0,1.0,1.0),
  #   .Dim = c(32,4)),
  n_11.tmp = rep(1,4)
  n_11 = rbind(inits0$n_11, n_11.tmp)
  
  
  ## METTRE A JOUR
  # p_1.1SW = c(
  #   1.0,1.0,1.0,1.0,0.9567,
  #   1.0,1.0,1.0,0.8397,0.9363,
  #   1.0,1.0,0.9997,0.9996,1.0,
  #   0.9981,0.9487,1.0,1.0,0.9932,
  #   0.7131,1.0,0.6242,0.9886,1.0,
  #   0.9496,1.0,1.0, 1.0,0.98,
  #   0.95,0.95)
  p1.1SW.tmp = (data$ech_1.1SW / data$ech_1SW_tot)
  p_1.1SW = c(inits0$p_1.1SW, p1.1SW.tmp[data$Y])
  
  ## METTRE A JOUR
  # no_ech_1.1SW = c(
  #   157.0,14.0,78.0,63.0,63.0,
  #   49.0,88.0,31.0,36.0,311.0,
  #   221.0,45.0,33.0,24.0,34.0,
  #   43.0,26.0,35.0,154.0,7.0,
  #   13.0,18.0,7.0,13.0,8.0,
  #   13.0,5.0,3.0,2.0,5,
  #   5,2),
  n_1SW.tmp <- sum(n[data$Y,1:2])
  no_ech_1SW.tmp = n_1SW.tmp - data$ech_1SW_tot[data$Y]
  no_ech_1.1SW.tmp = no_ech_1SW.tmp * p_1.1SW[data$Y] # mb-2022
  no_ech_1.1SW = c(inits0$no_ech_1.1SW, ceiling(no_ech_1.1SW.tmp))
  
  # /!\ 2022: separation des wild/farm origin de 1984 à 1989
  n_1SW.tmp=no_ech_1SW.tmp=no_ech_1SW_wild=NULL
  p_1SW_wild=(data$ech_1SW_wild / data$ech_1SW_tot)
  for (y in 1:6){
    n_1SW.tmp[y] <- sum(n[y,1:2])
    no_ech_1SW.tmp[y] = n_1SW.tmp[y] - data$ech_1SW_tot[y]
    no_ech_1SW_wild[y] = ceiling(no_ech_1SW.tmp[y] * p_1SW_wild[y])
    no_ech_1.1SW[y] = ceiling(no_ech_1SW_wild[y] * p_1.1SW[y]) 
  }
  




  
  ## METTRE A JOUR /!\ TAILLE MATRICE
  # no_ech_MSW = structure(.Data = c(
  #   2.0,4.0,2.0,2.0,            NA,
  #   0.0,2.0,7.0,1.0,            NA,
  #   2.0,0.0,7.0,1.0,            NA,
  #   0.0,1.0,4.0,5.0,            NA,
  #   0.0,6.0,8.0,5.0,            NA,
  #   2.0,9.0,13.0,0.0,            NA,
  #   0.0,1.0,6.0,1.0,            NA,
  #   5.0,0.0,18.0,1.0,            NA,
  #   0.0,0.0,12.0,0.0,            NA,
  #   16.0,13.0,9.0,4.0,            NA,
  #   1.0,12.0,15.0,0.0,            NA,
  #   3.0,0.0,19.0,3.0,            NA,
  #   0.0,3.0,2.0,9.0,            NA,
  #   1.0,2.0,1.0,0.0,            NA,
  #   0.0,5.0,1.0,0.0,            NA,
  #   5.0,8.0,9.0,3.0,            NA,
  #   0.0,4.0,2.0,1.0,            NA,
  #   8.0,0.0,10.0,1.0,            NA,
  #   0.0,5.0,7.0,0.0,            NA,
  #   0.0,0.0,11.0,0.0,            NA,
  #   1.0,0.0,4.0,0.0,            NA,
  #   3.0,0.0,1.0,1.0,            NA,
  #   4.0,0.0,5.0,2.0,            NA,
  #   0.0,3.0,3.0,2.0,            NA,
  #   0.0,3.0,4.0,0.0,            NA,
  #   1.0,0.0,3.0,0.0,            NA,
  #   2.0,0.0,2.0,0.0,            NA,
  #   0.0,0.0,2.0,5.0,            NA,
  #   2.0,0.0,2.0,0.0,            NA,
  #   4.0,0.0,5.0,2.0,            NA,
  #   8.0,0.0,10.0,1.0,            NA,
  #   3,1,0,0,NA),
  #   .Dim = c(32,5)),
  
  n_MSW.tmp <- sum(n[data$Y,3:4])
  pMSW.tmp = inits0$a_MSW / sum(inits0$a_MSW)
  no_ech_MSW.tmp = n_MSW.tmp - data$ech_MSW_tot[data$Y]
  no_ech_MSW.tmp = no_ech_MSW.tmp * pMSW.tmp[1:4]
  no_ech_MSW = rbind(inits0$no_ech_MSW, c(as.integer(no_ech_MSW.tmp),NA))
  
  n_MSW.tmp=no_ech_MSW.tmp=no_ech_MSW_wild=NULL
  p_MSW_wild=(data$ech_MSW_wild / data$ech_MSW_tot)
  for (y in 1:6){
    n_MSW.tmp[y] <- sum(n[y,3:4])
    no_ech_MSW.tmp[y] = n_MSW.tmp[y] - data$ech_MSW_tot[y]
    no_ech_MSW_wild[y] = ceiling(no_ech_MSW.tmp[y] * p_MSW_wild[y])
    
    for (j in 1:4){ # 1.2/2.2/1.3/2.3/autres
      no_ech_MSW[y,j] = round(no_ech_MSW_wild[y] * pMSW.tmp[j], 0) 
    }
  }
  
  # # to avoid no integer value for no_ech_MSW
  # Pas d'explications à ce jour!!! mb+ep-2024
  no_ech_MSW[1,3]<-0
  no_ech_MSW[2,3]<-0
  no_ech_MSW[3,3]<-0 
  no_ech_MSW[4,3]<-0
  
 # no_ech_MSW[41,1:4]<-0 #mb-11.4.2025


inits_updated <- list(
  alpha_1=alpha_1,
  alpha_2=alpha_2,
  e_21=e_21,
  logit_p_11_2=logit_p_11_2,
  logit_p_21=logit_p_21,
  logit_pi_U=logit_pi_U,
  logit_p_n12=logit_p_n12,
  n=n,
  n_11=n_11,
  p_1.1SW=p_1.1SW,
  
  no_ech_1SW_wild=no_ech_1SW_wild,p_1SW_wild=p_1SW_wild[1:6],
  no_ech_MSW_wild=no_ech_MSW_wild,p_MSW_wild=p_MSW_wild[1:6],
  
  no_ech_1.1SW=no_ech_1.1SW,
  no_ech_MSW = no_ech_MSW,
  num_2 =num_2
  #,nm_2=nm_2
)

inits <- list(c( inits_fix,inits_updated))


#write(inits[[1]],file=paste("inits/init-",site,"-",stade,year,".txt",sep=""), append=TRUE)
#lapply(inits[[1]], write, paste("inits/init-",site,"-",stade,year,".txt",sep=""), append=TRUE)
#save(inits,file=paste('inits/inits_',stade,'.Rdata',sep=""))
#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
#bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,'.txt',sep=""))
bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,"_",c,'.txt',sep=""))
} #end loop c