### Ce script est l? pour analyser les r?sultats des fichiers CODA obtenus avec le mod?le TACON de la Nivelle
### Besoin pour cela d'avoir enregistrer les fichiers CODA des 3 chaines MCMC.
### Permet d'obtenir des fichiers textes avec les estimations finales d'abondance
### /!\ Il faut mettre ? jour l'index de la derni?re ann?e (Y_last) et l'index du dernier site (I)
## /!\ Ce fichier n?cessite d'?crire 3 lignes de code en plus pour finir la mise ? jour (voir L.309-311)

##------------------ R PACKAGES ------------------------------##
library(coda)

##-----------------------------DATA ----------------------------------##
#year <- 2019
site <- "Nivelle"
stade <- "tacon"


## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="")
#setwd(work.dir)

# data.dir <- paste("data/","data-",stade,"-",year,".txt",sep="")
# data <- read.bugsdata(data.dir)
load(paste('data/data_',stade,"_",year,'.Rdata',sep="")) # chargement des données
  
# loading coda
load(paste('results/Results_',stade,"_",year,'.RData',sep=""))

# Post processings =======================================================
# as.mcmc.bugs <- function(x){
#     n.chains <- x$n.chains
#     sims <- x$sims.array
#     n.thin <- x$n.thin
#     if (n.chains==1) return(coda:::mcmc(sims[, 1, ], thin=n.thin))
#     out <- vector("list", length=n.chains)
#     for (i in seq(n.chains)) out[[i]] <- mcmc(sims[, i, ], thin=n.thin)
#     out <- mcmc.list(out)
#     varnames(out) <- dimnames(sims)[[3]]
#     return(out)
# }
as.mcmc.bugs <- function(x.bugs) as.mcmc(x.bugs$sims.matrix)

#fit.mcmc <- as.mcmc(fit) # using jags
fit.mcmc <- as.mcmc.bugs(fit) # using bugs


### /!\ MISE A JOUR
# Index de la derni?re ann?e 
Y_last <- data$Y_last
# Index du dernier site
I <- data$I
# Total number of iter
n_iter <- dim(fit.mcmc)[1]   # fit$n.iter # 10 000 iterations * 3 chaines MCMC
# To transform coda object into mcmc object /!\ Mettre ? jour le dossier et peut ?tre l'it?ration de d?part et de fin des chaines MCMC
#fit <- read.openbugs(stem="M:\\Nivelle\\Juvenile-2014\\coda\\", start = 10001, end = 20000, quiet=T)
#fit.mcmc <- as.matrix(fit)
#colnames(fit.mcmc) <- varnames(fit)
                                        
# Import files to work with (indexation of sites) /!\ Certains fichiers doivent ?tre mis ? jour chaque ann?e!
#START_i <- read.csv("data/tacon/START_i.csv",header=T,sep=";") # 22 yrs in row, 5 zones in column (LN1, LN2, LN, HN, LUR). Sites only SR methods
LN1 <- rep(NA,22) # Basse Nivelle en-dessous de Uxondoa 
LN2 <- rep(NA,22) # Basse Nivelle au-dessus de Uxondoa 
LN <- c(NA, 1,11,21,31,41,50,59,68,71,78,87,248,96,256,263,104,113,123,132, 140,147) # Basse-Nivelle
HN <- c(NA,  NA, 337, 345, 353, 361, 369, 155, 164, 170, 178, 186, 270, 194, 277, 285, 293, 202, 301, 211, 309, 312) # Haut-Nivelle
LUR <-  c(NA,  NA, 377, 381, 385, 389, 393, 215, 218, 221, 225, 229, 317, 233, 320, 324, 237, 241, 328, 245, 332, 334) # Lurgorreta
START_i <-cbind(LN1,LN2,LN,HN,LUR)

# END_i <- read.csv("data/tacon/END_i.csv",header=T,sep=";")
#LN1 <- rep(NA,22) # Basse Nivelle en-dessous de Uxondoa 
#LN2 <- rep(NA,22) # Basse Nivelle au-dessus de Uxondoa 
LN <- c(NA,  10,  20,  30,  40,  49,  58,  67,  70,  77,  86,  95, 255, 103, 262, 269, 112, 122, 131, 139, 146, 154) # Basse-Nivelle
HN <- c(NA,  NA, 344, 352, 360, 368, 376, 163, 169, 177, 185, 193, 276, 201, 284, 292, 300, 210, 308, 214, 311, 316) # Haut-Nivelle
LUR <-  c(NA,  NA, 380, 384, 388, 392, 396, 217, 220, 224, 228, 232, 319, 236, 323, 327, 240, 244, 331, 247, 333, 336) # Lurgorreta
END_i <-cbind(LN1,LN2,LN,HN,LUR)


#START_i_bis <- read.csv("data/tacon/START_i_bis.csv",header=T,sep=";") # Sites with both SR & CPUE methods
# LN1 <- rep(NA,22) # Basse Nivelle en-dessous de Uxondoa 
# LN2 <- rep(NA,22) # Basse Nivelle au-dessus de Uxondoa 
LN <- c(rep(NA,19),423, 425, 427) # Basse-Nivelle
HN <- c(rep(NA,19),429, 435, 440)  # Haut-Nivelle
LUR <-  c(rep(NA,19), 434, 443, 445) # Lurgorreta
START_i_bis <-cbind(LN1,LN2,LN,HN,LUR)

#END_i_bis <- read.csv("data/tacon/END_i_bis.csv",header=T,sep=";")
#LN1 <- rep(NA,22) # Basse Nivelle en-dessous de Uxondoa 
#LN2 <- rep(NA,22) # Basse Nivelle au-dessus de Uxondoa 
LN <- c(rep(NA,19),424, 426, 428) # Basse-Nivelle
HN <- c(rep(NA,19),433, 439, 442)  # Haut-Nivelle
LUR <-  c(rep(NA,19), NA, 444, NA) # Lurgorreta
END_i_bis <-cbind(LN1,LN2,LN,HN,LUR)


## This file needs to be updated every year!!!!
## Mettre ? jour gr?ce au fichier: sites-until2014.csv;;;;
## Pour la colonne LN, mettre le dernier index correspondant ? la colonne Site, de la derni?re ann?e pour la zone 3;;;;
## Pour la colonne HN, mettre le dernier index correspondant ? la colonne Site, de la derni?re ann?e pour la zone 4;;;;
## Pour la colonne LUR, mettre le dernier index correspondant ? la colonne Site, de la derni?re ann?e pour la zone 5;;;;
#START_i_CPUE <- read.csv("data/tacon/START_i_CPUE-until2014.csv",header=T,sep=";") # Only CPUE sites. /!\ This file needs to be updated every year
LN1 <- rep(NA,Y_last) # Basse Nivelle en-dessous de Uxondoa 
LN2 <- rep(NA,Y_last) # Basse Nivelle au-dessus de Uxondoa 
LN_tmp <- c(rep(NA,19), 446, NA, 447,449,459,463,512,529,546,563,580,597) # Basse-Nivelle
LN_new <- LN_tmp[31]+(17*(1:(Y_last-31))) # incemente par 17 chaque année
LN <-c(LN_tmp,LN_new) # ajout à la fin du vecteur
HN_tmp <- c(rep(NA,19), 453, NA, 467, 474, 484, 494, 516, 533, 550, 567, 584, 601)  # Haut-Nivelle
HN_new <- HN_tmp[31]+(17*(1:(Y_last-31))) # incemente par 17 chaque année
HN <-c(HN_tmp,HN_new) # ajout à la fin du vecteur
LUR_tmp <-  c(rep(NA,19), 455, NA, 504, 456, 506, 509, 526, 543, 560, 577, 594, 611) # Lurgorreta
LUR_new <- LUR_tmp[31]+(17*(1:(Y_last-31))) # incemente par 17 chaque année
LUR <-c(LUR_tmp,LUR_new) # ajout à la fin du vecteur
START_i_CPUE <-cbind(LN1,LN2,LN,HN,LUR)


#END_i_CPUE <- read.csv("data/tacon/END_i_CPUE-until2014.csv",header=T,sep=";") # /!\ This file needs to be updated every year
# LN1 <- rep(NA,Y_last) # Basse Nivelle en-dessous de Uxondoa 
# LN2 <- rep(NA,Y_last) # Basse Nivelle au-dessus de Uxondoa 
LN_tmp <- c(rep(NA,21),448, 452, 462, 466, 515, 532, 549, 566,583, 600) # Basse-Nivelle
LN_new <- LN_tmp[31]+(17*(1:(Y_last-31))) # incemente par 17 chaque année
LN <-c(LN_tmp,LN_new) # ajout à la fin du vecteur
HN_tmp <- c(rep(NA,19), 454,  NA, 473, 483, 493, 503, 525, 542, 559, 576, 593, 610)  # Haut-Nivelle
HN_new <- HN_tmp[31]+(17*(1:(Y_last-31))) # incemente par 17 chaque année
HN <-c(HN_tmp,HN_new) # ajout à la fin du vecteur
LUR_tmp <-  c(rep(NA,21), 505, 458, 508, 511, 528, 545, 562, 579, 596, 613) # Lurgorreta
LUR_new <- LUR_tmp[31]+(17*(1:(Y_last-31))) # incemente par 17 chaque année
LUR <-c(LUR_tmp,LUR_new) # ajout à la fin du vecteur
END_i_CPUE <-cbind(LN1,LN2,LN,HN,LUR)

## Mettre ? jour en recopiant les 17 derni?res lignes pour les colonnes Z, S, H /!\ Conserver l'ordre;;;;;;;
## Y est l'ann?e donc ajouter le nouvel index de l'ann?e (17 lignes);;;;;;;
## Site: mettre ? jour en incr?mentant de un en un (17 lignes);;;;;;;
## Nat: idem que Site;;;;;;;
last <- function(x) { return( x[length(x)] ) }
#sites <- read.csv("data/sites-until2014.csv",header=T,sep=";",dec=".") # /!\ This file needs to be updated every year
sites <- read.csv("results/sites.csv",header=T,sep=";",dec=".") # /!\ This file needs to be updated every year
if (last(sites$Y)<Y_last) {
Site <- (last(sites$Site)+1) : (last(sites$Site)+17)
Nat <- (last(sites$Nat)+1) : (last(sites$Nat)+17)
Comp <- rep(NA,17)
Res <- rep(NA,17)
Y <- rep(Y_last,17)
Z <- c(rep(3,4),rep(4,10),rep(5,3))
S <- c(449,  341,  462, 1462,  629,  777,  858,  479,  523,   68, 1138,  457, 1542,  660,  230,  325,  931)
H <- rep(1,17)

  sites_tmp <- cbind(Site,Nat,Comp,Res,Y,Z,S,H)
  sites_new <- rbind(sites,sites_tmp) 
  write.table(sites_new,file="results/sites.csv",sep=";",row.names = FALSE)
  
    sites <- sites_new 
}



# Pas de mise ? jour ici
jcomp_ns_riff <- fit.mcmc[,(which(colnames(fit.mcmc)=="jcomp_ns_riff[13,3]", arr.ind=T)):(which(colnames(fit.mcmc)=="jcomp_ns_riff[26,3]", arr.ind=T))]
jcomp_ns_runs <- fit.mcmc[,(which(colnames(fit.mcmc)=="jcomp_ns_runs[13,3]", arr.ind=T)):(which(colnames(fit.mcmc)=="jcomp_ns_runs[26,3]", arr.ind=T))]
jnat_ns_riff <- fit.mcmc[,(which(colnames(fit.mcmc)=="jnat_ns_riff[2,3]", arr.ind=T)):
    (which(colnames(fit.mcmc)==(paste("jnat_ns_riff[",Y_last,",5]",sep="")), arr.ind=T))]
jnat_ns_runs <- fit.mcmc[,(which(colnames(fit.mcmc)=="jnat_ns_runs[2,3]", arr.ind=T)):
    (which(colnames(fit.mcmc)==(paste("jnat_ns_runs[",Y_last,",5]",sep="")), arr.ind=T))]
jres_ns_riff <- fit.mcmc[,(which(colnames(fit.mcmc)=="jres_ns_riff[3,4]", arr.ind=T)):(which(colnames(fit.mcmc)=="jres_ns_riff[12,8]", arr.ind=T))]
jres_ns_runs <- fit.mcmc[,(which(colnames(fit.mcmc)=="jres_ns_runs[3,4]", arr.ind=T)):(which(colnames(fit.mcmc)=="jres_ns_runs[12,8]", arr.ind=T))]
n1 <- fit.mcmc[,(which(colnames(fit.mcmc)=="n1[1]", arr.ind=T)):(which(colnames(fit.mcmc)=="n1[445]", arr.ind=T))]
n1_comp <- fit.mcmc[,(which(colnames(fit.mcmc)=="n1_comp[248]", arr.ind=T)):(which(colnames(fit.mcmc)=="n1_comp[445]", arr.ind=T))]
n1_nat <- fit.mcmc[,(which(colnames(fit.mcmc)=="n1_nat[248]", arr.ind=T)):(which(colnames(fit.mcmc)=="n1_nat[445]", arr.ind=T))]
beta <- fit.mcmc[,"beta_dj[2]"]
dj <- fit.mcmc[,(which(colnames(fit.mcmc)=="dj[1]", arr.ind=T)):(which(colnames(fit.mcmc)==(paste("dj[",I,"]",sep="")), arr.ind=T))]
dj_comp <- fit.mcmc[,(which(colnames(fit.mcmc)=="dj_comp[248]", arr.ind=T)):(which(colnames(fit.mcmc)=="dj_comp[515]", arr.ind=T))]
dj_nat <- fit.mcmc[,(which(colnames(fit.mcmc)=="dj_nat[248]", arr.ind=T)):(which(colnames(fit.mcmc)=="dj_nat[515]", arr.ind=T))] 
beta <- cbind(rep(1,n_iter),beta)


## EXTRAPOLATION. NO UPDATE FOR THIS PART
## -----------------------------------------------------------------------------
## SAMPLED SITES (SR, SR + CPUE methods)##
## In total 22 years until beginning of CPUE method of sampling only
###################

# Preparing matrix of results to fill-in
jLN_samp <- jLN_nat_samp <- jLN_comp_samp <- matrix(0,nrow=n_iter,ncol=22)

jHN_samp <- jHN_nat_samp <- jHN_comp_samp <- jHN_res_samp <- matrix(0,nrow=n_iter,ncol=22)

jLUR_samp <- jLUR_nat_samp <- jLUR_comp_samp <- jLUR_res_samp <- matrix(0,nrow=n_iter,ncol=22)

jVHN_samp <- jVHN_res_samp <- matrix(0,nrow=n_iter,ncol=22)

jLAP_samp <- jLAP_res_samp <- matrix(0,nrow=n_iter,ncol=22)


#Results
pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
setTxtProgressBar(pb,i)
  
  #######################
  ## Natural recruitment
  ## -------------------
  ## LN from 1985 to 1990
  for (y in 2:7) { 
    jLN_samp[i,y] <- sum(n1[i,START_i[y,3]:END_i[y,3]])
    #jLN_samp[i,y] <- sum(fit.mcmc[,paste("n1[",START_i[y,3],"]"):paste("n1[",END_i[y,3],"]")])     
    } ## End of loop over years

  # LN (zone 3)+HN (zone 4)+LUR (zone 5) from 1991 to 1995
  for (y in 8:12) {
    jLN_samp[i,y] <- sum(n1[i,START_i[y,3]:END_i[y,3]])
    jHN_samp[i,y] <- sum(n1[i,START_i[y,4]:END_i[y,4]])
    jLUR_samp[i,y] <- sum(n1[i,START_i[y,5]:END_i[y,5]])
    } ## End of loop over years
  
  # Year 1997 (y = 14)  
  jLN_samp[i,14] <- sum(n1[i,START_i[14,3]:END_i[14,3]])
  jHN_samp[i,14] <- sum(n1[i,START_i[14,4]:END_i[14,4]])
  jLUR_samp[i,14] <- sum(n1[i,START_i[14,5]:END_i[14,5]])
  
  #Year 2000 (y=17) 
  jLN_samp[i,17] <- sum(n1[i,START_i[17,3]:END_i[17,3]])
  jLUR_samp[i,17] <- sum(n1[i,START_i[17,5]:END_i[17,5]])

  # Year 2001 (y = 18)
  jLN_samp[i,18] <- sum(n1[i,START_i[18,3]:END_i[18,3]])
  jHN_samp[i,18] <- sum(n1[i,START_i[18,4]:END_i[18,4]])
  jLUR_samp[i,18] <- sum(n1[i,START_i[18,5]:END_i[18,5]])
  
  #Year 2002 (y=19)
  jLN_samp[i,19] <- sum(n1[i,START_i[19,3]:END_i[19,3]])
  
  # Year 2003 (y=20)
  jLN_samp[i,20] <- sum(n1[i,START_i[20,3]:END_i[20,3]])+sum(n1[i,START_i_bis[20,3]:END_i_bis[20,3]])
  jHN_samp[i,20] <- sum(n1[i,START_i[20,4]:END_i[20,4]])+sum(n1[i,START_i_bis[20,4]:END_i_bis[20,4]])
  jLUR_samp[i,20] <- sum(n1[i,START_i[20,5]:END_i[20,5]])+n1[i,434]
  
  # Year 2004 (y=21)
  jLN_samp[i,21] <- sum(n1[i,START_i[21,3]:END_i[21,3]])+sum(n1[i,START_i_bis[21,3]:END_i_bis[21,3]])
  
  # Year 2005 (y = 22)
  jLN_samp[i,22] <- sum(n1[i,START_i[22,3]:END_i[22,3]])+sum(n1[i,START_i_bis[22,3]:END_i_bis[22,3]])
  
  #####################################
  ## Natural recruitment + Compensation
  ## ----------------------------------
  
  # Year 1996 (y=13)
  jLN_nat_samp[i,13] <- sum(n1_nat[i,sites$Comp[START_i[13,3]]:sites$Comp[END_i[13,3]]])
  jLN_comp_samp[i,13] <- sum(n1_comp[i,sites$Comp[START_i[13,3]]:sites$Comp[END_i[13,3]]])
  jLN_samp[i,13] <- sum(n1[i,START_i[13,3]:END_i[13,3]])
  
  jHN_nat_samp[i,13] <- sum(n1_nat[i,sites$Comp[START_i[13,4]]:sites$Comp[END_i[13,4]]])
  jHN_comp_samp[i,13] <- sum(n1_comp[i,sites$Comp[START_i[13,4]]:sites$Comp[END_i[13,4]]])
  jHN_samp[i,13] <- sum(n1[i,START_i[13,4]:END_i[13,4]])
  
  jLUR_nat_samp[i,13] <- sum(n1_nat[i,sites$Comp[START_i[13,5]]:sites$Comp[END_i[13,5]]])
  jLUR_comp_samp[i,13] <- sum(n1_comp[i,sites$Comp[START_i[13,5]]:sites$Comp[END_i[13,5]]])
  jLUR_samp[i,13] <- sum(n1[i,START_i[13,5]:END_i[13,5]])
  
  # Year 1998 & 1999 (y=15 & y =16)
  for (y in 15:16) {
    jLN_nat_samp[i,y] <- sum(n1_nat[i,sites$Comp[START_i[y,3]]:sites$Comp[END_i[y,3]]])
    jLN_comp_samp[i,y] <- sum(n1_comp[i,sites$Comp[START_i[y,3]]:sites$Comp[END_i[y,3]]])
    jLN_samp[i,y] <- sum(n1[i,START_i[y,3]:END_i[y,3]])
    
    jHN_nat_samp[i,y] <- sum(n1_nat[i,sites$Comp[START_i[y,4]]:sites$Comp[END_i[y,4]]])
    jHN_comp_samp[i,y] <- sum(n1_comp[i,sites$Comp[START_i[y,4]]:sites$Comp[END_i[y,4]]])
    jHN_samp[i,y] <- sum(n1[i,START_i[y,4]:END_i[y,4]])
    
    jLUR_nat_samp[i,y] <- sum(n1_nat[i,sites$Comp[START_i[y,5]]:sites$Comp[END_i[y,5]]])
    jLUR_comp_samp[i,y] <- sum(n1_comp[i,sites$Comp[START_i[y,5]]:sites$Comp[END_i[y,5]]])
    jLUR_samp[i,y] <- sum(n1[i,START_i[y,5]:END_i[y,5]])
    } ## End of loop over years

  # Year 2000 ( y= 17)
  jHN_nat_samp[i,17] <- sum(n1_nat[i,sites$Comp[START_i[17,4]]:sites$Comp[END_i[17,4]]])
  jHN_comp_samp[i,17] <- sum(n1_comp[i,sites$Comp[START_i[17,4]]:sites$Comp[END_i[17,4]]])
  jHN_samp[i,17] <- sum(n1[i,START_i[17,4]:END_i[17,4]])
  
  # Year 2002 (y=19)
  jHN_nat_samp[i,19] <- sum(n1_nat[i,sites$Comp[START_i[19,4]]:sites$Comp[END_i[19,4]]])
  jHN_comp_samp[i,19] <- sum(n1_comp[i,sites$Comp[START_i[19,4]]:sites$Comp[END_i[19,4]]])
  jHN_samp[i,19] <- sum(n1[i,START_i[19,4]:END_i[19,4]])
  
  jLUR_nat_samp[i,19] <- sum(n1_nat[i,sites$Comp[START_i[19,5]]:sites$Comp[END_i[19,5]]])
  jLUR_comp_samp[i,19] <- sum(n1_comp[i,sites$Comp[START_i[19,5]]:sites$Comp[END_i[19,5]]])
  jLUR_samp[i,19] <- sum(n1[i,START_i[19,5]:END_i[19,5]])

  #Year 2004 (y=21)
  jHN_nat_samp[i,21] <- sum(n1_nat[i,sites$Comp[START_i[21,4]]:sites$Comp[END_i[21,4]]]) + sum(n1_nat[i,sites$Comp[START_i_bis[21,4]]:sites$Comp[END_i_bis[21,4]]])
  jHN_comp_samp[i,21] <- sum(n1_comp[i,sites$Comp[START_i[21,4]]:sites$Comp[END_i[21,4]]]) + sum(n1_comp[i,sites$Comp[START_i_bis[21,4]]:sites$Comp[END_i_bis[21,4]]])
  jHN_samp[i,21] <- sum(n1[i,START_i[21,4]:END_i[21,4]]) + sum(n1[i,START_i_bis[21,4]:END_i_bis[21,4]])
  
  jLUR_nat_samp[i,21] <- sum(n1_nat[i,sites$Comp[START_i[21,5]]:sites$Comp[END_i[21,5]]]) + sum(n1_nat[i,sites$Comp[START_i_bis[21,5]]:sites$Comp[END_i_bis[21,5]]])
  jLUR_comp_samp[i,21] <- sum(n1_comp[i,sites$Comp[START_i[21,5]]:sites$Comp[END_i[21,5]]]) + sum(n1_comp[i,sites$Comp[START_i_bis[21,5]]:sites$Comp[END_i_bis[21,5]]])
  jLUR_samp[i,21] <- sum(n1[i,START_i[21,5]:END_i[21,5]]) + sum(n1[i,START_i_bis[21,5]:END_i_bis[21,5]])

  # Year 2005 (y=22)
  jHN_nat_samp[i,22] <- sum(n1_nat[i,sites$Comp[START_i[22,4]]:sites$Comp[END_i[22,4]]]) + sum(n1_nat[i,sites$Comp[START_i_bis[22,4]]:sites$Comp[END_i_bis[22,4]]])
  jHN_comp_samp[i,22] <- sum(n1_comp[i,sites$Comp[START_i[22,4]]:sites$Comp[END_i[22,4]]]) + sum(n1_comp[i,sites$Comp[START_i_bis[22,4]]:sites$Comp[END_i_bis[22,4]]])
  jHN_samp[i,22] <- sum(n1[i,START_i[22,4]:END_i[22,4]]) + sum(n1[i,START_i_bis[22,4]:END_i_bis[22,4]])
  
  jLUR_nat_samp[i,22] <- sum(n1_nat[i,sites$Comp[START_i[22,5]]:sites$Comp[END_i[22,5]]]) + n1_nat[i,sites$Comp[445]]
  jLUR_comp_samp[i,22] <- sum(n1_comp[i,sites$Comp[START_i[22,5]]:sites$Comp[END_i[22,5]]]) + n1_comp[i,sites$Comp[445]]
  jLUR_samp[i,22] <- sum(n1[i,START_i[22,5]:END_i[22,5]]) + n1[i,445]

  ####################
  ## Restocking
  ## ----------
  ## HN + LUR from 1986 to 1990
  for (y in 3:7) {
    jHN_samp[i,y] <- sum(n1[i,START_i[y,4]:END_i[y,4]])
    jLUR_samp[i,y] <- sum(n1[i,START_i[y,5]:END_i[y,5]])
    } ## End of loop over years

  ## VHN, year 1991 (y=8), 1992 (y=9), 1994 (y=11)
  jVHN_samp[i,8] <- sum(n1[i,397:398]) ; jVHN_samp[i,9] <- sum(n1[i,399:400])
  jVHN_samp[i,11] <- sum(n1[i,401:402])

  ## LAP, year 1989 (y=6), year 1994 (y=11), year 1995 (y=12)
  jLAP_samp[i,6] <- sum(n1[i,403:414]) ; jLAP_samp[i,11] <- sum(n1[i,415:418])
  jLAP_samp[i,12] <- sum(n1[i,419:422])

} ## end of loop over iterations

########################
## NOT SAMPLED SITES ##
#######################

# Preparing matrix of results to fill-in
jLN_ns <- jLN_nat_ns <- jLN_comp_ns <- matrix(0,nrow=n_iter,ncol=Y_last)

jHN_ns <- jHN_nat_ns <- jHN_comp_ns <- jHN_res_ns <- matrix(0,nrow=n_iter,ncol=Y_last)

jLUR_ns <- jLUR_nat_ns <- jLUR_comp_ns <- jLUR_res_ns <- matrix(0,nrow=n_iter,ncol=Y_last)

jVHN_res_ns <- jLAP_res_ns <- jVHN_ns <- jLAP_ns <- matrix(0,nrow=n_iter,ncol=Y_last)

pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
  setTxtProgressBar(pb,i)

  ########################
  ## Natural recruitment /!\ NEEDS TO BE UPDATED EVERY YEAR
  ## ------------------
  
  # Site LN from 1985 to 1990
  jLN_ns[i,2] <- jLN_nat_ns[i,2] <- jnat_ns_riff[i,"jnat_ns_riff[2,3]"]+jnat_ns_runs[i,"jnat_ns_runs[2,3]"]
  jLN_ns[i,3] <- jLN_nat_ns[i,3] <- jnat_ns_riff[i,"jnat_ns_riff[3,3]"]+jnat_ns_runs[i,"jnat_ns_runs[3,3]"]
  jLN_ns[i,4] <- jLN_nat_ns[i,4] <- jnat_ns_riff[i,"jnat_ns_riff[4,3]"]+jnat_ns_runs[i,"jnat_ns_runs[4,3]"]
  jLN_ns[i,5] <- jLN_nat_ns[i,5] <- jnat_ns_riff[i,"jnat_ns_riff[5,3]"]+jnat_ns_runs[i,"jnat_ns_runs[5,3]"]
  jLN_ns[i,6] <- jLN_nat_ns[i,6] <- jnat_ns_riff[i,"jnat_ns_riff[6,3]"]+jnat_ns_runs[i,"jnat_ns_runs[6,3]"]
  jLN_ns[i,7] <- jLN_nat_ns[i,7] <- jnat_ns_riff[i,"jnat_ns_riff[7,3]"]+jnat_ns_runs[i,"jnat_ns_runs[7,3]"]
  
  # Year 1991
  jLN_ns[i,8] <- jLN_nat_ns[i,8] <- jnat_ns_riff[i,"jnat_ns_riff[8,3]"]+jnat_ns_runs[i,"jnat_ns_runs[8,3]"]
  jHN_ns[i,8] <- jHN_nat_ns[i,8] <- jnat_ns_riff[i,"jnat_ns_riff[8,4]"]+jnat_ns_runs[i,"jnat_ns_runs[8,4]"]  
  jLUR_ns[i,8] <- jLUR_nat_ns[i,8] <- jnat_ns_riff[i,"jnat_ns_riff[8,5]"]+jnat_ns_runs[i,"jnat_ns_runs[8,5]"]
  
  # Year 1992
  jLN_ns[i,9] <- jLN_nat_ns[i,9] <- jnat_ns_riff[i,"jnat_ns_riff[9,3]"]+jnat_ns_runs[i,"jnat_ns_runs[9,3]"]
  jHN_ns[i,9] <- jHN_nat_ns[i,9] <- jnat_ns_riff[i,"jnat_ns_riff[9,4]"]+jnat_ns_runs[i,"jnat_ns_runs[9,4]"]  
  jLUR_ns[i,9] <- jLUR_nat_ns[i,9] <- jnat_ns_riff[i,"jnat_ns_riff[9,5]"]+jnat_ns_runs[i,"jnat_ns_runs[9,5]"]  
  
  #Year 1993
  jLN_ns[i,10] <- jLN_nat_ns[i,10] <- jnat_ns_riff[i,"jnat_ns_riff[10,3]"]+jnat_ns_runs[i,"jnat_ns_runs[10,3]"]
  jHN_ns[i,10] <- jHN_nat_ns[i,10] <- jnat_ns_riff[i,"jnat_ns_riff[10,4]"]+jnat_ns_runs[i,"jnat_ns_runs[10,4]"]
  jLUR_ns[i,10] <- jLUR_nat_ns[i,10] <- jnat_ns_riff[i,"jnat_ns_riff[10,5]"]+jnat_ns_runs[i,"jnat_ns_runs[10,5]"]  
  
  #Year 1994
  jLN_ns[i,11] <- jLN_nat_ns[i,11] <- jnat_ns_riff[i,"jnat_ns_riff[11,3]"]+jnat_ns_runs[i,"jnat_ns_runs[11,3]"]  
  jHN_ns[i,11] <- jHN_nat_ns[i,11] <- jnat_ns_riff[i,"jnat_ns_riff[11,4]"]+jnat_ns_runs[i,"jnat_ns_runs[11,4]"]  
  jLUR_ns[i,11] <- jLUR_nat_ns[i,11] <- jnat_ns_riff[i,"jnat_ns_riff[11,5]"]+jnat_ns_runs[i,"jnat_ns_runs[11,5]"]
  
  #Year 1995
  jLN_ns[i,12] <- jLN_nat_ns[i,12] <- jnat_ns_riff[i,"jnat_ns_riff[12,3]"]+jnat_ns_runs[i,"jnat_ns_runs[12,3]"]
  jHN_ns[i,12] <- jHN_nat_ns[i,12] <- jnat_ns_riff[i,"jnat_ns_riff[12,4]"]+jnat_ns_runs[i,"jnat_ns_runs[12,4]"]
  jLUR_ns[i,12] <- jLUR_nat_ns[i,12] <- jnat_ns_riff[i,"jnat_ns_riff[12,5]"]+jnat_ns_runs[i,"jnat_ns_runs[12,5]"]

  #Year 1997
  jLN_ns[i,14] <- jLN_nat_ns[i,14] <- jnat_ns_riff[i,"jnat_ns_riff[14,3]"]+jnat_ns_runs[i,"jnat_ns_runs[14,3]"]
  jHN_ns[i,14] <- jHN_nat_ns[i,14] <- jnat_ns_riff[i,"jnat_ns_riff[14,4]"]+jnat_ns_runs[i,"jnat_ns_runs[14,4]"]  
  jLUR_ns[i,14] <- jLUR_nat_ns[i,14] <- jnat_ns_riff[i,"jnat_ns_riff[14,5]"]+jnat_ns_runs[i,"jnat_ns_runs[14,5]"]
  
  #Year 2000
  jLN_ns[i,17] <- jLN_nat_ns[i,17] <- jnat_ns_riff[i,"jnat_ns_riff[17,3]"]+jnat_ns_runs[i,"jnat_ns_runs[17,3]"]
  jLUR_ns[i,17] <- jLUR_nat_ns[i,17] <- jnat_ns_riff[i,"jnat_ns_riff[17,5]"]+jnat_ns_runs[i,"jnat_ns_runs[17,5]"]  
  
  #Year 2001  
  jLN_ns[i,18] <- jLN_nat_ns[i,18] <- jnat_ns_riff[i,"jnat_ns_riff[18,3]"]+jnat_ns_runs[i,"jnat_ns_runs[18,3]"]
  jHN_ns[i,18] <- jHN_nat_ns[i,18] <- jnat_ns_riff[i,"jnat_ns_riff[18,4]"]+jnat_ns_runs[i,"jnat_ns_runs[18,4]"]  
  jLUR_ns[i,18] <- jLUR_nat_ns[i,18] <- jnat_ns_riff[i,"jnat_ns_riff[18,5]"]+jnat_ns_runs[i,"jnat_ns_runs[18,5]"] 
  
  #Year 2002
  jLN_ns[i,19] <- jLN_nat_ns[i,19] <- jnat_ns_riff[i,"jnat_ns_riff[19,3]"]+jnat_ns_runs[i,"jnat_ns_runs[19,3]"]   
  
  #Year 2003
  jLN_ns[i,20] <- jLN_nat_ns[i,20] <- jnat_ns_riff[i,"jnat_ns_riff[20,3]"]+jnat_ns_runs[i,"jnat_ns_runs[20,3]"]
  jHN_ns[i,20] <- jHN_nat_ns[i,20] <- jnat_ns_riff[i,"jnat_ns_riff[20,4]"]+jnat_ns_runs[i,"jnat_ns_runs[20,4]"]  
  jLUR_ns[i,20] <- jLUR_nat_ns[i,20] <- jnat_ns_riff[i,"jnat_ns_riff[20,5]"]+jnat_ns_runs[i,"jnat_ns_runs[20,5]"]  
  
  #Year 2004
  jLN_ns[i,21] <- jLN_nat_ns[i,21] <- jnat_ns_riff[i,"jnat_ns_riff[21,3]"]+jnat_ns_runs[i,"jnat_ns_runs[21,3]"]  
  
  #Year 2005
  jLN_ns[i,22] <- jLN_nat_ns[i,22] <- jnat_ns_riff[i,"jnat_ns_riff[22,3]"]+jnat_ns_runs[i,"jnat_ns_runs[22,3]"]
  
  #Year 2006  
  jLN_ns[i,23] <- jLN_nat_ns[i,23] <- jnat_ns_riff[i,"jnat_ns_riff[23,3]"]+jnat_ns_runs[i,"jnat_ns_runs[23,3]"]  
  jLUR_ns[i,23] <- jLUR_nat_ns[i,23] <- jnat_ns_riff[i,"jnat_ns_riff[23,5]"]+jnat_ns_runs[i,"jnat_ns_runs[23,5]"]
  
  #Year 2009
  jHN_ns[i,26] <- jHN_nat_ns[i,26] <- jnat_ns_riff[i,"jnat_ns_riff[26,4]"]+jnat_ns_runs[i,"jnat_ns_runs[26,4]"]
  jLUR_ns[i,26] <- jLUR_nat_ns[i,26] <- jnat_ns_riff[i,"jnat_ns_riff[26,5]"]+jnat_ns_runs[i,"jnat_ns_runs[26,5]"]  
  
  # #Year 2010
  # jLN_ns[i,27] <- jLN_nat_ns[i,27] <- jnat_ns_riff[i,"jnat_ns_riff[27,3]"]+jnat_ns_runs[i,"jnat_ns_runs[27,3]"]
  # jHN_ns[i,27] <- jHN_nat_ns[i,27] <- jnat_ns_riff[i,"jnat_ns_riff[27,4]"]+jnat_ns_runs[i,"jnat_ns_runs[27,4]"]
  # jLUR_ns[i,27] <- jLUR_nat_ns[i,27] <- jnat_ns_riff[i,"jnat_ns_riff[27,5]"]+jnat_ns_runs[i,"jnat_ns_runs[27,5]"]
  # 
  # #Year 2011
  # jLN_ns[i,28] <- jLN_nat_ns[i,28] <- jnat_ns_riff[i,"jnat_ns_riff[28,3]"]+jnat_ns_runs[i,"jnat_ns_runs[28,3]"]
  # jHN_ns[i,28] <- jHN_nat_ns[i,28] <- jnat_ns_riff[i,"jnat_ns_riff[28,4]"]+jnat_ns_runs[i,"jnat_ns_runs[28,4]"]
  # jLUR_ns[i,28] <- jLUR_nat_ns[i,28] <- jnat_ns_riff[i,"jnat_ns_riff[28,5]"]+jnat_ns_runs[i,"jnat_ns_runs[28,5]"]
  # 
  # #Year 2012
  # jLN_ns[i,29] <- jLN_nat_ns[i,29] <- jnat_ns_riff[i,"jnat_ns_riff[29,3]"]+jnat_ns_runs[i,"jnat_ns_runs[29,3]"]
  # jHN_ns[i,29] <- jHN_nat_ns[i,29] <- jnat_ns_riff[i,"jnat_ns_riff[29,4]"]+jnat_ns_runs[i,"jnat_ns_runs[29,4]"]
  # jLUR_ns[i,29] <- jLUR_nat_ns[i,29] <- jnat_ns_riff[i,"jnat_ns_riff[29,5]"]+jnat_ns_runs[i,"jnat_ns_runs[29,5]"]  
  # 
  # #Year 2013
  # jLN_ns[i,30] <- jLN_nat_ns[i,30] <- jnat_ns_riff[i,"jnat_ns_riff[30,3]"]+jnat_ns_runs[i,"jnat_ns_runs[30,3]"]
  # jHN_ns[i,30] <- jHN_nat_ns[i,30] <- jnat_ns_riff[i,"jnat_ns_riff[30,4]"]+jnat_ns_runs[i,"jnat_ns_runs[30,4]"]
  # jLUR_ns[i,30] <- jLUR_nat_ns[i,30] <- jnat_ns_riff[i,"jnat_ns_riff[30,5]"]+jnat_ns_runs[i,"jnat_ns_runs[30,5]"] 
  # 
  # #Year 2014
  # jLN_ns[i,31] <- jLN_nat_ns[i,31] <- jnat_ns_riff[i,"jnat_ns_riff[31,3]"]+jnat_ns_runs[i,"jnat_ns_runs[31,3]"]
  # jHN_ns[i,31] <- jHN_nat_ns[i,31] <- jnat_ns_riff[i,"jnat_ns_riff[31,4]"]+jnat_ns_runs[i,"jnat_ns_runs[31,4]"]
  # jLUR_ns[i,31] <- jLUR_nat_ns[i,31] <- jnat_ns_riff[i,"jnat_ns_riff[31,5]"]+jnat_ns_runs[i,"jnat_ns_runs[31,5]"]  
  
  # From 2010 to now on  
  for (y in 27:Y_last) {
    jLN_ns[i,y] <- jLN_nat_ns[i,y] <- jnat_ns_riff[i,paste("jnat_ns_riff[",y,",3]",sep="")]+jnat_ns_runs[i,paste("jnat_ns_runs[",y,",3]",sep="")]
    jHN_ns[i,y] <- jHN_nat_ns[i,y] <- jnat_ns_riff[i,paste("jnat_ns_riff[",y,",4]",sep="")]+jnat_ns_runs[i,paste("jnat_ns_runs[",y,",4]",sep="")]
    jLUR_ns[i,y] <- jLUR_nat_ns[i,y] <- jnat_ns_riff[i,paste("jnat_ns_riff[",y,",5]",sep="")]+jnat_ns_runs[i,paste("jnat_ns_runs[",y,",5]",sep="")] 
  } ## End of loop over years
  
  # MISE A JOUR DERNIERE ANNEE. Remplacer le chiffre 31 (correspondant ? l'ann?e 2014) par la valeur de l'index Y_last

  ##################################################   
  ## Natural recruitment + Compensation. NO UPDATE
  ## ----------------------------------------------
  
  #Year 1996
  jLN_nat_ns[i,13] <- jnat_ns_riff[i,"jnat_ns_riff[13,3]"]+jnat_ns_runs[i,"jnat_ns_runs[13,3]"]
  jLN_comp_ns[i,13] <- jcomp_ns_riff[i,"jcomp_ns_riff[13,3]"]+jcomp_ns_runs[i,"jcomp_ns_runs[13,3]"]
  jLN_ns[i,13] <- jLN_nat_ns[i,13]+jLN_comp_ns[i,13]
  
  jHN_nat_ns[i,13] <- jnat_ns_riff[i,"jnat_ns_riff[13,4]"]+jnat_ns_runs[i,"jnat_ns_runs[13,4]"]
  jHN_comp_ns[i,13] <- jcomp_ns_riff[i,"jcomp_ns_riff[13,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[13,4]"]
  jHN_ns[i,13] <- jHN_nat_ns[i,13]+jHN_comp_ns[i,13]
  
  jLUR_nat_ns[i,13] <- jnat_ns_riff[i,"jnat_ns_riff[13,5]"]+jnat_ns_runs[i,"jnat_ns_runs[13,5]"]
  jLUR_comp_ns[i,13] <- jcomp_ns_riff[i,"jcomp_ns_riff[13,5]"]+jcomp_ns_runs[i,"jcomp_ns_runs[13,5]"]
  jLUR_ns[i,13] <- jLUR_nat_ns[i,13]+jLUR_comp_ns[i,13]
  
  #Year 1998
  jLN_nat_ns[i,15] <- jnat_ns_riff[i,"jnat_ns_riff[15,3]"]+jnat_ns_runs[i,"jnat_ns_runs[15,3]"]
  jLN_comp_ns[i,15] <- jcomp_ns_riff[i,"jcomp_ns_riff[15,3]"]+jcomp_ns_runs[i,"jcomp_ns_runs[15,3]"]
  jLN_ns[i,15] <- jLN_nat_ns[i,15]+jLN_comp_ns[i,15]
  
  jHN_nat_ns[i,15] <- jnat_ns_riff[i,"jnat_ns_riff[15,4]"]+jnat_ns_runs[i,"jnat_ns_runs[15,4]"]
  jHN_comp_ns[i,15] <- jcomp_ns_riff[i,"jcomp_ns_riff[15,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[15,4]"]
  jHN_ns[i,15] <- jHN_nat_ns[i,15]+jHN_comp_ns[i,15]
  
  jLUR_nat_ns[i,15] <- jnat_ns_riff[i,"jnat_ns_riff[15,5]"]+jnat_ns_runs[i,"jnat_ns_runs[15,5]"]
  jLUR_comp_ns[i,15] <- jcomp_ns_riff[i,"jcomp_ns_riff[15,5]"]+jcomp_ns_runs[i,"jcomp_ns_runs[15,5]"]
  jLUR_ns[i,15] <- jLUR_nat_ns[i,15]+jLUR_comp_ns[i,15]

  #Year 1999
  jLN_nat_ns[i,16] <- jnat_ns_riff[i,"jnat_ns_riff[16,3]"]+jnat_ns_runs[i,"jnat_ns_runs[16,3]"]
  jLN_comp_ns[i,16] <- jcomp_ns_riff[i,"jcomp_ns_riff[16,3]"]+jcomp_ns_runs[i,"jcomp_ns_runs[16,3]"]
  jLN_ns[i,16] <- jLN_nat_ns[i,16]+jLN_comp_ns[i,16]
  
  jHN_nat_ns[i,16] <- jnat_ns_riff[i,"jnat_ns_riff[16,4]"]+jnat_ns_runs[i,"jnat_ns_runs[16,4]"]
  jHN_comp_ns[i,16] <- jcomp_ns_riff[i,"jcomp_ns_riff[16,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[16,4]"]
  jHN_ns[i,16] <- jHN_nat_ns[i,16]+jHN_comp_ns[i,16]
  
  jLUR_nat_ns[i,16] <- jnat_ns_riff[i,"jnat_ns_riff[16,5]"]+jnat_ns_runs[i,"jnat_ns_runs[16,5]"]
  jLUR_comp_ns[i,16] <- jcomp_ns_riff[i,"jcomp_ns_riff[16,5]"]+jcomp_ns_runs[i,"jcomp_ns_runs[16,5]"]
  jLUR_ns[i,16] <- jLUR_nat_ns[i,16]+jLUR_comp_ns[i,16]
  
  #Year 2000
  jHN_nat_ns[i,17] <- jnat_ns_riff[i,"jnat_ns_riff[17,4]"]+jnat_ns_runs[i,"jnat_ns_runs[17,4]"]
  jHN_comp_ns[i,17] <- jcomp_ns_riff[i,"jcomp_ns_riff[17,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[17,4]"]
  jHN_ns[i,17] <- jHN_nat_ns[i,17]+jHN_comp_ns[i,17]
  
  #Year 2002
  jHN_nat_ns[i,19] <- jnat_ns_riff[i,"jnat_ns_riff[19,4]"]+jnat_ns_runs[i,"jnat_ns_runs[19,4]"]
  jHN_comp_ns[i,19] <- jcomp_ns_riff[i,"jcomp_ns_riff[19,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[19,4]"]
  jHN_ns[i,19] <- jHN_nat_ns[i,19]+jHN_comp_ns[i,19]
  
  jLUR_nat_ns[i,19] <- jnat_ns_riff[i,"jnat_ns_riff[19,5]"]+jnat_ns_runs[i,"jnat_ns_runs[19,5]"]
  jLUR_comp_ns[i,19] <- jcomp_ns_riff[i,"jcomp_ns_riff[19,5]"]+jcomp_ns_runs[i,"jcomp_ns_runs[19,5]"]
  jLUR_ns[i,19] <- jLUR_nat_ns[i,19]+jLUR_comp_ns[i,19]
  
  #Year 2004
  jHN_nat_ns[i,21] <- jnat_ns_riff[i,"jnat_ns_riff[21,4]"]+jnat_ns_runs[i,"jnat_ns_runs[21,4]"]
  jHN_comp_ns[i,21] <- jcomp_ns_riff[i,"jcomp_ns_riff[21,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[21,4]"]
  jHN_ns[i,21] <- jHN_nat_ns[i,21]+jHN_comp_ns[i,21]
  
  jLUR_nat_ns[i,21] <- jnat_ns_riff[i,"jnat_ns_riff[21,5]"]+jnat_ns_runs[i,"jnat_ns_runs[21,5]"]
  jLUR_comp_ns[i,21] <- jcomp_ns_riff[i,"jcomp_ns_riff[21,5]"]+jcomp_ns_runs[i,"jcomp_ns_runs[21,5]"]
  jLUR_ns[i,21] <- jLUR_nat_ns[i,21]+jLUR_comp_ns[i,21]

  #Year 2005
  jHN_nat_ns[i,22] <- jnat_ns_riff[i,"jnat_ns_riff[22,4]"]+jnat_ns_runs[i,"jnat_ns_runs[22,4]"]
  jHN_comp_ns[i,22] <- jcomp_ns_riff[i,"jcomp_ns_riff[22,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[22,4]"]
  jHN_ns[i,22] <- jHN_nat_ns[i,22]+jHN_comp_ns[i,22]
  
  jLUR_nat_ns[i,22] <- jnat_ns_riff[i,"jnat_ns_riff[22,5]"]+jnat_ns_runs[i,"jnat_ns_runs[22,5]"]
  jLUR_comp_ns[i,22] <- jcomp_ns_riff[i,"jcomp_ns_riff[22,5]"]+jcomp_ns_runs[i,"jcomp_ns_runs[22,5]"]
  jLUR_ns[i,22] <- jLUR_nat_ns[i,22]+jLUR_comp_ns[i,22]
  
  #Year 2006
  jHN_nat_ns[i,23] <- jnat_ns_riff[i,"jnat_ns_riff[23,4]"]+jnat_ns_runs[i,"jnat_ns_runs[23,4]"]
  jHN_comp_ns[i,23] <- jcomp_ns_riff[i,"jcomp_ns_riff[23,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[23,4]"]
  jHN_ns[i,23] <- jHN_nat_ns[i,23]+jHN_comp_ns[i,23]
  
  #Year 2007
  jLN_nat_ns[i,24] <- jnat_ns_riff[i,"jnat_ns_riff[24,3]"]+jnat_ns_runs[i,"jnat_ns_runs[24,3]"]
  jLN_comp_ns[i,24] <- jcomp_ns_riff[i,"jcomp_ns_riff[24,3]"]+jcomp_ns_runs[i,"jcomp_ns_runs[24,3]"]
  jLN_ns[i,24] <- jLN_nat_ns[i,24]+jLN_comp_ns[i,24]
  
  jHN_nat_ns[i,24] <- jnat_ns_riff[i,"jnat_ns_riff[24,4]"]+jnat_ns_runs[i,"jnat_ns_runs[24,4]"]
  jHN_comp_ns[i,24] <- jcomp_ns_riff[i,"jcomp_ns_riff[24,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[24,4]"]
  jHN_ns[i,24] <- jHN_nat_ns[i,24]+jHN_comp_ns[i,24]
  
  jLUR_nat_ns[i,24] <- jnat_ns_riff[i,"jnat_ns_riff[24,5]"]+jnat_ns_runs[i,"jnat_ns_runs[24,5]"]
  jLUR_comp_ns[i,24] <- jcomp_ns_riff[i,"jcomp_ns_riff[24,5]"]+jcomp_ns_runs[i,"jcomp_ns_runs[24,5]"]
  jLUR_ns[i,24] <- jLUR_nat_ns[i,24]+jLUR_comp_ns[i,24]
  
  # Year 2008
  jLN_nat_ns[i,25] <- jnat_ns_riff[i,"jnat_ns_riff[25,3]"]+jnat_ns_runs[i,"jnat_ns_runs[25,3]"]
  jLN_comp_ns[i,25] <- jcomp_ns_riff[i,"jcomp_ns_riff[25,3]"]+jcomp_ns_runs[i,"jcomp_ns_runs[25,3]"]
  jLN_ns[i,25] <- jLN_nat_ns[i,25]+jLN_comp_ns[i,25]
  
  jHN_nat_ns[i,25] <- jnat_ns_riff[i,"jnat_ns_riff[25,4]"]+jnat_ns_runs[i,"jnat_ns_runs[25,4]"]
  jHN_comp_ns[i,25] <- jcomp_ns_riff[i,"jcomp_ns_riff[25,4]"]+jcomp_ns_runs[i,"jcomp_ns_runs[25,4]"]
  jHN_ns[i,25] <- jHN_nat_ns[i,25]+jHN_comp_ns[i,25]
  
  jLUR_nat_ns[i,25] <- jnat_ns_riff[i,"jnat_ns_riff[25,5]"]+jnat_ns_runs[i,"jnat_ns_runs[25,5]"]
  jLUR_comp_ns[i,25] <- jcomp_ns_riff[i,"jcomp_ns_riff[25,5]"]+jcomp_ns_runs[i,"jcomp_ns_runs[25,5]"]
  jLUR_ns[i,25] <- jLUR_nat_ns[i,25]+jLUR_comp_ns[i,25]
  
  # Year 2009
  jLN_nat_ns[i,26] <- jnat_ns_riff[i,"jnat_ns_riff[26,3]"]+jnat_ns_runs[i,"jnat_ns_runs[26,3]"]
  jLN_comp_ns[i,26] <- jcomp_ns_riff[i,"jcomp_ns_riff[26,3]"]+jcomp_ns_runs[i,"jcomp_ns_runs[26,3]"]
  jLN_ns[i,26] <- jLN_nat_ns[i,26]+jLN_comp_ns[i,26]
  
  ##############
  ## Restocking. NO UPDATE FOR THIS PART
  ## ----------
  
  #Year 1986
  jHN_ns[i,3] <- jHN_res_ns[i,3] <- jres_ns_riff[i,"jres_ns_riff[3,4]"]+jres_ns_runs[i,"jres_ns_runs[3,4]"]
  jLUR_ns[i,3] <- jLUR_res_ns[i,3] <- jres_ns_riff[i,"jres_ns_riff[3,5]"]+jres_ns_runs[i,"jres_ns_runs[3,5]"]
  
  # YEar 1987
  jHN_ns[i,4] <- jHN_res_ns[i,4] <- jres_ns_riff[i,"jres_ns_riff[4,4]"]+jres_ns_runs[i,"jres_ns_runs[4,4]"]
  jLUR_ns[i,4] <- jLUR_res_ns[i,4] <- jres_ns_riff[i,"jres_ns_riff[4,5]"]+jres_ns_runs[i,"jres_ns_runs[4,5]"]
  
  # YEar 1988
  jHN_ns[i,5] <- jHN_res_ns[i,5] <- jres_ns_riff[i,"jres_ns_riff[5,4]"]+jres_ns_runs[i,"jres_ns_runs[5,4]"]
  jLUR_ns[i,5] <- jLUR_res_ns[i,5] <- jres_ns_riff[i,"jres_ns_riff[5,5]"]+jres_ns_runs[i,"jres_ns_runs[5,5]"]
  
  # Year 1989
  jHN_ns[i,6] <- jHN_res_ns[i,6] <- jres_ns_riff[i,"jres_ns_riff[6,4]"]+jres_ns_runs[i,"jres_ns_runs[6,4]"]
  jLUR_ns[i,6] <- jLUR_res_ns[i,6] <- jres_ns_riff[i,"jres_ns_riff[6,5]"]+jres_ns_runs[i,"jres_ns_runs[6,5]"]
  jVHN_ns[i,6] <- jVHN_res_ns[i,6] <- jres_ns_riff[i,"jres_ns_riff[6,7]"]+jres_ns_runs[i,"jres_ns_runs[6,7]"]  
  jLAP_ns[i,6] <- jLAP_res_ns[i,6] <- jres_ns_riff[i,"jres_ns_riff[6,8]"]+jres_ns_runs[i,"jres_ns_runs[6,8]"]
  
  # Year 1990  
  jHN_ns[i,7] <- jHN_res_ns[i,7] <- jres_ns_riff[i,"jres_ns_riff[7,4]"]+jres_ns_runs[i,"jres_ns_runs[7,4]"]
  jLUR_ns[i,7] <- jLUR_res_ns[i,7] <- jres_ns_riff[i,"jres_ns_riff[7,5]"]+jres_ns_runs[i,"jres_ns_runs[7,5]"]
  jVHN_ns[i,7] <- jVHN_res_ns[i,7] <- jres_ns_riff[i,"jres_ns_riff[7,7]"]+jres_ns_runs[i,"jres_ns_runs[7,7]"]
  
  # Year 1991
  jVHN_ns[i,8] <- jVHN_res_ns[i,8] <- jres_ns_riff[i,"jres_ns_riff[8,7]"]+jres_ns_runs[i,"jres_ns_runs[8,7]"]
  
  # Year 1992
  jVHN_ns[i,9] <- jVHN_res_ns[i,9] <- jres_ns_riff[i,"jres_ns_riff[9,7]"]+jres_ns_runs[i,"jres_ns_runs[9,7]"]
  
  # Year 1994
  jVHN_ns[i,11] <- jVHN_res_ns[i,11] <- jres_ns_riff[i,"jres_ns_riff[11,7]"]+jres_ns_runs[i,"jres_ns_runs[11,7]"]
  jLAP_ns[i,11] <- jLAP_res_ns[i,11] <- jres_ns_riff[i,"jres_ns_riff[11,8]"]+jres_ns_runs[i,"jres_ns_runs[11,8]"]
  
  #Year 1995
  jLAP_ns[i,12] <- jLAP_res_ns[i,12] <- jres_ns_riff[i,"jres_ns_riff[12,8]"]+jres_ns_runs[i,"jres_ns_runs[12,8]"]

} ## End of loop over iterations



###############################
############################
## TOTAL PER ZONE X YEAR ##
###########################
#################################

# Preparing matrix of results to fill-in
jLN <- jLN_nat <- jLN_comp <- matrix(0,nrow=n_iter,ncol=Y_last)

jHN <- jHN_nat <- jHN_comp <- jHN_res <- matrix(0,nrow=n_iter,ncol=Y_last)

jLUR <- jLUR_nat <- jLUR_comp <- jLUR_res <- matrix(0,nrow=n_iter,ncol=Y_last)

jVHN <- jVHN_res <- matrix(0,nrow=n_iter,ncol=Y_last)

jLAP <- jLAP_res <- matrix(0,nrow=n_iter,ncol=Y_last)
 
pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
setTxtProgressBar(pb,i)

    #Year 1985
    jLN[i,2] <- jLN_nat[i,2] <- jLN_samp[i,2] + jLN_nat_ns[i,2]

    # From 1986 to 1990
    for (y in 3:7) {
        jLN[i,y] <- jLN_nat[i,y] <- jLN_samp[i,y] + jLN_nat_ns[i,y]
        jHN[i,y] <- jHN_res[i,y] <- jHN_samp[i,y ]+ jHN_res_ns[i,y]
        jLUR[i,y] <- jLUR_res[i,y] <- jLUR_samp[i,y] + jLUR_res_ns[i,y]
        } ## End of loop over years

    # Year 1989, 1990
    for (y in 6:7) {
        jVHN[i,y] <- jVHN_res[i,y] <- jVHN_res_ns[i,y]
        } ## End of loop over years
        
    # Year 1989
    jLAP[i,6] <- jLAP_res[i,6] <- jLAP_samp[i,6]+jLAP_res_ns[i,6]

    #Year 1991 to 1995
    for (y in 8:12) {
        jLN[i,y] <- jLN_nat[i,y] <- jLN_samp[i,y]+jLN_nat_ns[i,y]
        jHN[i,y] <- jHN_nat[i,y] <- jHN_samp[i,y]+jHN_nat_ns[i,y]
        jLUR[i,y] <- jLUR_nat[i,y] <- jLUR_samp[i,y]+jLUR_nat_ns[i,y]
        } ## End of loop over years

    # Year 1991, 1992
    for (y in 8:9) {
        jVHN[i,y] <- jVHN_res[i,y] <- jVHN_samp[i,y]+jVHN_res_ns[i,y]
        } ## End of loop over years
        
    # Year 1993. Number of tacons 0+ released in VHN
    jVHN[i,10] <- jVHN_res[i,10] <- 779
    
    # Year 1994.
    jVHN[i,11] <- jVHN_res[i,11] <- jVHN_samp[i,11]+jVHN_res_ns[i,11]

    # Year 1994, 1995
    for (y in 11:12) {
        jLAP[i,y] <- jLAP_res[i,y] <- jLAP_samp[i,y]+jLAP_res_ns[i,y]
        } ## End of loop over years

    # Year 1996
    jLN[i,13] <- jLN_samp[i,13]+jLN_ns[i,13]
    jLN_nat[i,13] <- jLN_nat_samp[i,13]+jLN_nat_ns[i,13]
    jLN_comp[i,13] <- jLN_comp_samp[i,13]+jLN_comp_ns[i,13]
    
    jHN[i,13] <- jHN_samp[i,13]+jHN_ns[i,13]
    jHN_nat[i,13] <- jHN_nat_samp[i,13]+jHN_nat_ns[i,13]
    jHN_comp[i,13] <- jHN_comp_samp[i,13]+jHN_comp_ns[i,13]
    
    jLUR[i,13] <- jLUR_samp[i,13]+jLUR_ns[i,13]
    jLUR_nat[i,13] <- jLUR_nat_samp[i,13]+jLUR_nat_ns[i,13]
    jLUR_comp[i,13] <- jLUR_comp_samp[i,13]+jLUR_comp_ns[i,13]
    
    # Year 1997
    jLN[i,14] <- jLN_nat[i,14] <- jLN_samp[i,14]+jLN_nat_ns[i,14]    
    jHN[i,14] <- jHN_nat[i,14] <- jHN_samp[i,14]+jHN_nat_ns[i,14]    
    jLUR[i,14] <- jLUR_nat[i,14] <- jLUR_samp[i,14]+jLUR_nat_ns[i,14]    
    
    # Year 1998, 1999
    for (y in 15:16) {
        jLN[i,y] <- jLN_samp[i,y]+jLN_ns[i,y]
        jLN_nat[i,y] <- jLN_nat_samp[i,y]+jLN_nat_ns[i,y]
        jLN_comp[i,y] <- jLN_comp_samp[i,y]+jLN_comp_ns[i,y]
    
        jHN[i,y] <- jHN_samp[i,y]+jHN_ns[i,y]
        jHN_nat[i,y] <- jHN_nat_samp[i,y]+jHN_nat_ns[i,y]
        jHN_comp[i,y] <- jHN_comp_samp[i,y]+jHN_comp_ns[i,y]
    
        jLUR[i,y] <- jLUR_samp[i,y]+jLUR_ns[i,y]
        jLUR_nat[i,y] <- jLUR_nat_samp[i,y]+jLUR_nat_ns[i,y]
        jLUR_comp[i,y] <- jLUR_comp_samp[i,y]+jLUR_comp_ns[i,y]
        } ## End of loop over years
        
    # Year 2000    
    jLN[i,17] <- jLN_nat[i,17] <- jLN_samp[i,17]+jLN_nat_ns[i,17]
                 
    jHN[i,17] <- jHN_samp[i,17]+jHN_ns[i,17]                                  
    jHN_nat[i,17] <- jHN_nat_samp[i,17]+jHN_nat_ns[i,17]                      
    jHN_comp[i,17] <- jHN_comp_samp[i,17]+jHN_comp_ns[i,17] 
                      
    jLUR[i,17] <- jLUR_nat[i,17] <- jLUR_samp[i,17]+jLUR_nat_ns[i,17]         

    # Year 2001
    jLN[i,18] <- jLN_nat[i,18] <- jLN_samp[i,18]+jLN_nat_ns[i,18]
    
    jHN[i,18] <- jHN_nat[i,18] <- jHN_samp[i,18]+jHN_nat_ns[i,18] 
       
    jLUR[i,18] <- jLUR_nat[i,18] <- jLUR_samp[i,18]+jLUR_nat_ns[i,18]
    
    #Year 2002
    jLN[i,19] <- jLN_nat[i,19] <- jLN_samp[i,19]+jLN_nat_ns[i,19]
      
    jHN[i,19] <- jHN_samp[i,19]+jHN_ns[i,19]
    jHN_nat[i,19] <- jHN_nat_samp[i,19]+jHN_nat_ns[i,19]
    jHN_comp[i,19] <- jHN_comp_samp[i,19]+jHN_comp_ns[i,19]
    
    jLUR[i,19] <- jLUR_samp[i,19]+jLUR_ns[i,19]
    jLUR_nat[i,19] <- jLUR_nat_samp[i,19]+jLUR_nat_ns[i,19]
    jLUR_comp[i,19] <- jLUR_comp_samp[i,19]+jLUR_comp_ns[i,19]      
    
    # Year 2003
    jLN[i,20] <- jLN_nat[i,20] <- jLN_samp[i,20]+jLN_nat_ns[i,20]

    jHN[i,20] <- jHN_nat[i,20] <- jHN_samp[i,20]+jHN_nat_ns[i,20]

    jLUR[i,20] <- jLUR_nat[i,20] <- jLUR_samp[i,20]+jLUR_nat_ns[i,20]    
    
    # Year 2004, 2005
    for (y in 21:22) {
        jLN[i,y] <- jLN_nat[i,y] <- jLN_samp[i,y]+jLN_nat_ns[i,y]
        
        jHN[i,y] <- jHN_samp[i,y]+jHN_ns[i,y]
        jHN_nat[i,y] <- jHN_nat_samp[i,y]+jHN_nat_ns[i,y]
        jHN_comp[i,y] <- jHN_comp_samp[i,y]+jHN_comp_ns[i,y]
        
        jLUR[i,y] <- jLUR_samp[i,y]+jLUR_ns[i,y]
        jLUR_nat[i,y] <- jLUR_nat_samp[i,y]+jLUR_nat_ns[i,y]
        jLUR_comp[i,y] <- jLUR_comp_samp[i,y]+jLUR_comp_ns[i,y]
        } ## End of loop over years
    
    # Year 2006
    jLN[i,23] <- jLN_nat[i,23] <- jLN_nat_ns[i,23] 
    
    jHN[i,23] <- jHN_ns[i,23]                                                   
    jHN_nat[i,23] <- jHN_nat_ns[i,23]
    jHN_comp[i,23] <- jHN_comp_ns[i,23]         
    
    jLUR[i,23] <- jLUR_nat[i,23] <- jLUR_nat_ns[i,23]                           
      
    # Year 2007, 2008  
    for (y in 24:25) {
        jLN[i,y] <- jLN_ns[i,y]
        jLN_nat[i,y] <- jLN_nat_ns[i,y] 
        jLN_comp[i,y] <- jLN_comp_ns[i,y]
        
        jHN[i,y] <- jHN_ns[i,y]
        jHN_nat[i,y] <- jHN_nat_ns[i,y]
        jHN_comp[i,y] <- jHN_comp_ns[i,y]
        
        jLUR[i,y] <- jLUR_ns[i,y]
        jLUR_nat[i,y] <- jLUR_nat_ns[i,y]
        jLUR_comp[i,y] <- jLUR_comp_ns[i,y]
        } ## End of loop over years
        
    #Year 2009
    jLN[i,26] <- jLN_ns[i,26]                                                     
    jLN_nat[i,26] <- jLN_nat_ns[i,26] 
    jLN_comp[i,26] <- jLN_comp_ns[i,26]    
    
    jHN[i,26] <- jHN_nat[i,26] <- jHN_nat_ns[i,26]                                
    
    jLUR[i,26] <- jLUR_nat[i,26] <- jLUR_nat_ns[i,26]                                                
       
    # From 2010 to now on  
    for (y in 27:Y_last) {
        jLN[i,y] <- jLN_nat[i,y] <- jLN_nat_ns[i,y]
        
        jHN[i,y] <- jHN_nat[i,y] <- jHN_nat_ns[i,y]
        
        jLUR[i,y] <- jLUR_nat[i,y] <- jLUR_nat_ns[i,y]
        } ## End of loop over years
    }# end of loop over iterations


#######################
###################
## TOTAL PER YEAR ##
####################
########################
probs <- c(0.025,0.05,0.25,0.5,0.75,0.95,0.975) # quantiles

# Preparing matrix of results to fill-in
YOY_tot <- YOYnat <- YOYcomp <- YOYres <- matrix(0,nrow=n_iter,ncol=Y_last)

pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
setTxtProgressBar(pb,i)
    for (y in 2:Y_last) {
        YOY_tot[i,y] <- jLN[i,y]+jHN[i,y]+jLUR[i,y]+jVHN[i,y]+jLAP[i,y]
        YOYnat[i,y] <- jLN_nat[i,y]+jHN_nat[i,y]+jLUR_nat[i,y]
        YOYcomp[i,y] <- jLN_comp[i,y]+jHN_comp[i,y]+jLUR_comp[i,y]
        YOYres[i,y] <- jHN_res[i,y]+jLUR_res[i,y]+jVHN_res[i,y]+jLAP_res[i,y]
        } ## End of loop over years
    } ## End of loop over iterations

# Summary statistics per type of reproduction
YOY_tot_q <- YOYnat_q <- YOYcomp_q <- YOYres_q <- matrix(NA,nrow=Y_last,ncol=9) 

write.table(YOYnat,file="results/YOYnat_iter.txt", row.names=F, col.names=c("",paste0(1985:year)), sep = "\t") #mb-21.03.2022



for (y in 2:Y_last) {
  ## Moyenne
  YOY_tot_q[y,1] <- mean(YOY_tot[,y]) ; YOYnat_q[y,1] <- mean(YOYnat[,y])
  YOYcomp_q[y,1] <- mean(YOYcomp[,y]) ; YOYres_q[y,1] <- mean(YOYres[,y])

  ## Sd
  YOY_tot_q[y,2] <- sd(YOY_tot[,y]) ; YOYnat_q[y,2] <- sd(YOYnat[,y])
  YOYcomp_q[y,2] <- sd(YOYcomp[,y]) ; YOYres_q[y,2] <- sd(YOYres[,y])

  ## Quantiles
  YOY_tot_q[y,3:9] <- quantile(YOY_tot[,y],probs,names=FALSE)
  YOYnat_q[y,3:9] <- quantile(YOYnat[,y],probs,names=FALSE)
  YOYcomp_q[y,3:9] <- quantile(YOYcomp[,y],probs,names=FALSE)
  YOYres_q[y,3:9] <- quantile(YOYres[,y],probs,names=FALSE)
  }

# Write the results in tables
cnames <- c("mean", "sd","q0.025", "q0.05", "q0.25","q0.5","q0.75","q0.95","q0.975")
write.table(YOY_tot_q,file="results/YOY_tot_q.txt", row.names=F, col.names=cnames, sep = "\t")
write.table(YOYnat_q,file="results/YOYnat_q.txt", row.names=F, col.names=cnames, sep = "\t")
write.table(YOYcomp_q,file="results/YOYcomp_q.txt", row.names=F, col.names=cnames, sep = "\t")
write.table(YOYres_q,file="results/YOYres_q.txt", row.names=F, col.names=cnames, sep = "\t")
#write.csv(YOY_tot_q,file="results/YOY_tot.csv", row.names=F, col.names=cnames)#, sep = "\t")
#write.table(YOYnat_q,file="results/YOYnat.txt", row.names=F, col.names=cnames, sep = "\t")
#write.table(YOYcomp_q,file="results/YOYcomp.txt", row.names=F, col.names=cnames, sep = "\t")
#write.table(YOYres_q,file="results/YOYres.txt", row.names=F, col.names=cnames, sep = "\t")


##########################################################################
##########################################################################
#####TOTAL PER YEAR, PER ZONE AND PER TYPE OF RECRUITMENT####
########################################################################
########################################################################

# Preparing matrix of results to fill-in
jLN_q <- jLN_nat_q <- jLN_comp_q <- matrix(NA,nrow=Y_last,ncol=9) 

jHN_q <- jHN_nat_q <- jHN_comp_q <- jHN_res_q <- matrix(NA,nrow=Y_last,ncol=9) 

jLUR_q <- jLUR_nat_q <- jLUR_comp_q <- jLUR_res_q <- matrix(NA,nrow=Y_last,ncol=9)

jVHN_q <- jVHN_res_q <- matrix(NA,nrow=Y_last,ncol=9) 

jLAP_q <- jLAP_res_q <- matrix(NA,nrow=Y_last,ncol=9) 

## Summary statistics per zone and type of reproduction ##
  for (y in 1:Y_last) {
  ## Moyenne
  jLN_q[y,1] <- mean(jLN[,y]) 
  jLN_nat_q[y,1] <- mean(jLN_nat[,y])
  jLN_comp_q[y,1] <- mean(jLN_comp[,y])

  jHN_q[y,1] <- mean(jHN[,y]) 
  jHN_nat_q[y,1] <- mean(jHN_nat[,y])
  jHN_comp_q[y,1] <- mean(jHN_comp[,y]) 
  jHN_res_q[y,1] <- mean(jHN_res[,y])

  jLUR_q[y,1] <- mean(jLUR[,y]) 
  jLUR_nat_q[y,1] <- mean(jLUR_nat[,y])
  jLUR_comp_q[y,1] <- mean(jLUR_comp[,y])
  jLUR_res_q[y,1] <- mean(jLUR_res[,y])

  jVHN_q[y,1] <- mean(jVHN[,y]) 
  jVHN_res_q[y,1] <- mean(jVHN_res[,y])

  jLAP_q[y,1] <- mean(jLAP[,y]) 
  jLAP_res_q[y,1] <- mean(jLAP_res[,y])

  ## Sd
  jLN_q[y,2] <- sd(jLN[,y])
  jLN_nat_q[y,2] <- sd(jLN_nat[,y])
  jLN_comp_q[y,2] <- sd(jLN_comp[,y])

  jHN_q[y,2] <- sd(jHN[,y]) 
  jHN_nat_q[y,2] <- sd(jHN_nat[,y])
  jHN_comp_q[y,2] <- sd(jHN_comp[,y]) 
  jHN_res_q[y,2] <- sd(jHN_res[,y])

  jLUR_q[y,2] <- sd(jLUR[,y]) 
  jLUR_nat_q[y,2] <- sd(jLUR_nat[,y])
  jLUR_comp_q[y,2] <- sd(jLUR_comp[,y]) 
  jLUR_res_q[y,2] <- sd(jLUR_res[,y])

  jVHN_q[y,2] <- sd(jVHN[,y])
  jVHN_res_q[y,2] <- sd(jVHN_res[,y])

  jLAP_q[y,2] <- sd(jLAP[,y])
  jLAP_res_q[y,2] <- sd(jLAP_res[,y])

  ## Quantiles
  jLN_q[y,3:9] <- quantile(jLN[,y],probs,names=FALSE)
  jLN_nat_q[y,3:9] <- quantile(jLN_nat[,y],probs,names=FALSE)
  jLN_comp_q[y,3:9] <- quantile(jLN_comp[,y],probs,names=FALSE)

  jHN_q[y,3:9] <- quantile(jHN[,y],probs,names=FALSE)
  jHN_nat_q[y,3:9] <- quantile(jHN_nat[,y],probs,names=FALSE)
  jHN_comp_q[y,3:9] <- quantile(jHN_comp[,y],probs,names=FALSE)
  jHN_res_q[y,3:9] <- quantile(jHN_res[,y],probs,names=FALSE)

  jLUR_q[y,3:9] <- quantile(jLUR[,y],probs,names=FALSE)
  jLUR_nat_q[y,3:9] <- quantile(jLUR_nat[,y],probs,names=FALSE)
  jLUR_comp_q[y,3:9] <- quantile(jLUR_comp[,y],probs,names=FALSE)
  jLUR_res_q[y,3:9] <- quantile(jLUR_res[,y],probs,names=FALSE)

  jVHN_q[y,3:9] <- quantile(jVHN[,y],probs,names=FALSE)
  jVHN_res_q[y,3:9] <- quantile(jVHN_res[,y],probs,names=FALSE)

  jLAP_q[y,3:9] <- quantile(jLAP[,y],probs,names=FALSE)
  jLAP_res_q[y,3:9] <- quantile(jLAP_res[,y],probs,names=FALSE)
  } ## End of loop over years

# Write the results in tables
write.table(jLN_q,file="results/jLN.txt", row.names=F, col.names=cnames, sep = "\t") 
write.table(jLN_nat_q,file="results/jLN_nat.txt", row.names=F, col.names=cnames, sep = "\t")
write.table(jLN_comp_q,file="results/jLN_comp.txt", row.names=F, col.names=cnames, sep = "\t")

write.table(jHN_q,file="results/jHN.txt", row.names=F, col.names=cnames, sep = "\t") 
write.table(jHN_nat_q,file="results/jHN_nat.txt", row.names=F, col.names=cnames, sep = "\t")
write.table(jHN_comp_q,file="results/jHN_comp.txt", row.names=F, col.names=cnames, sep = "\t") 
write.table(jHN_res_q,file="results/jHN_res.txt", row.names=F, col.names=cnames, sep = "\t")

write.table(jLUR_q,file="results/jLUR.txt", row.names=F, col.names=cnames, sep = "\t") 
write.table(jLUR_nat_q,file="results/jLUR_nat.txt", row.names=F, col.names=cnames, sep = "\t")
write.table(jLUR_comp_q,file="results/jLUR_comp.txt", row.names=F, col.names=cnames, sep = "\t") 
write.table(jLUR_res_q,file="results/jLUR_res.txt", row.names=F, col.names=cnames, sep = "\t")

write.table(jVHN_q,file="results/jVHN.txt", row.names=F, col.names=cnames, sep = "\t") 
write.table(jVHN_res_q,file="results/jVHN_res.txt", row.names=F, col.names=cnames, sep = "\t")

write.table(jLAP_q,file="results/jLAP.txt", row.names=F, col.names=cnames, sep = "\t") 
write.table(jLAP_res_q,file="results/jLAP_res.txt", row.names=F, col.names=cnames, sep = "\t")

###################################################"
###################################################"
## WEIGHED MEAN (DENSITY)
## -----------------------------------------------------------------------------
## 1. Mean per zone X year
## ---------------------
###################################################"
###################################################"
S_req <- dj_wth <- dj_nat_wth <- dj_comp_wth <- dj_res_wth <- matrix(0,nrow=n_iter,ncol=I)

pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
setTxtProgressBar(pb,i)

    for (j in 1:247) {
    S_req[i,j] <- sites$S[j]*beta[i,sites$H[j]] #beta is habitat effect linked to runs
    dj_wth[i,j] <- dj[i,j]*sites$S[j] ; dj_nat_wth[i,j] <- dj_wth[i,j]
    } ## End of loop over sites
    
    for (j in 248:336) {
    S_req[i,j] <- sites$S[j]*beta[i,sites$H[j]]
    dj_wth[i,j] <- dj[i,j]*sites$S[j]
    dj_nat_wth[i,j] <- dj_nat[i,sites$Comp[j]]*sites$S[j]
    dj_comp_wth[i,j] <- dj_comp[i,sites$Comp[j]]*sites$S[j]
    } ## End of loop over sites
    
    for (j in 337:422) {
    S_req[i,j] <- sites$S[j]*beta[i,sites$H[j]]
    dj_wth[i,j] <- dj[i,j]*sites$S[j] ; dj_res_wth[i,j] <- dj_wth[i,j]
    } ## End of loop over sites
    
    for (j in 423:434) {
    S_req[i,j] <- sites$S[j]*beta[i,sites$H[j]]
    dj_wth[i,j] <- dj[i,j]*sites$S[j] ; dj_nat_wth[i,j] <- dj_wth[i,j]
    } ## End of loop over sites
    
    for (j in 435:445) {
    S_req[i,j] <- sites$S[j]*beta[i,sites$H[j]]
    dj_wth[i,j] <- dj[i,j]*sites$S[j]
    dj_nat_wth[i,j] <- dj_nat[i,sites$Comp[j]]*sites$S[j]
    dj_comp_wth[i,j] <- dj_comp[i,sites$Comp[j]]*sites$S[j]
    } ## End of loop over sites
    
    for (j in 446:458) {
    S_req[i,j] <- sites$S[j]*beta[i,sites$H[j]]
    dj_wth[i,j] <- dj[i,j]*sites$S[j] ; dj_nat_wth[i,j] <- dj_wth[i,j]
    } ## End of loop over sites
    
    for (j in 459:515) {
    S_req[i,j] <- sites$S[j]*beta[i,sites$H[j]]
    dj_wth[i,j] <- dj[i,j]*sites$S[j]
    dj_nat_wth[i,j] <- dj_nat[i,sites$Comp[j]]*sites$S[j]
    dj_comp_wth[i,j] <- dj_comp[i,sites$Comp[j]]*sites$S[j]
    } ## End of loop over sites
    
    for (j in 516:I) {
    S_req[i,j] <- sites$S[j]*beta[i,sites$H[j]]
    dj_wth[i,j] <- dj[i,j]*sites$S[j] ; dj_nat_wth[i,j] <- dj_wth[i,j]
    } ## End of loop over sites
  } ## End of loop over iterations

## -----------------------------------------------------------------------------
# Predicted Density per type of recruitment, per zone and per year
Num1_LN <- Num1_nat_LN <- Num1_comp_LN <- Num1_res_LN <- matrix(0,nrow=n_iter,ncol=Y_last)

Num1_HN <- Num1_nat_HN <- Num1_comp_HN <- Num1_res_HN <- matrix(0,nrow=n_iter,ncol=Y_last)

Num1_LUR <- Num1_nat_LUR <- Num1_comp_LUR <- Num1_res_LUR <- matrix(0,nrow=n_iter,ncol=Y_last)

Num1_VHN <- Num1_res_VHN <- Num1_LAP <- Num1_res_LAP <- matrix(0,nrow=n_iter,ncol=Y_last)

pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
setTxtProgressBar(pb,i)
  
  Num1_LN[i,2] <- Num1_nat_LN[i,2] <- sum(dj_wth[i,START_i[2,3]:END_i[2,3]])

    for(y in 3:5) {  # from 1986 to 1988
    Num1_LN[i,y] <- Num1_nat_LN[i,y] <- sum(dj_wth[i,START_i[y,3]:END_i[y,3]])
    Num1_HN[i,y] <- Num1_res_HN[i,y] <- sum(dj_wth[i,START_i[y,4]:END_i[y,4]])
    Num1_LUR[i,y] <- Num1_res_LUR[i,y] <- sum(dj_wth[i,START_i[y,5]:END_i[y,5]])
    } ## End of loop over years

  # Year 1989
  Num1_LN[i,6] <- Num1_nat_LN[i,6] <- sum(dj_wth[i,START_i[6,3]:END_i[6,3]])
  Num1_HN[i,6] <- Num1_res_HN[i,6] <- sum(dj_wth[i,START_i[6,4]:END_i[6,4]])
  Num1_LUR[i,6] <- Num1_res_LUR[i,6] <- sum(dj_wth[i,START_i[6,5]:END_i[6,5]])
  Num1_LAP[i,6] <- Num1_res_LAP[i,6] <- sum(dj_wth[i,403:414])

  # Year 1990
  Num1_LN[i,7] <- Num1_nat_LN[i,7] <- sum(dj_wth[i,START_i[7,3]:END_i[7,3]])
  Num1_HN[i,7] <- Num1_res_HN[i,7] <- sum(dj_wth[i,START_i[7,4]:END_i[7,4]])
  Num1_LUR[i,7] <- Num1_res_LUR[i,7] <- sum(dj_wth[i,START_i[7,5]:END_i[7,5]])

  # Year 1991 & 1992
    for(y in 8:9) {
    Num1_LN[i,y] <- Num1_nat_LN[i,y] <- sum(dj_wth[i,START_i[y,3]:END_i[y,3]])
    Num1_HN[i,y] <- Num1_nat_HN[i,y] <- sum(dj_wth[i,START_i[y,4]:END_i[y,4]])
    Num1_LUR[i,y] <- Num1_nat_LUR[i,y] <- sum(dj_wth[i,START_i[y,5]:END_i[y,5]])
    } ## End of loop over years
    
  # Year 1991 & 1992, VHN  
  Num1_VHN[i,8] <- Num1_res_VHN[i,8] <- sum(dj_wth[i,397:398])
  Num1_VHN[i,9] <- Num1_res_VHN[i,9] <- sum(dj_wth[i,399:400])

  # Year 1993
  Num1_LN[i,10] <- Num1_nat_LN[i,10] <- sum(dj_wth[i,START_i[10,3]:END_i[10,3]])
  Num1_HN[i,10] <- Num1_nat_HN[i,10] <- sum(dj_wth[i,START_i[10,4]:END_i[10,4]])
  Num1_LUR[i,10] <- Num1_nat_LUR[i,10] <- sum(dj_wth[i,START_i[10,5]:END_i[10,5]])

  # Year 1994
  Num1_LN[i,11] <- Num1_nat_LN[i,11] <- sum(dj_wth[i,START_i[11,3]:END_i[11,3]])
  Num1_HN[i,11] <- Num1_nat_HN[i,11] <- sum(dj_wth[i,START_i[11,4]:END_i[11,4]])
  Num1_LUR[i,11] <- Num1_nat_LUR[i,11] <- sum(dj_wth[i,START_i[11,5]:END_i[11,5]])
  Num1_VHN[i,11] <- Num1_res_VHN[i,11] <- sum(dj_wth[i,401:402])
  Num1_LAP[i,11] <- Num1_res_LAP[i,11] <- sum(dj_wth[i,415:418])

  # Year 1995
  Num1_LN[i,12] <- Num1_nat_LN[i,12] <- sum(dj_wth[i,START_i[12,3]:END_i[12,3]])
  Num1_HN[i,12] <- Num1_nat_HN[i,12] <- sum(dj_wth[i,START_i[12,4]:END_i[12,4]])
  Num1_LUR[i,12] <- Num1_nat_LUR[i,12] <- sum(dj_wth[i,START_i[12,5]:END_i[12,5]])
  Num1_LAP[i,12] <- Num1_res_LAP[i,12] <- sum(dj_wth[i,419:422])

  # Year 1996
  Num1_LN[i,13] <- sum(dj_wth[i,START_i[13,3]:END_i[13,3]])
  Num1_nat_LN[i,13] <- sum(dj_nat_wth[i,START_i[13,3]:END_i[13,3]])
  Num1_comp_LN[i,13] <- sum(dj_comp_wth[i,START_i[13,3]:END_i[13,3]])
  
  Num1_HN[i,13] <- sum(dj_wth[i,START_i[13,4]:END_i[13,4]])
  Num1_nat_HN[i,13] <- sum(dj_nat_wth[i,START_i[13,4]:END_i[13,4]])
  Num1_comp_HN[i,13] <- sum(dj_comp_wth[i,START_i[13,4]:END_i[13,4]])
  
  Num1_LUR[i,13] <- sum(dj_wth[i,START_i[13,5]:END_i[13,5]])
  Num1_nat_LUR[i,13] <- sum(dj_nat_wth[i,START_i[13,5]:END_i[13,5]])
  Num1_comp_LUR[i,13] <- sum(dj_comp_wth[i,START_i[13,5]:END_i[13,5]])

  # Year 1997
  Num1_LN[i,14] <- Num1_nat_LN[i,14] <- sum(dj_wth[i,START_i[14,3]:END_i[14,3]])
  Num1_HN[i,14] <- Num1_nat_HN[i,14] <- sum(dj_wth[i,START_i[14,4]:END_i[14,4]])
  Num1_LUR[i,14] <- Num1_nat_LUR[i,14] <- sum(dj_wth[i,START_i[14,5]:END_i[14,5]])

  # Year 1998 & 1999
    for(y in 15:16) {
    Num1_LN[i,y] <- sum(dj_wth[i,START_i[y,3]:END_i[y,3]])
    Num1_nat_LN[i,y] <- sum(dj_nat_wth[i,START_i[y,3]:END_i[y,3]])
    Num1_comp_LN[i,y] <- sum(dj_comp_wth[i,START_i[y,3]:END_i[y,3]])
    
    Num1_HN[i,y] <- sum(dj_wth[i,START_i[y,4]:END_i[y,4]])
    Num1_nat_HN[i,y] <- sum(dj_nat_wth[i,START_i[y,4]:END_i[y,4]])
    Num1_comp_HN[i,y] <- sum(dj_comp_wth[i,START_i[y,4]:END_i[y,4]])
    
    Num1_LUR[i,y] <- sum(dj_wth[i,START_i[y,5]:END_i[y,5]])
    Num1_nat_LUR[i,y] <- sum(dj_nat_wth[i,START_i[y,5]:END_i[y,5]])
    Num1_comp_LUR[i,y] <- sum(dj_comp_wth[i,START_i[y,5]:END_i[y,5]])
    } ## End of loop over years

  # Year 2000
  Num1_LN[i,17] <- Num1_nat_LN[i,17] <- sum(dj_wth[i,START_i[17,3]:END_i[17,3]])
  
  Num1_HN[i,17] <- sum(dj_wth[i,START_i[17,4]:END_i[17,4]])
  Num1_nat_HN[i,17] <- sum(dj_nat_wth[i,START_i[17,4]:END_i[17,4]])
  Num1_comp_HN[i,17] <- sum(dj_comp_wth[i,START_i[17,4]:END_i[17,4]])
  
  Num1_LUR[i,17] <- Num1_nat_LUR[i,17] <- sum(dj_wth[i,START_i[17,5]:END_i[17,5]])

  # Year 2001
  Num1_LN[i,18] <- Num1_nat_LN[i,18] <- sum(dj_wth[i,START_i[18,3]:END_i[18,3]])
  Num1_HN[i,18] <- Num1_nat_HN[i,18] <- sum(dj_wth[i,START_i[18,4]:END_i[18,4]])
  Num1_LUR[i,18] <- Num1_nat_LUR[i,18] <- sum(dj_wth[i,START_i[18,5]:END_i[18,5]])

  # Year 2002
  Num1_LN[i,19] <- Num1_nat_LN[i,19] <- sum(dj_wth[i,START_i[19,3]:END_i[19,3]])
  
  Num1_HN[i,19] <- sum(dj_wth[i,START_i[19,4]:END_i[19,4]])
  Num1_nat_HN[i,19] <- sum(dj_nat_wth[i,START_i[19,4]:END_i[19,4]])
  Num1_comp_HN[i,19] <- sum(dj_comp_wth[i,START_i[19,4]:END_i[19,4]])
  
  Num1_LUR[i,19] <- sum(dj_wth[i,START_i[19,5]:END_i[19,5]])
  Num1_nat_LUR[i,19] <- sum(dj_nat_wth[i,START_i[19,5]:END_i[19,5]])
  Num1_comp_LUR[i,19] <- sum(dj_comp_wth[i,START_i[19,5]:END_i[19,5]])

  # Year 2003
  Num1_LN[i,20] <- Num1_nat_LN[i,20] <- sum(dj_wth[i,START_i[20,3]:END_i[20,3]])+sum(dj_wth[i,START_i_bis[20,3]:END_i_bis[20,3]])+dj_wth[i,446]
  Num1_HN[i,20] <- Num1_nat_HN[i,20] <- sum(dj_wth[i,START_i[20,4]:END_i[20,4]])+sum(dj_wth[i,START_i_bis[20,4]:END_i_bis[20,4]])+sum(dj_wth[i,START_i_CPUE[20,4]:END_i_CPUE[20,4]])
  Num1_LUR[i,20] <- Num1_nat_LUR[i,20] <- sum(dj_wth[i,START_i[20,5]:END_i[20,5]])+dj_wth[i,434]+dj_wth[i,455]

  # Year 2004
  Num1_LN[i,21] <- Num1_nat_LN[i,21] <- sum(dj_wth[i,START_i[21,3]:END_i[21,3]])+sum(dj_wth[i,START_i_bis[21,3]:END_i_bis[21,3]])
  
  Num1_HN[i,21] <- sum(dj_wth[i,START_i[21,4]:END_i[21,4]])+sum(dj_wth[i,START_i_bis[21,4]:END_i_bis[21,4]])
  Num1_nat_HN[i,21] <- sum(dj_nat_wth[i,START_i[21,4]:END_i[21,4]])+sum(dj_nat_wth[i,START_i_bis[21,4]:END_i_bis[21,4]])
  Num1_comp_HN[i,21] <- sum(dj_comp_wth[i,START_i[21,4]:END_i[21,4]])+sum(dj_comp_wth[i,START_i_bis[21,4]:END_i_bis[21,4]])
  
  Num1_LUR[i,21] <- sum(dj_wth[i,START_i[21,5]:END_i[21,5]])+sum(dj_wth[i,START_i_bis[21,5]:END_i_bis[21,5]])
  Num1_nat_LUR[i,21] <- sum(dj_nat_wth[i,START_i[21,5]:END_i[21,5]])+sum(dj_nat_wth[i,START_i_bis[21,5]:END_i_bis[21,5]])
  Num1_comp_LUR[i,21] <- sum(dj_comp_wth[i,START_i[21,5]:END_i[21,5]])+sum(dj_comp_wth[i,START_i_bis[21,5]:END_i_bis[21,5]])

  # Year 2005
  Num1_LN[i,22] <- Num1_nat_LN[i,22] <- sum(dj_wth[i,START_i[22,3]:END_i[22,3]])+sum(dj_wth[i,START_i_bis[22,3]:END_i_bis[22,3]])+sum(dj_wth[i,START_i_CPUE[22,3]:END_i_CPUE[22,3]])
  
  Num1_HN[i,22] <- sum(dj_wth[i,START_i[22,4]:END_i[22,4]])+sum(dj_wth[i,START_i_bis[22,4]:END_i_bis[22,4]])+sum(dj_wth[i,START_i_CPUE[22,4]:END_i_CPUE[22,4]])
  Num1_nat_HN[i,22] <- sum(dj_nat_wth[i,START_i[22,4]:END_i[22,4]])+sum(dj_nat_wth[i,START_i_bis[22,4]:END_i_bis[22,4]])+sum(dj_nat_wth[i,START_i_CPUE[22,4]:END_i_CPUE[22,4]])
  Num1_comp_HN[i,22] <- sum(dj_comp_wth[i,START_i[22,4]:END_i[22,4]])+sum(dj_comp_wth[i,START_i_bis[22,4]:END_i_bis[22,4]])+sum(dj_comp_wth[i,START_i_CPUE[22,4]:END_i_CPUE[22,4]])
  
  Num1_LUR[i,22] <- sum(dj_wth[i,START_i[22,5]:END_i[22,5]])+dj_wth[i,445]+sum(dj_wth[i,START_i_CPUE[22,5]:END_i_CPUE[22,5]])
  Num1_nat_LUR[i,22] <- sum(dj_nat_wth[i,START_i[22,5]:END_i[22,5]])+dj_nat_wth[i,445]+sum(dj_nat_wth[i,START_i_CPUE[22,5]:END_i_CPUE[22,5]])
  Num1_comp_LUR[i,22] <- sum(dj_comp_wth[i,START_i[22,5]:END_i[22,5]])+dj_comp_wth[i,445]+sum(dj_comp_wth[i,START_i_CPUE[22,5]:END_i_CPUE[22,5]])

  # Year 2006
  Num1_LN[i,23] <- Num1_nat_LN[i,23] <- sum(dj_wth[i,START_i_CPUE[23,3]:END_i_CPUE[23,3]])
  
  Num1_HN[i,23] <- sum(dj_wth[i,START_i_CPUE[23,4]:END_i_CPUE[23,4]])
  Num1_nat_HN[i,23] <- sum(dj_nat_wth[i,START_i_CPUE[23,4]:END_i_CPUE[23,4]])
  Num1_comp_HN[i,23] <- sum(dj_comp_wth[i,START_i_CPUE[23,4]:END_i_CPUE[23,4]])
  
  Num1_LUR[i,23] <- Num1_nat_LUR[i,23] <- sum(dj_wth[i,START_i_CPUE[23,5]:END_i_CPUE[23,5]])

  # Year 2007 & 2008
    for(y in 24:25) {
    Num1_LN[i,y] <- sum(dj_wth[i,START_i_CPUE[y,3]:END_i_CPUE[y,3]])
    Num1_nat_LN[i,y] <- sum(dj_nat_wth[i,START_i_CPUE[y,3]:END_i_CPUE[y,3]])
    Num1_comp_LN[i,y] <- sum(dj_comp_wth[i,START_i_CPUE[y,3]:END_i_CPUE[y,3]])
    
    Num1_HN[i,y] <- sum(dj_wth[i,START_i_CPUE[y,4]:END_i_CPUE[y,4]])
    Num1_nat_HN[i,y] <- sum(dj_nat_wth[i,START_i_CPUE[y,4]:END_i_CPUE[y,4]])
    Num1_comp_HN[i,y] <- sum(dj_comp_wth[i,START_i_CPUE[y,4]:END_i_CPUE[y,4]])
    
    Num1_LUR[i,y] <- sum(dj_wth[i,START_i_CPUE[y,5]:END_i_CPUE[y,5]])
    Num1_nat_LUR[i,y] <- sum(dj_nat_wth[i,START_i_CPUE[y,5]:END_i_CPUE[y,5]])
    Num1_comp_LUR[i,y] <- sum(dj_comp_wth[i,START_i_CPUE[y,5]:END_i_CPUE[y,5]])
    } ## End of loop over years

  # Year 2009
  Num1_LN[i,26] <- sum(dj_wth[i,START_i_CPUE[26,3]:END_i_CPUE[26,3]])
  Num1_nat_LN[i,26] <- sum(dj_nat_wth[i,START_i_CPUE[26,3]:END_i_CPUE[26,3]])
  Num1_comp_LN[i,26] <- sum(dj_comp_wth[i,START_i_CPUE[26,3]:END_i_CPUE[26,3]])
  
  Num1_HN[i,26] <- Num1_nat_HN[i,26] <- sum(dj_wth[i,START_i_CPUE[26,4]:END_i_CPUE[26,4]])
  Num1_LUR[i,26] <- Num1_nat_LUR[i,26] <- sum(dj_wth[i,START_i_CPUE[26,5]:END_i_CPUE[26,5]])

  # From year 2010 to now on
  for(y in 27:Y_last) {
  Num1_LN[i,y] <- Num1_nat_LN[i,y] <- sum(dj_wth[i,START_i_CPUE[y,3]:END_i_CPUE[y,3]])
  Num1_HN[i,y] <- Num1_nat_HN[i,y] <- sum(dj_wth[i,START_i_CPUE[y,4]:END_i_CPUE[y,4]])
  Num1_LUR[i,y] <- Num1_nat_LUR[i,y] <- sum(dj_wth[i,START_i_CPUE[y,5]:END_i_CPUE[y,5]])
    } ## End of loop over years
  } ## End of loop over iterations

# Surface per zone in run equivalent?
## -----------------------------------------------------------------------------
Den1_LN <- Den1_HN <- Den1_LUR <- Den1_VHN <- Den1_LAP <- matrix(1,nrow=n_iter,ncol=Y_last)


pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
setTxtProgressBar(pb,i)
  
  Den1_LN[i,2] <- sum(S_req[i,START_i[2,3]:END_i[2,3]]) # Year 1985 for LN

  # From 1986 to 2002 for LN, HN & LUR
    for(y in 3:19) {
    Den1_LN[i,y] <- sum(S_req[i,START_i[y,3]:END_i[y,3]])
    Den1_HN[i,y] <- sum(S_req[i,START_i[y,4]:END_i[y,4]])
    Den1_LUR[i,y] <- sum(S_req[i,START_i[y,5]:END_i[y,5]])
    } ## End of loop over years

  # Year 1989 for LAP
  Den1_LAP[i,6] <- sum(S_req[i,403:414])
  
  # Year 1991 for VHN
  Den1_VHN[i,8] <- sum(S_req[i,397:398])
  
  # Year 1992 for VHN
  Den1_VHN[i,9] <- sum(S_req[i,399:400])
  
  # Year 1994 for VHN and LAP
  Den1_VHN[i,11] <- sum(S_req[i,401:402])
  Den1_LAP[i,11] <- sum(S_req[i,415:418])
  
  #Year 1995 for LAP
  Den1_LAP[i,12] <- sum(S_req[i,419:422])

  # Year 2003
  Den1_LN[i,20] <- sum(S_req[i,START_i[20,3]:END_i[20,3]])+sum(S_req[i,START_i_bis[20,3]:END_i_bis[20,3]])+S_req[i,446]
  Den1_HN[i,20] <- sum(S_req[i,START_i[20,4]:END_i[20,4]])+sum(S_req[i,START_i_bis[20,4]:END_i_bis[20,4]])+sum(S_req[i,START_i_CPUE[20,4]:END_i_CPUE[20,4]])
  Den1_LUR[i,20] <- sum(S_req[i,START_i[20,5]:END_i[20,5]])+S_req[i,434]+S_req[i,455]

  #Year 2004
  Den1_LN[i,21] <- sum(S_req[i,START_i[21,3]:END_i[21,3]])+sum(S_req[i,START_i_bis[21,3]:END_i_bis[21,3]])
  Den1_HN[i,21] <- sum(S_req[i,START_i[21,4]:END_i[21,4]])+sum(S_req[i,START_i_bis[21,4]:END_i_bis[21,4]])
  Den1_LUR[i,21] <- sum(S_req[i,START_i[21,5]:END_i[21,5]])+sum(S_req[i,START_i_bis[21,5]:END_i_bis[21,5]])

  #Year 2005
  Den1_LN[i,22] <- sum(S_req[i,START_i[22,3]:END_i[22,3]])+sum(S_req[i,START_i_bis[22,3]:END_i_bis[22,3]])+sum(S_req[i,START_i_CPUE[22,3]:END_i_CPUE[22,3]])
  Den1_HN[i,22] <- sum(S_req[i,START_i[22,4]:END_i[22,4]])+sum(S_req[i,START_i_bis[22,4]:END_i_bis[22,4]])+sum(S_req[i,START_i_CPUE[22,4]:END_i_CPUE[22,4]])
  Den1_LUR[i,22] <- sum(S_req[i,START_i[22,5]:END_i[22,5]])+S_req[i,445]+sum(S_req[i,START_i_CPUE[22,5]:END_i_CPUE[22,5]])

  #From 2006 to now on
    for(y in 23:Y_last) {
    Den1_LN[i,y] <- sum(S_req[i,START_i_CPUE[y,3]:END_i_CPUE[y,3]])
    Den1_HN[i,y] <- sum(S_req[i,START_i_CPUE[y,4]:END_i_CPUE[y,4]])
    Den1_LUR[i,y] <- sum(S_req[i,START_i_CPUE[y,5]:END_i_CPUE[y,5]])
    } ## End of loop over years

  } ## End of loop over iterations

#Predicted density per surface per zone in run equivalent, per type of recruitment and per year?
## -----------------------------------------------------------------------------
mu_dj_wth_LN <- mu_djnat_wth_LN <- mu_djcomp_wth_LN <- mu_djres_wth_LN <- matrix(0,nrow=n_iter,ncol=Y_last)

mu_dj_wth_HN <- mu_djnat_wth_HN <- mu_djcomp_wth_HN <- mu_djres_wth_HN <- matrix(0,nrow=n_iter,ncol=Y_last)

mu_dj_wth_LUR <- mu_djnat_wth_LUR <- mu_djcomp_wth_LUR <- mu_djres_wth_LUR <- matrix(0,nrow=n_iter,ncol=Y_last)

mu_dj_wth_VHN <- mu_djres_wth_VHN <- matrix(0,nrow=n_iter,ncol=Y_last)

mu_dj_wth_LAP <- mu_djres_wth_LAP <- matrix(0,nrow=n_iter,ncol=Y_last)


pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
setTxtProgressBar(pb,i)

    # From 1985 to now on!
    for (y in 2:Y_last) {
    
    # Zone LN
    mu_dj_wth_LN[i,y] <- Num1_LN[i,y]/max(Den1_LN[i,y],1)
    mu_djnat_wth_LN[i,y] <- Num1_nat_LN[i,y]/max(Den1_LN[i,y],1)
    mu_djcomp_wth_LN[i,y] <- Num1_comp_LN[i,y]/max(Den1_LN[i,y],1)
    mu_djres_wth_LN[i,y] <- Num1_res_LN[i,y]/max(Den1_LN[i,y],1)

    # Zone HN
    mu_dj_wth_HN[i,y] <- Num1_HN[i,y]/max(Den1_HN[i,y],1)
    mu_djnat_wth_HN[i,y] <- Num1_nat_HN[i,y]/max(Den1_HN[i,y],1)
    mu_djcomp_wth_HN[i,y] <- Num1_comp_HN[i,y]/max(Den1_HN[i,y],1)
    mu_djres_wth_HN[i,y] <- Num1_res_HN[i,y]/max(Den1_HN[i,y],1)

    # Zone LUR
    mu_dj_wth_LUR[i,y] <- Num1_LUR[i,y]/max(Den1_LUR[i,y],1)
    mu_djnat_wth_LUR[i,y] <- Num1_nat_LUR[i,y]/max(Den1_LUR[i,y],1)
    mu_djcomp_wth_LUR[i,y] <- Num1_comp_LUR[i,y]/max(Den1_LUR[i,y],1)
    mu_djres_wth_LUR[i,y] <- Num1_res_LUR[i,y]/max(Den1_LUR[i,y],1)

    # Zone VHN
    mu_dj_wth_VHN[i,y] <- Num1_VHN[i,y]/max(Den1_VHN[i,y],1)
    mu_djres_wth_VHN[i,y] <- Num1_res_VHN[i,y]/max(Den1_VHN[i,y],1)

    # Zone LAP
    mu_dj_wth_LAP[i,y] <- Num1_LAP[i,y]/max(Den1_LAP[i,y],1)
    mu_djres_wth_LAP[i,y] <- Num1_res_LAP[i,y]/max(Den1_LAP[i,y],1)
    } ## End of loop over years

  } ## End of loop over iterations

#####################
## 2. Mean per year
## ----------------
#####################

## Indicatrices
I_y <- matrix(0,nrow=Y_last,ncol=I) # indicator of which sites is sampled per year 
I_z <- matrix(0,nrow=8,ncol=I) # indicator of the zones of each sites
somme <-  I_prod <- matrix(0,nrow=Y_last,ncol=8)

  for(j in 1:I) {
    for (y in 2:Y_last) {
      if (y==sites$Y[j]) {
      I_y[y,j] <- 1
      }
    } ## End of loop over years
    
    for (z in 3:8) { # there isn't any zone 1, zone 2 and zone 6 (zone 6, only 0)
      if (z==sites$Z[j]) {
      I_z[z,j] <- 1
      }
    } ## End of loop over zones
  } ## End of loop over sites
  
    for (y in 2:Y_last) {
      for (z in 3:8) {
      somme[y,z] <- sum(I_y[y,]*I_z[z,])
      I_prod[y,z] <- min(1,somme[y,z]) ## Couples zone X ann?e ?chantillonn?s
      } ## End of loop over zones
    } ## End of loop over years


# Total surface in equivalent riffle
Stot_req <- matrix(NA,nrow=n_iter,ncol=8)

mu_dj_wth_y <- mu_djnat_wth_y <- mu_djcomp_wth_y <- mu_djres_wth_y <- matrix(NA,nrow=n_iter,ncol=Y_last)


pb = txtProgressBar(min = 0, max = n_iter, initial = 0) 
for (i in 1:n_iter) {
setTxtProgressBar(pb,i)
  Stot_req[i,3] <- data$S_tot[3,1] + (data$S_tot[3,2] *beta[i,2])
  Stot_req[i,4] <- data$S_tot[4,1] + (data$S_tot[4,2] *beta[i,2])
  Stot_req[i,5] <- data$S_tot[5,1] + (data$S_tot[5,2] *beta[i,2])
  Stot_req[i,7] <- data$S_tot[7,1] + (data$S_tot[7,2] *beta[i,2])
  Stot_req[i,8] <- data$S_tot[8,2] + (data$S_tot[8,2] *beta[i,2])

    # From 1985 to 1992
    for (y in 2:9) {
        mu_dj_wth_y[i,y] <- (mu_dj_wth_LN[i,y]*Stot_req[i,3] + mu_dj_wth_HN[i,y]*Stot_req[i,4] +
                     mu_dj_wth_LUR[i,y]*Stot_req[i,5] + mu_dj_wth_VHN[i,y]*Stot_req[i,7]+
                     mu_dj_wth_LAP[i,y]*Stot_req[i,8]) / ((I_prod[y,3]*Stot_req[i,3])+
                     (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) +
                     (I_prod[y,7]*Stot_req[i,7]) + (I_prod[y,8]*Stot_req[i,8]))
                     
        mu_djnat_wth_y[i,y] <- (mu_djnat_wth_LN[i,y]*Stot_req[i,3] + mu_djnat_wth_HN[i,y]*Stot_req[i,4] +
                     mu_djnat_wth_LUR[i,y]*Stot_req[i,5]) / ((I_prod[y,3]*Stot_req[i,3]) +
                     (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) +
                     (I_prod[y,7]*Stot_req[i,7]) + (I_prod[y,8]*Stot_req[i,8]))
                     
        mu_djcomp_wth_y[i,y] <- 0
    
        mu_djres_wth_y[i,y] <- (mu_djres_wth_HN[i,y]*Stot_req[i,4] + mu_djres_wth_LUR[i,y]*Stot_req[i,5] +
                     mu_djres_wth_VHN[i,y]*Stot_req[i,7] + mu_djres_wth_LAP[i,y]*Stot_req[i,8]) /
                     ((I_prod[y,3]*Stot_req[i,3]) + (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) +
                     (I_prod[y,7]*Stot_req[i,7])+ (I_prod[y,8]*Stot_req[i,8]))
        } ## End of loop over years
    
    # Year 1993
    mu_dj_wth_y[i,10] <- (mu_dj_wth_LN[i,10]*Stot_req[i,3] + mu_dj_wth_HN[i,10]*Stot_req[i,4] + mu_dj_wth_LUR[i,10]*Stot_req[i,5]+779) / 
                    ((I_prod[y,3]*Stot_req[i,3]) + (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) + Stot_req[i,7])
                    
    mu_djnat_wth_y[i,10] <- (mu_djnat_wth_LN[i,10]*Stot_req[i,3] + mu_djnat_wth_HN[i,10]*Stot_req[i,4] + mu_djnat_wth_LUR[i,10]*Stot_req[i,5]) / 
                    ((I_prod[y,3]*Stot_req[i,3]) + (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) + Stot_req[i,7])
                    
    mu_djcomp_wth_y[i,10] <- 0
    
    mu_djres_wth_y[i,10] <- 779/((I_prod[y,3]*Stot_req[i,3]) + (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) + Stot_req[i,7])
    
    # From 1994 to now on!
    for (y in 11:Y_last) {
        mu_dj_wth_y[i,y] <- (mu_dj_wth_LN[i,y]*Stot_req[i,3] + mu_dj_wth_HN[i,y]*Stot_req[i,4] +
                         mu_dj_wth_LUR[i,y]*Stot_req[i,5] + mu_dj_wth_VHN[i,y]*Stot_req[i,7] + mu_dj_wth_LAP[i,y]*Stot_req[i,8]) / 
                         ((I_prod[y,3]*Stot_req[i,3])+ (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) + 
                         (I_prod[y,7]*Stot_req[i,7]) + (I_prod[y,8]*Stot_req[i,8]))
                         
        mu_djnat_wth_y[i,y] <- (mu_djnat_wth_LN[i,y]*Stot_req[i,3] + mu_djnat_wth_HN[i,y]*Stot_req[i,4] + mu_djnat_wth_LUR[i,y]*Stot_req[i,5]) / 
                         ((I_prod[y,3]*Stot_req[i,3]) + (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) +
                         (I_prod[y,7]*Stot_req[i,7]) + (I_prod[y,8]*Stot_req[i,8]))
                         
        mu_djcomp_wth_y[i,y] <- (mu_djcomp_wth_LN[i,y]*Stot_req[i,3] + mu_djcomp_wth_HN[i,y]*Stot_req[i,4] + mu_djcomp_wth_LUR[i,y]*Stot_req[i,5]) /
                         ((I_prod[y,3]*Stot_req[i,3]) + (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) +
                         (I_prod[y,7]*Stot_req[i,7]) + (I_prod[y,8]*Stot_req[i,8]))
                         
        mu_djres_wth_y[i,y] <- (mu_djres_wth_HN[i,y]*Stot_req[i,4] + mu_djres_wth_LUR[i,y]*Stot_req[i,5] +
                          mu_djres_wth_VHN[i,y]*Stot_req[i,7] + mu_djres_wth_LAP[i,y]*Stot_req[i,8]) / 
                         ((I_prod[y,3]*Stot_req[i,3])+ (I_prod[y,4]*Stot_req[i,4]) + (I_prod[y,5]*Stot_req[i,5]) +
                         (I_prod[y,7]*Stot_req[i,7]) + (I_prod[y,8]*Stot_req[i,8]))
    } ## End of loop over years
  } ## End of loop over iterations

## statistiques
###############
mu_w_q <- muNat_w_q <- muComp_w_q <- muRes_w_q <- matrix(NA,nrow=Y_last,ncol=9) 

  for (y in 2:Y_last) {
  ## Moyenne
  mu_w_q[y,1] <- mean(mu_dj_wth_y[,y]) 
  muNat_w_q[y,1] <- mean(mu_djnat_wth_y[,y])
  muComp_w_q[y,1] <- mean(mu_djcomp_wth_y[,y]) 
  muRes_w_q[y,1] <- mean(mu_djres_wth_y[,y])

  ## Sd
  mu_w_q[y,2] <- sd(mu_dj_wth_y[,y]) 
  muNat_w_q[y,2] <- sd(mu_djnat_wth_y[,y])
  muComp_w_q[y,2] <- sd(mu_djcomp_wth_y[,y]) 
  muRes_w_q[y,2] <- sd(mu_djres_wth_y[,y])

  ## Quantiles
  mu_w_q[y,3:9] <- quantile(mu_dj_wth_y[,y],probs,names=FALSE)
  muNat_w_q[y,3:9] <- quantile(mu_djnat_wth_y[,y],probs,names=FALSE)
  muComp_w_q[y,3:9] <- quantile(mu_djcomp_wth_y[,y],probs,names=FALSE)
  muRes_w_q[y,3:9] <- quantile(mu_djres_wth_y[,y],probs,names=FALSE)
  } ## End of loop over years

write.table(mu_w_q,file="results/mu_w.txt", row.names=F, col.names=cnames, sep="\t")
write.table(muNat_w_q,file="results/muNat_w.txt", row.names=F, col.names=cnames, sep="\t")
write.table(muComp_w_q,file="results/muComp_w.txt", row.names=F, col.names=cnames, sep="\t")
write.table(muRes_w_q,file="results/muRes_w.txt", row.names=F, col.names=cnames, sep="\t")

                                        