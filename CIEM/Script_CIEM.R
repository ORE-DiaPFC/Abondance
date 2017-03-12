rm(list=ls())   # Clear memory

# TO DO
# - Scorff: revoir les données de l année N-1 pour prendre en compte les bécards
# - Oir: vérifier les données adultes de l'année 1986' (différence capturés vs marqués NEGATIVE!!!)
# - Oir : demander les fecondités des 1SW et MSW à Fred
# - OIR: chercher les données de surface de production accessible aux spawners dans les données de juvéniles
# ' Bresle: vérifier tx fecondité et sex-ratio


year <- 2016




###################################################################################################################
######## Table 7 - Exploitation rate in the rivers Scorff ######
###################################################################################################################

## SCORFF
site <- "Scorff"
stade <- "adult"

# load dataset
load(paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Scorff/adult/data/data_',stade,"_",year,'.Rdata',sep="")) # chargement des données


## Cm_F[t,a]: Annual number of marked fish caught by fishing per sea age category and showed at Moulin des Princes. 1:1SW, 2:MSW  
## Cum_F[t,a]: Annual number of unmarked fish caught by fishing per sea age category and showed at Moulin des Princes. 1:1SW, 2:MSW
C_F_1SW <- data$Cm_F[,1] + data$Cum_F[,1] # marked + unmarked 1SW fish caugth by fishing
C_F_MSW <- data$Cm_F[,2] + data$Cum_F[,2] # marked + unmarked MSW fish caugth by fishing

# /!\ Annual number of fish caught by fishing per sea age category from 1994 to 2002 / Not all reported then
#data$C_F # 94 -> 2002
C_F_1SW[1:length(data$C_F[,1])] <- data$C_F[,1]
C_F_MSW[1:length(data$C_F[,2])] <- data$C_F[,2]

# load estimations of size popualtions
load("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Scorff/adult/results/Results_adult_2016.RData")
n_1SW <- fit$median$n_1SW # medians
n_MSW <- fit$median$n_MSW # medians

Expl_rate <- cbind(
  Expl_rate_1SW = (C_F_1SW / n_1SW)*100,
  Expl_rate_MSW = (C_F_MSW / n_MSW)*100  
    )

Expl_rate <- rbind(Expl_rate,colMeans(Expl_rate))

rownames(Expl_rate) <- c(seq(1994,year,1), "Average")
colnames(Expl_rate) <- c("1SW (%)", "MSW (%)")

write.csv(round(Expl_rate,1), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table7_Scorff_',year,'.csv',sep=""))


# con <- file("object.csv", open="wt")
# writeLines(paste("# this csv file was created by mycode.R on 
# 12/2/2005"), con)
# write.csv( object, con)
# close(con)



###################################################################################################################
######## Table 8 - Index rivers :spawning stock, egg deposition and attainment of CLs ######
###################################################################################################################

##________________________NIVELLE (starting in 1984)
site <- "Nivelle"
stade <- "adult"

load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Nivelle/adult/results/Results_adult_",year,".RData",sep=""))

years <- seq(1984, year, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c("1SW",	"MSW",		"eggs (million)",	"eggs/CL")
rownames(table) <- years

#Conservation Limit:
CL = 1.44

# Spawners:
table[,"1SW"] <- fit$median$e_1SW # spawners 1SW
table[,"MSW"] <- fit$median$e_MSW # spawners MSW

# Eggs
table[,"eggs (million)"] <- fit$median$eggs_tot / 1e6 # depose eggs
table[,"eggs/CL"] <- table[,"eggs (million)"] / CL

write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table8_',site,"_",year,'.csv',sep=""))


##________________________ SCORFF (starting in 1994)
site <- "Scorff"
stade <- "adult"
nyear <- length(seq(1994,year,1))

load("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Scorff/tacon/data/data_tacon_2016.Rdata") # DATA
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))

years <- seq(1984, year, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c("1SW",	"MSW",		"eggs (million)",	"eggs/CL")
rownames(table) <- years

# Spawners:
table[,"1SW"] <- c(rep(NA,10), fit$median$e_1SW) # spawners 1SW
table[,"MSW"] <- c(rep(NA,10),fit$median$e_MSW) # spawners MSW

# Eggs:
mcmc <- fit$sims.matrix
e_1SW.mcmc <- mcmc[,paste("e_1SW[",1:nyear,"]",sep="")]
e_MSW.mcmc <- mcmc[,paste("e_MSW[",1:nyear,"]",sep="")]

#Conservation Limit de taux de depose oeufs
CL = 300 # eggs/100m²


fec_1SW = 0.45 * 3485 # fecondité 1SW Female
fec_MSW = 0.80 * 5569 # fecondite MSW Female
eggs_tot.mcmc = e_1SW.mcmc * fec_1SW + e_MSW.mcmc * fec_MSW
eggs_tot <- apply(eggs_tot.mcmc,2,quantile, probs=0.5) #median

S_prod <- colSums(data$S_Sc) + 31 # surface de production juveniles accessible aux spawners / ajout de 31 pour tenir compte des affluents non proscptés
CL_eggs <- S_prod[2:(nyear+1)] * CL # /!\ data$S_Sc starts in 1993 instead of 1994

ratio_CL <- eggs_tot / CL_eggs
  
table[,"eggs (million)"] <- c(rep(NA,10),eggs_tot / 1e6) # depose eggs
table[,"eggs/CL"] <- c(rep(NA,10),ratio_CL)

write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table8_',site,"_",year,'.csv',sep=""))



##_______________________________ OIR (starting in 1984)
site <- "Oir"
stade <- "adult"
nyear <- length(seq(1984,year,1))

load("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Oir/adult/data/data_adult_2016.Rdata") # DATA
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))


# /!\ regarder difference capturés - marqués (data$C_MC - data$Cm_MC)-> nombre negatif en 1986!!!!
# retirer les individus non marqués des effectifs estimés n (n - (data$C_MC - data$Cm_MC))

years <- seq(1984, year, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c("1SW",	"MSW",		"eggs (million)",	"eggs/CL")
rownames(table) <- years

# Spawners:
table[,"1SW"] <- fit$median$Nesc_1SW # spawners 1SW
table[,"MSW"] <- fit$median$Nesc_MSW # spawners MSW

# Eggs:
mcmc <- fit$sims.matrix
e_1SW.mcmc <- mcmc[,paste("Nesc_1SW[",1:nyear,",2]",sep="")] # female only
e_MSW.mcmc <- mcmc[,paste("Nesc_MSW[",1:nyear,",4]",sep="")] # female only


#S_prod <- colSums(data$S_Sc) # surface de production juveniles accessible aux spawners 
#CL_eggs <- S_prod[2:(nyear+1)] * CL 

fec_1SW = 3485 # fecondité 1SW # A REVOIR avec Fred
fec_MSW = 5569 # fecondite MSW # A REVOIR avec Fred
eggs_tot.mcmc = e_1SW.mcmc * fec_1SW + e_MSW.mcmc * fec_MSW
eggs_tot <- apply(eggs_tot.mcmc,2,quantile, probs=0.5) #median

# calculs basés sur la conservation limit des tableaux CIEM!! A REFAIRE à partir des données MAIS
#/!\ chercher les données de surface d eproduction dans les données de juvéniles

#Conservation Limit de taux de depose oeufs
CL_eggs = 0.12 # issue des tableaux CIEM, A REVOIR!!!
ratio_CL <- (eggs_tot/1e6) / CL_eggs

table[,"eggs (million)"] <- eggs_tot / 1e6 # depose eggs
table[,"eggs/CL"] <- ratio_CL

write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table8_',site,"_",year,'.csv',sep=""))





##_______________________________ BRESLE (starting in 1984)
site <- "Bresle"
stade <- "adult"
nyear <- length(seq(1984,year,1))

load("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Bresle/adult/data/data_adult_2016.Rdata") # DATA
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))


# /!\ regarder difference capturés - marqués (data$C_MC - data$Cm_MC)-> nombre negatif en 1986!!!!
# retirer les individus non marqués des effectifs estimés n (n - (data$C_MC - data$Cm_MC))

years <- seq(1984, year, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c("1SW",	"MSW",		"eggs (million)",	"eggs/CL")
rownames(table) <- years

# Spawners:
table[,"1SW"] <- fit$median$n_1SW # spawners 1SW
table[,"MSW"] <- fit$median$n_MSW # spawners MSW

# Eggs:
mcmc <- fit$sims.matrix
e_1SW.mcmc <- mcmc[,paste("n_1SW[",1:nyear,"]",sep="")] # female only
e_MSW.mcmc <- mcmc[,paste("n_MSW[",1:nyear,"]",sep="")] # female only


#S_prod <- colSums(data$S_Sc) # surface de production juveniles accessible aux spawners 
#CL_eggs <- S_prod[2:(nyear+1)] * CL 

fec_1SW = 0.45 * 3485 # fecondité 1SW Female # A REVOIR
fec_MSW = 0.80 * 5569 # fecondite MSW Female # A REVOIR
eggs_tot.mcmc = e_1SW.mcmc * fec_1SW + e_MSW.mcmc * fec_MSW
eggs_tot <- apply(eggs_tot.mcmc,2,quantile, probs=0.5) #median

# calculs basés sur la conservation limit des tableaux CIEM!! A REFAIRE à partir des données MAIS
#/!\ chercher les données de surface d eproduction dans les données de juvéniles

#Conservation Limit de taux de depose oeufs
CL_eggs = 0.36 # issue des tableaux CIEM, A REVOIR!!!
ratio_CL <- (eggs_tot/1e6) / CL_eggs

table[,"eggs (million)"] <- eggs_tot / 1e6 # depose eggs
table[,"eggs/CL"] <- ratio_CL

write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table8_',site,"_",year,'.csv',sep=""))





###################################################################################################################
######## Table 9 - juvenile and adult salmon  numbers (estim.), in-river return rate in the monitored rivers ######
###################################################################################################################

# Nota : juvenile fish are smolts, except in r. Nivelle (parrs O+). Adult numbers refer to the smolt year N: runs of N+1 and N+2 


##________________________NIVELLE (starting in 1984)
site <- "Nivelle"

years <- seq(1984, year, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c( "smolt year","0+ parr", 	"adults",	"survival rate (%)")
rownames(table) <- years
smolt.years <- years+1
table[,1] <- smolt.years
  
## JUVENILES
stade <- "tacon"
#load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Nivelle/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
YOY_tot_q <- read.table(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Nivelle/",stade,"/results/YOY_tot.txt",sep=""), header = TRUE)
table[,2] <- YOY_tot_q[,"q0.5"]

## ADULTS
stade <- "adult"
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Nivelle/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
n_1SW <- fit$median$n_1SW # spawners 1SW
n_MSW <- fit$median$n_MSW # spawners MSW

for (y in 1:(nrow(table))){
table[y,3] <- n_1SW[y+2] + n_MSW[y+3] # Parr 0+ become 1SW 2 years later / MSW 3 years later; /!\ NA reported if one of the two is missing!!!
}

## SURVIVAL
table[,4] <- (table[,3] / table[,2])*100

write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table9_',site,"_",year,'.csv',sep=""), row.names = FALSE)



##________________________OIR (starting in 1984)
site <- "Oir"

years <- seq(1984, year, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c( "smolt year","smolt", 	"adults",	"survival rate (%)")
rownames(table) <- years
smolt.years <- years
table[,1] <- smolt.years

## JUVENILES
stade <- "smolt"
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
table[,2] <- c(NA,NA,fit$median$Nesc) # capture of smolts started in 1986

## ADULTS
stade <- "adult"
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
n_1SW <- fit$median$Nesc_1SW # spawners 1SW
n_MSW <- fit$median$Nesc_MSW # spawners MSW

for (y in 1:(nrow(table))){
  table[y,3] <- n_1SW[y+1] + n_MSW[y+2] # Smolt 1+ become 1SW 1 years later / MSW 2 years later; /!\ NA reported if one of the two is missing!!!
}

## SURVIVAL
table[,4] <- (table[,3] / table[,2])*100 

write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table9_',site,"_",year,'.csv',sep=""), row.names = FALSE)


##________________________BRESLE (starting in 1984)
site <- "Bresle"

years <- seq(1982, year, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c( "smolt year","smolt", 	"adults",	"survival rate (%)")
rownames(table) <- years
smolt.years <- years
table[,1] <- smolt.years

## JUVENILES (1982 to now ; # /!\ NO CAPTURE IN 1988 to 1991 & 2001)
stade <- "smolt"
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
Nesc <- fit$median$Nesc # escapement from river
table[1:6,2] <- Nesc[1:6] # 1982 to 1987
table[11:19,2] <- Nesc[7:15] # 1992 to 2000
table[21:nrow(table),2] <- Nesc[16:length(Nesc)] # 2002 to now


## ADULTS (1984 to now)
stade <- "adult"
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
n_1SW <- c(NA,NA,fit$median$n_1SW) # spawners 1SW
n_MSW <- c(NA,NA,fit$median$n_MSW) # spawners MSW

for (y in 1:(nrow(table))){
  table[y,3] <- n_1SW[y+1] + n_MSW[y+2] # Smolt 1+ become 1SW 1 years later / MSW 2 years later; /!\ NA reported if one of the two is missing!!!
}

## SURVIVAL
table[,4] <- (table[,3] / table[,2])*100 

write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table9_',site,"_",year,'.csv',sep=""), row.names = FALSE)



##________________________SCORFF (starting in 1994)
site <- "Scorff"

years <- seq(1984, year, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c( "smolt year","smolt", 	"adults",	"survival rate (%)")
rownames(table) <- years
smolt.years <- years
table[,1] <- smolt.years

## JUVENILES from 1995 to now on
stade <- "smolt"
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
Nesc <- fit$median$Nesc # escapement from river
table[12:nrow(table),2] <- Nesc # 1995 to now



## ADULTS (1984 to now)
stade <- "adult"
load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
n_1SW <- c(rep(NA,10), fit$median$e_1SW) # spawners 1SW
n_MSW <- c(rep(NA,10),fit$median$e_MSW) # spawners MSW

for (y in 1:(nrow(table))){
  table[y,3] <- n_1SW[y+1] + n_MSW[y+2] # Smolt 1+ become 1SW 1 years later / MSW 2 years later; /!\ NA reported if one of the two is missing!!!
}

## SURVIVAL
table[,4] <- (table[,3] / table[,2])*100 

write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table9_',site,"_",year,'.csv',sep=""), row.names = FALSE)
