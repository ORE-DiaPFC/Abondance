rm(list=ls())   # Clear memory


year <- "2016"

######## Table 7 - Exploitation rate in the rivers Scorff ######

## SCORFF
site <- "Scorff"
stade <- "adult"

# load dataset
load(paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Scorff/adult/data/data_',stade,"_",year,'.Rdata',sep="")) # chargement des données

## Cm_F[t,a]: Annual number of marked fish caught by fishing per sea age category and showed at Moulin des Princes. 1:1SW, 2:MSW  
## Cum_F[t,a]: Annual number of unmarked fish caught by fishing per sea age category and showed at Moulin des Princes. 1:1SW, 2:MSW
C_F_1SW <- data$Cm_F[,1] + data$Cum_F[,1] # marked + unmarked 1SW fish caugth by fishing
C_F_MSW <- data$Cm_F[,2] + data$Cum_F[,2] # marked + unmarked MSW fish caugth by fishing

# load estimations of size popualtions
load("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Scorff/adult/results/Results_adult_2016.RData")
n_1SW <- fit$median$n_1SW # medians
n_MSW <- fit$median$n_MSW # medians

Expl_rate <- cbind(
  Expl_rate_1SW = (C_F_1SW / n_1SW)*100,
  Expl_rate_MSW = (C_F_MSW / n_MSW)*100  
    )

Expl_rate <- rbind(Expl_rate,colMeans(Expl_rate))

rownames(Expl_rate) <- c(seq(1994,2016,1), "Average")
colnames(Expl_rate) <- c("1SW (%)", "MSW (%)")

write.csv(round(Expl_rate,1), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table7_Scorff_',year,'.csv',sep=""))


######## Table 8 - Index rivers :spawning stock, egg deposition and attainment of CLs ######


## NIVELLE
site <- "Nivelle"
stade <- "adult"

load(paste("~/Documents/RESEARCH/PROJECTS/ORE/Abundance/Nivelle/adult/results/Results_adult_",year,".RData",sep=""))

years <- seq(1987, 2016, 1)
table <- array(, dim=c(length(years), 4))
colnames(table) <- c("1SW",	"MSW",		"eggs (million)",	"eggs/CL")
rownames(table) <- years

#Conservation Limit:
CL = 1.44




######## Table 9 - juvenile and adult salmon  numbers (estim.), in-river return rate in the monitored rivers ######


