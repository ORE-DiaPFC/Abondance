### Ce script est l? pour analyser les r?sultats des fichiers CODA obtenus avec le mod?le TACON de la Nivelle
### Besoin pour cela d'avoir enregistrer les fichiers CODA des 3 chaines MCMC.
### Permet d'obtenir des fichiers textes avec les estimations finales d'abondance
### /!\ Il faut mettre ? jour l'index de la derni?re ann?e (Y_last) et l'index du dernier site (I)
## /!\ Ce fichier n?cessite d'?crire 3 lignes de code en plus pour finir la mise ? jour (voir L.309-311)

##------------------ R PACKAGES ------------------------------##
library(coda)

##-----------------------------DATA ----------------------------------##
year <- 2023
site <- "Scorff"
stade <- "adult"


years <- 1993:year

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="")
#setwd(work.dir)

# data.dir <- paste("data/","data-",stade,"-",year,".txt",sep="")
# data <- read.bugsdata(data.dir)
load(paste('data/data_',stade,"_",year,'.Rdata',sep="")) # chargement des données
  
# loading coda
load(paste('results/Results_',stade,"_",year,'.RData',sep=""))


# Save returns estimates in csv file
res <- fit$summary
p_male_1SW <- res[paste0("p_male[",1:data$Y,",1]"),]
rownames(p_male_1SW) <- 1994:year
write.csv(p_male_1SW, file="results/Scorff_adults_pMale_1SW.csv")

#res <- fit$summary
p_male_MSW <- res[paste0("p_male[",1:data$Y,",2]"),]
rownames(p_male_MSW) <- 1994:year
write.csv(p_male_MSW, file="results/Scorff_adults_pMale_MSW.csv")


save(p_male_1SW, p_male_MSW, file="p_male_Scor.Rdata")