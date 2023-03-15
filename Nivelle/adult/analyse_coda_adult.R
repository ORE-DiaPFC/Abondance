### Ce script est l? pour analyser les r?sultats des fichiers CODA obtenus avec le mod?le TACON de la Nivelle
### Besoin pour cela d'avoir enregistrer les fichiers CODA des 3 chaines MCMC.
### Permet d'obtenir des fichiers textes avec les estimations finales d'abondance
### /!\ Il faut mettre ? jour l'index de la derni?re ann?e (Y_last) et l'index du dernier site (I)
## /!\ Ce fichier n?cessite d'?crire 3 lignes de code en plus pour finir la mise ? jour (voir L.309-311)

##------------------ R PACKAGES ------------------------------##
library(coda)

##-----------------------------DATA ----------------------------------##
year <- 2022
site <- "Nivelle"
stade <- "adult"


years <- 1984:year

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="")
#setwd(work.dir)

# data.dir <- paste("data/","data-",stade,"-",year,".txt",sep="")
# data <- read.bugsdata(data.dir)
#load(paste('data/data_',stade,"_",year,'.Rdata',sep="")) # chargement des données
  
# loading coda
#load(paste('results/Results_',stade,"_",year,'.RData',sep=""))

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
#as.mcmc.bugs <- function(x.bugs) as.mcmc(x.bugs$sims.matrix)

#fit.mcmc <- as.mcmc(fit) # using jags
#fit.mcmc <- as.mcmc.bugs(fit) # using bugs


### /!\ MISE A JOUR
# Index de la derni?re ann?e 
#Y_last <- data$Y_last
# Index du dernier site
#I <- data$I
# Total number of iter
#n_iter <- dim(fit.mcmc)[1]   # fit$n.iter # 10 000 iterations * 3 chaines MCMC


#"eggs_tot" : annual number of eggs produced     
eggs_tot <- fit.mcmc$sims.matrix[,paste0("eggs_tot[",1:data$Y,"]")]

write.table(eggs_tot,file="results/eggs_tot_iter.txt", row.names=F, col.names=c(paste0(years)), sep = "\t") #mb-21.03.2022


                                   