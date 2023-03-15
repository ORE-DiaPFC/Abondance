rm(list=ls())   # Clear memory



##------------------ R PACKAGES ------------------------------##
library(R2OpenBUGS)
library(rjags) # require to use "read.bugsdata" function
library(coda)
library(mcmcplots)
# library(dclone)
# library(snow)
# require(ggmcmc)


##-----------------------------INFO ----------------------------------##
year <- "2022"
site <- "Bresle"
stade <- "adult"


## WORKING DIRECTORY:
work.dir<-paste("/media/hdd4To/mbuoro/ORE-DiaPFC/Abundance",site,stade,sep="/")
setwd(work.dir)

# cleaning
system("mkdir bugs/")
#system("rm bugs/*")


##-----------------------------DATA ----------------------------------##
source(paste('data/data_',stade,'_TMP.R',sep="")) # creation du fichier Rdata
load(paste('data/data_',stade,"_",year,'.Rdata',sep="")) # chargement des données


#----------------------------PARAMETERS---------------------------------##
source(paste('parameters_',stade,'.R',sep="")) # chargement des paramètres


#------------------------INITS----------------------------------##
#if(!file.exists(paste('inits/inits_',stade,year,'.Rdata',sep=""))){
#if(!file.exists(paste("inits/init-",site,"-",stade,year,".txt",sep=""))){
#inits=list()
#for (c in 1:2){
#  inits.tmp=NULL
source(paste('inits/inits_',stade,'.R',sep="")) # création des inits des données
#  inits[[c]]<-inits.tmp
#}
#load(paste('inits/inits_',stade,year,'.Rdata',sep=""))
#}
#load(paste('inits/inits_',stade,'.Rdata',sep="")) # chargement des inits
#if(site == "Bresle" && stade == "adult") {inits <- list(read.bugsdata(paste("inits/init-",site,"-",stade,year,".txt",sep="")))}
#if(site == "Nivelle") {inits <- list(read.bugsdata(paste("inits/init-",site,"-",stade,year,".txt",sep="")))}
#for (c in 1:2){
inits.tmp1 <- read.bugsdata(paste("inits/init-",site,"-",stade,year,"_",1,".txt",sep=""))
inits.tmp2 <- read.bugsdata(paste("inits/init-",site,"-",stade,year,"_",2,".txt",sep=""))
#inits <- rep(list(inits.tmp),2)
inits <- list(inits.tmp1,inits.tmp2)

#------------------------MODEL----------------------------------##
model <- paste("model/model_",stade,"-",site,".R",sep="") # path of the model
#if(site == "Scorff" && stade == "smolt") {model <- paste("model/model_",stade,"-",site,"_",year,"_age.R",sep="")} # le modèle Scorrf pour les smolt peut changer tous les ans suivant conditions
model

filename <- file.path(work.dir, model)
#system(paste("cp",model,paste(stade,"-",site,".txt",sep=""),sep=""))


#---------------------------ANALYSIS-----------------------------##
nChains = 2 #length(inits) # Number of chains to run.
adaptSteps = 1000 # Number of steps to "tune" the samplers.
nburnin=1000 # Number of steps to "burn-in" the samplers.
nstore=10000 # Total number of steps in chains to save.
nthin=300 # Number of steps to "thin" (1=keep every step).
#nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

analysis=FALSE

if(analysis){
### Start of the run ###
start.time = Sys.time(); cat("Start of the run\n"); 

######### BUGS ##########
fit <- bugs(
  data
  ,inits
  ,model.file = filename
  ,parameters
  ,n.chains = nChains, n.iter = nstore + nburnin, n.burnin = nburnin, n.thin = nthin
  ,DIC=FALSE
  ,codaPkg = FALSE, clearWD=FALSE
  ,saveExec=TRUE
  #,restart=TRUE
  #,debug=TRUE
  ,working.directory=paste(work.dir,"bugs",sep="/")
  # If Macos:
  #, OpenBUGS.pgm = "/Users/mbuoro/.wine/drive_c/Program Files/OpenBUGS/OpenBUGS323/OpenBUGS.exe"
  #, useWINE = TRUE
)

## cleaning
system("rm bugs/CODA*")

### Save inits ###
# save last values for inits
# inits <- fit$last.values
# if(site == "Nivelle") {
#   save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
#   }


######### JAGS ##########
## Compile & adapt
#Create, initialize, and adapt the model:
# fit <- jags.model(
#   model,
#   data,inits,
#   n.chains=nChains,
#   n.adapt = adaptSteps)

# # Run JAGS in parallel. Each Chain is sent to a seperate core.
# cl <- makeSOCKcluster(nChains)                       # Request 3 cores. /!\ Need to check how many core  your computer has
# fit.mcmc <- jags.parfit(cl,
#                         data,
#                         parameters,
#                         model.dir,
#                         inits,
#                         n.chains=nChains,n.adapt=adaptSteps,n.update=nburnin,n.iter=nstore*nthin, thin=nthin
# )
# stopCluster(cl) #### /!\ Really important to do!

# duration of the run 
end.time = Sys.time()
elapsed.time = difftime(end.time, start.time, units='mins')
cat("Sample analyzed after ", elapsed.time, ' minutes\n')



## BACKUP
save(fit,file=paste('results/Results_',stade,"_",year,'.RData',sep=""))

} else {
  load(paste0('results/Results_',stade,"_",year,'.RData'))
}


## Check if enough independent samples
test <-  any(fit$summary[,"n.eff"]<1000)
#try(if(iter > 10) stop("too many iterations"))

mydf <- as.matrix(round(fit$summary,3))
mydf <- cbind(rownames(mydf), mydf)
rownames(mydf) <- NULL
colnames(mydf)[1] <- c("Parameters")#, colnames(mydf))
write.table(mydf,file=paste('results/Results_',stade,"_",year,'.csv',sep=""),sep=",", row.names = FALSE)
     
#------------------------------------------------------------------------------
# EXAMINE THE RESULTS

## To check chains and distributions:
source("posterior_check.R")
# traplot(fit, "junk")
# denplot(fit, "junk")

source("diagnostics.R")



#------------------------------------------------------------------------------
## SUMMARY
if(site == "Scorff" && stade == "adult") {source("summary_adult.R")}
if(site == "Nivelle" && stade == "tacon") {source("analyse_coda_tacon.R")}
if(site == "Nivelle" && stade == "adult") {source("analyse_coda_adult.R")}


if(site == "Scorff"){
  dir<- c("/media/hdd4To/mbuoro/ORE-DiaPFC/Abundance/")
  setwd(dir)
  f1 <- paste0(dir,"Scorff/tacon/results/Results_tacon","_",year,".RData")
  f2 <- paste0(dir,"Scorff/smolt/results/Results_smolt","_",year,".RData")
  f3 <- paste0(dir,"Scorff/adult/results/Results_adult","_",year,".RData")
  if (file.exists(f1)&&file.exists(f2)&&file.exists(f3)){
    # source(paste0(dir,"Scorff/script_bilan.R"))
    source(knitr::purl(paste0(dir,"/",site,"/Bilan_",site,".Rmd"), quiet=TRUE))
  }}
# if(site == "Scorff"){
# setwd("/media/hdd4To/mbuoro/ORE-DiaPFC/Abundance")
# f1 <- paste0("Scorff/tacon/results/Results_tacon","_",year,".RData")
# f2 <- paste0("Scorff/smolt/results/Results_smolt","_",year,".RData")
# f3 <- paste0("Scorff/adult/results/Results_adult","_",year,".RData")
# if (file.exists(f1)&&file.exists(f2)&&file.exists(f3)){
#   source("script_bilan.R")
# }}
