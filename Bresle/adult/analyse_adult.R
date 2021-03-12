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
year <- "2020"
site <- "Bresle"
stade <- "adult"


## WORKING DIRECTORY:
work.dir<-paste("/media/hdd/mbuoro/ORE-DiaPFC/Abundance",site,stade,sep="/")
setwd(work.dir)

# cleaning
system("mkdir bugs/")
system("rm bugs/*")


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
if(site == "Scorff" && stade == "smolt") {model <- paste("model/model_",stade,"-",site,"_",year,".R",sep="")} # le modèle Scorrf pour les smolt peut changer tous les ans suivant conditions
model

filename <- file.path(work.dir, model)
#system(paste("cp",model,paste(stade,"-",site,".txt",sep=""),sep=""))


#---------------------------ANALYSIS-----------------------------##
nChains = 2 #length(inits) # Number of chains to run.
adaptSteps = 1000 # Number of steps to "tune" the samplers.
nburnin=100 # Number of steps to "burn-in" the samplers.
nstore=20000 # Total number of steps in chains to save.
nthin=100 # Number of steps to "thin" (1=keep every step).
#nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

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
  #,debug=TRUE
  #,working.directory=paste(work.dir,"bugs",sep="/")
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

mydf <- as.matrix(fit$summary)
mydf <- cbind(rownames(mydf), mydf)
rownames(mydf) <- NULL
colnames(mydf)[1] <- c("Parameters")#, colnames(mydf))
write.table(mydf,file=paste('results/Results_',stade,"_",year,'.csv',sep=""),sep=",", row.names = FALSE)
     
#------------------------------------------------------------------------------
# EXAMINE THE RESULTS
fit.mcmc <- as.mcmc(fit) # using bugs



## To check chains and distributions:
source("posterior_check.R")
# traplot(fit, "junk")
# denplot(fit, "junk")




# DIAGNOSTICS:
parameterstotest <- hyperparameters # all parameters
# parameterstotest <- c(
#   "epsilon_p"
# )

# Start writing to an output file
sink(paste('results/Diagnostics_',stade,"_",year,'.txt',sep=""))

cat("=============================\n")
cat("DIAGNOSTICS\n")
cat("=============================\n")

cat("Number of chains: ", fit$n.chains,"\n")
cat("Number of iterations: ", fit$n.keep,"\n")

if (nChains > 1) {
  cat("Convergence: gelman-Rubin R test\n")
  #gelman.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)],multivariate=TRUE)
  gelman.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)],multivariate=TRUE)
  test <- gelman.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)],multivariate=TRUE)
  
}
cat("Approximate convergence is diagnosed when the upper limit is close to 1 and <1.1 \n")


cat("\n---------------------------\n")
cat("Heidelberger and Welch's convergence diagnostic\n")
cat("
heidel.diag is a run length control diagnostic based on a criterion of relative accuracy for the estimate of the mean. The default setting corresponds to a relative accuracy of two significant digits.

heidel.diag also implements a convergence diagnostic, and removes up to half the chain in order to ensure that the means are estimated from a chain that has converged.
\n")
#heidel.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], eps=0.1, pvalue=0.05)
heidel.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], eps=0.1, pvalue=0.05)

cat("\n---------------------------\n")
cat("Geweke's convergence diagnostic\n")
cat("
Geweke (1992) proposed a convergence diagnostic for Markov chains based on a test for equality of the means of the first and last part of a Markov chain (by default the first 10% and the last 50%).
If the samples are drawn from the stationary distribution of the chain, the two means are equal and Geweke's statistic has an asymptotically standard normal distribution. 
The test statistic is a standard Z-score: the difference between the two sample means divided by its estimated standard error. The standard error is estimated from the spectral density at zero and so takes into account any autocorrelation.

The Z-score is calculated under the assumption that the two parts of the chain are asymptotically independent, which requires that the sum of frac1 and frac2 be strictly less than 1.
\n")
#geweke.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], frac1 = 0.1, frac2 = 0.5)
geweke.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], frac1 = 0.1, frac2 = 0.5)

cat("\n---------------------------\n")
cat("Raftery and Lewis's diagnostic\n")
raftery.diag(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)], q=0.025, r=0.005, s=0.95, converge.eps=0.001)

# Stop writing to the file
sink()


## Plot the chains:
#pdf(paste('results/Results_',stade,"_",year,'.pdf',sep=""))

# if(site == "Bresle" && stade == "smolt") {
# parameters.trend <- c("Ntot","Nesc","lambda","p_B","p_Btot","epsilon_B","p_Eu","epsilon_Eu")
# }
# if(site == "Bresle" && stade == "adult") {
#   parameters.trend <- c("n_tot","n_1SW","n_MSW","pi_B","lambda_tot","Plambda","pi_Eu","epsilon_Eu")
# }
# if(site == "Oir" && stade == "adult") {
#   parameters.trend <- c("n_tot","n_1SW","n_MSW","n","Nesc_1SW","Nesc_MSW","Nesc_tot","pi_MC","lambda_n","lambda","p_recap","epsilon_MC")
# }
# if(site == "Oir" && stade == "smolt") {
#   parameters.trend <- c("Ntot","Nesc","p_MC","lambda","alpha_MC","beta_MC","overdisp_MC","mean_MC")
# }

# for (i in 1:length(parameters.trend)){
#   caterplot(fit.mcmc,parameters.trend[i], reorder = FALSE, horizontal=FALSE, style=c("plain")) 
# }

#caterplot(fit.mcmc,parameterstotest, reorder = FALSE, horizontal=FALSE, style=c("plain")) 

#traplot(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)])

#gelman.plot(fit.mcmc[,which(varnames(fit.mcmc)%in%parameterstotest)])

#for (par in hyperparameters){
#  traplot(fit.mcmc,par) 
#  denplot(fit.mcmc,par) 
#}
#dev.off()


#------------------------------------------------------------------------------
## SUMMARY
if(site == "Scorff" && stade == "adult") {source("summary_adult.R")}
if(site == "Nivelle" && stade == "tacon") {source("analyse_coda_tacon.R")}

if(site == "Scorff"){
  dir<- c("/media/hdd/mbuoro/ORE-DiaPFC/Abundance/")
  setwd(dir)
  f1 <- paste0(dir,"Scorff/tacon/results/Results_tacon","_",year,".RData")
  f2 <- paste0(dir,"Scorff/smolt/results/Results_smolt","_",year,".RData")
  f3 <- paste0(dir,"Scorff/adult/results/Results_adult","_",year,".RData")
  if (file.exists(f1)&&file.exists(f2)&&file.exists(f3)){
    # source(paste0(dir,"Scorff/script_bilan.R"))
    source(knitr::purl(paste0(dir,"/",site,"/Bilan_",site,".Rmd"), quiet=TRUE))
  }}
# if(site == "Scorff"){
# setwd("/media/hdd/mbuoro/ORE-DiaPFC/Abundance")
# f1 <- paste0("Scorff/tacon/results/Results_tacon","_",year,".RData")
# f2 <- paste0("Scorff/smolt/results/Results_smolt","_",year,".RData")
# f3 <- paste0("Scorff/adult/results/Results_adult","_",year,".RData")
# if (file.exists(f1)&&file.exists(f2)&&file.exists(f3)){
#   source("script_bilan.R")
# }}
