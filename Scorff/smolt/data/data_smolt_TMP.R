##-----------------------------INFO ----------------------------------##
year <- "2022"
site <- "Scorff"
stade <- "smolt"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)

Nyears = length(1995:year)

##-----------------------------DATA ----------------------------------##
fish <- read.bugsdata(paste("data/data_",stade,".txt",sep=""))

#######################################################################################
# Mean flow observed from 1st April to May 10 (based on days when trap is working) 
# for 1: Moulin des Princes 
# and 2: Moulin de Lesl?
#######################################################################################
Q <- read.table(paste("data/data_flow.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Q <- as.matrix(Q);mode(Q)<- "numeric"

############################################################################################
####### Standardization of flow covariates 
############################################################################################
logQ=stlogQ=array(,dim=dim(Q))
# Standardization of flow for MP
#for (t in 1:Nyears) {
  logQ[,1] <- log(Q[,1]) # ln transformation of covariate
  stlogQ[,1] <- (logQ[,1] - mean(logQ[,1]))/sd(logQ[,1]) # standardized covariate 
#} # end of loop over years

#### Standardization of covariates (flow) for ML
#for (t in 3:Nyears) {
  logQ[3:Nyears,2] <- log(Q[3:Nyears,2]) # ln transformation of covariate
  stlogQ[3:Nyears,2] <- (logQ[3:Nyears,2] - mean(logQ[3:Nyears,2]))/sd(logQ[3:Nyears,2]) # standardized covariate  
#} #end of loop over years

  
## Age of smolt
age <- read.bugsdata(paste("data/data_age.txt",sep=""))

data <- c(Nyears=Nyears
          ,fish
          #,Q=list(Q)
          ,stlogQ=list(stlogQ)
          ,age
)

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
