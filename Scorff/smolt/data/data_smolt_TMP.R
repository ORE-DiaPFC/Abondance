##-----------------------------INFO ----------------------------------##
year <- "2023"
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


###################################################			
##  n_smp : Annual number of fish sampled by sea age category and sexed ; 1: 1SW-Male, 2: 1SW-Female, 3: MSW-Male, 4: MSW-Female	
###################################################################################
n_sex_smp <- read.csv("data/data_smolt_sex.csv",header = TRUE); colnames(n_sex_smp)<-NULL
n_sex_smp <- as.matrix(n_sex_smp[,2:5])

n_sex_smp[26,] <- 1
n_sex_smp[29,3] <- 1

data <- list(Nyears=Nyears
          #,fish=fish
          #,Q=list(Q)
          ,stlogQ=stlogQ
          ,n=age$n,n1=age$n1
          ,n_sex_smp=n_sex_smp
          )
data <- c(data, fish)

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
