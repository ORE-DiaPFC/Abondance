##-----------------------------INFO ----------------------------------##
#year <- "2020"
#site <- "Oir"
#stade <- "adult"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)


##-----------------------------DATA ----------------------------------##
## DATA DIRECTORY:
Nyears <- read.bugsdata(paste("data/","list-data.txt",sep=""))

#########################################################################
# C_MC[t,g]: Annual number of fish captured at the trap (Moulin Cerisel) per category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW             
# Cm_MC[t,g]: Annual number of fish marked and released at the trap (Moulin Cerisel) per category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW 
#########################################################################
mark <- read.table(paste("data/capt-MC.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
mark <- as.matrix(mark);mode(mark)<- "numeric"
C_MC <- mark[,1:4]
Cm_MC <- mark[,5:8]


#########################################################################
# Cm_R[t,g]: Annual number of mark-recaptured fish per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW             
# Cum_R[t,g]: Annual number of unmark-recaptured fish (Nr-Nrm) per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW 
#########################################################################
mark <- read.table(paste("data/capt-R.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
mark <- as.matrix(mark);mode(mark)<- "numeric"
Cm_R <- mark[,1:4]
Cum_R <- mark[,5:8]


### Flow data (l/s) are from 15 october to 31 december 
##### Covariate is standardized within WinBUGS
Q <- read.bugsdata(paste("data/data-flow.txt",sep=""))



data <- list( Nyears=Nyears$Nyears
              ,Cm_R=Cm_R,Cum_R=Cum_R
              ,Q=Q$Q
              ,C_MC=C_MC, Cm_MC=Cm_MC
)

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep="")) # sauvegarde des données
