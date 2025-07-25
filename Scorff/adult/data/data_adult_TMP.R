##-----------------------------INFO ----------------------------------##
year <- "2024"
site <- "Scorff"
stade <- "adult"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)



##-----------------------------DATA ----------------------------------##
## DATA DIRECTORY:
Y <- read.bugsdata(paste("data/","data_list.txt",sep=""))


## Number of nights when recaptures on spawning ground are done (whether some salmons are captured or not). 
## Covariate is standardized within WinBUGS
## In 94 and 95, it's the number of nights for which some fishes are recaptured (not the total number of nights)
eff_R <- read.bugsdata(paste("data/","data_effort_R.txt",sep=""))
if(length(eff_R)!=Y) warnings("data missing")


######################################################################################################################################
##  Cm_R[t,a]: Annual number of marked fish caught during or after reproduction per sea age category; 1: 1SW, 2: MSW
##  Cum_R[t,a]: Annual number of unmarked fish caught during or after reproduction per sea age category; 1: 1SW, 2: MSW
######################################################################################################################################
mark <- read.table(paste("data/","data_R.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
mark <- as.matrix(mark);mode(mark)<- "numeric"
Cm_R <- mark[,1:2]
Cum_R <- mark[,3:4]

######################################################################################################################################			
##  Cm_R_pulsium[t,a]: Annual number of marked fish caught by electric fishing during reproduction per sea age category; 1: 1SW, 2: MSW			
##  Cum_R_pulsium[t,a]: Annual number of unmarked fish caught by electric fishing during reproduction per sea age category; 1: 1SW, 2: MSW			
######################################################################################################################################	
mark_pulsium <- read.table(paste("data/","data_Rpulsium.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
mark_pulsium <- as.matrix(mark_pulsium);mode(mark_pulsium)<- "numeric"
Cm_R_pulsium <- mark_pulsium[,1:2]
Cum_R_pulsium <- mark_pulsium[,3:4]

######################################################################################################################################
## Mean flow (m3/s): 1: 15 june - 15 august for 1SW
## 2: 1 avril - 1 june for MSW
## Covariate is standardized within WinBUGS
######################################################################################################################################  
Q <- read.table(paste("data/","data_Qsea-age_avril.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Q <- as.matrix(Q);mode(Q)<- "numeric"

## Mean flow 24november- 24december (m3/s)
## Covariate is standardized within WinBUGS
Q_dec <- read.bugsdata(paste("data/","data_Q_dec.txt",sep=""))


######################################################################################################################################
##  C_MP[t,a]: Annual number of fish captured at Moulin des Princes per sea age category; 1:1SW, 2:MSW
##  Cum_MP[t,a]: Annual number of fish captured at Moulin des Princes and released unmarked per sea age category (happened only once in 1995 for 1SW); 1:1SW, 2:MSW
##  Cm_MP[t,a]: Annual number of fish captured and marked at Moulin des Princes per sea age category; 1:1SW, 2:MSW
######################################################################################################################################
capt <- read.table(paste("data/","data_oD.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
capt <- as.matrix(capt);mode(capt)<- "numeric"
Cm_D <- capt[,1:2]
Cum_D <- capt[,3:4]

######################################################################################################################################
##  C_MP[t,a]: Annual number of fish captured at Moulin des Princes per sea age category; 1:1SW, 2:MSW
##  Cum_MP[t,a]: Annual number of fish captured at Moulin des Princes and released unmarked per sea age category (happened only once in 1995 for 1SW); 1:1SW, 2:MSW
##  Cm_MP[t,a]: Annual number of fish captured and marked at Moulin des Princes per sea age category; 1:1SW, 2:MSW
######################################################################################################################################
capt <- read.table(paste("data/","data_MP.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
capt <- as.matrix(capt);mode(capt)<- "numeric"
C_MP <- capt[,1:2]
Cm_MP <- capt[,3:4]
Cum_MP <- capt[,5:6]

######################################################################################################################################
# THIS FILE DOES NOT NEED TO BE UPDATED
# C_F[t,a]: Annual number of fish caught by fishing per sea age category (from 1994 to 2002)
######################################################################################################################################
#C_F <- read.table(paste("data/","data_F-est94-02.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
C_F <- read.table(paste("data/","data_F-tot.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")

C_F <- as.matrix(C_F);mode(C_F)<- "numeric"

##############################################################################################################################
# THIS FILE DOES NOT NEED TO BE UPDATED
# Cuo_F[t,a]: Annual number of fish caught by fishing per sea age category (from 1994 to 2002) and notre observed at MP
######################################################################################################################################
#Cuo_F <- read.table(paste("data/","data_F-uo94-02.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
#Cuo_F <- as.matrix(Cuo_F);mode(Cuo_F)<- "numeric"

###############################################################################################################################
## Cm_F[t,a]: Annual number of marked fish caught by fishing per sea age category and showed at Moulin des Princes. 1:1SW, 2:MSW  
## Cum_F[t,a]: Annual number of unmarked fish caught by fishing per sea age category and showed at Moulin des Princes. 1:1SW, 2:MSW
###############################################################################################################################   
mark <- read.table(paste("data/","data_F.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
mark <- as.matrix(mark);mode(mark)<- "numeric"
Cm_F <- mark[,1:2]
Cum_F <- mark[,3:4]



###################################################			
##  n_smp : Annual number of fish sampled by sea age category and sexed ; 1: 1SW-Male, 2: 1SW-Female, 3: MSW-Male, 4: MSW-Female	
###################################################################################
n_sex_smp <- read.csv("data/data_sex.csv",header = TRUE)
n_sex_smp <- as.matrix(n_sex_smp[,2:5])



data <- list( Y=Y$Y
              ,eff_R=eff_R$eff_R
              ,eff_R_pulsium=eff_R$eff_R_pulsium
              ,Cm_R=Cm_R,Cum_R=Cum_R
              ,Cm_R_pulsium=Cm_R_pulsium,Cum_R_pulsium=Cum_R_pulsium
              ,Q=Q,Q_dec=Q_dec$Q_dec
              ,Cm_D=Cm_D,Cum_D=Cum_D
              ,C_MP=C_MP,Cm_MP=Cm_MP,Cum_MP=Cum_MP
              ,C_F=C_F,Cm_F=Cm_F,Cum_F=Cum_F
              ,n_sex_smp=n_sex_smp
              #,Cuo_F=Cuo_F
              )

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep="")) # sauvegarde des données