#rm(list=ls())   # Clear memory


##-----------------------------INFO ----------------------------------##
year <- "2019"
site <- "Bresle"
stade <- "smolt"


## WORKING DIRECTORY:
#work.dir<-paste("/home/mbuoro/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abondance",site,stade,sep="/")
#setwd(work.dir)



##-----------------------------DATA ----------------------------------##
list <- read.bugsdata(paste("data/data_list.txt",sep="/"))

######################################################################################
##  C_B[]: Annual number of smolt captured at Beauchamps
##  D_B[]: Annual number of dead smolt at Beauchamps
##  Cm_B[t]: Annual number of smolt marked and released downstream Beauchamps
##  Cum_B[t]: Annual number of smolt captured at Beauchamps and released unmarked downstream
##  Cm_Eu[t]: Annual number of marked smolt captured at Eu
##  Cum_Eu[t]: Annual number of unmarked smolt captured at Eu
###################################################################################### 
smolt <- read.table(paste("data/","data_smolt.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
smolt <- as.matrix(smolt);mode(smolt)<- "numeric"
C_B=smolt[,1]
D_B=smolt[,2]
Cm_B=smolt[,3]
Cum_B=smolt[,4]
Cm_Eu=smolt[,5]
Cum_Eu=smolt[,6]

#### D?bit pour Eu (l/s). Moyenne sur la p?riode 1 avril - 10 mai
#### Ne sont indiqu?s que les ann?es o? il y a eu du pi?geage (n=20 years)
#### Covariate is standardized within WinBUGS
flow <- read.bugsdata(paste("data/","data_flow_Eu.txt",sep=""))
Q_Eu=flow$Q_Eu


data <- list(
  Nyears=list$Nyears, NBeau=list$NBeau,NEu=list$NEu
  , C_B=C_B,D_B=D_B, Cm_B=Cm_B,Cum_B=Cum_B,Cm_Eu=Cm_Eu, Cum_Eu=Cum_Eu
  , Q_Eu=Q_Eu
)

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
