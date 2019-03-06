#rm(list=ls())   # Clear memory
library(R2OpenBUGS)
library(rjags)
library(coda)

##-----------------------------INFO ----------------------------------##
year <- "2018"
site <- "Bresle"
stade <- "adult"


## WORKING DIRECTORY:
#work.dir<-paste("/home/mbuoro/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abondance",site,stade,sep="/")
#setwd(work.dir)


##-----------------------------DATA ----------------------------------##
## DATA DIRECTORY:
list <- read.bugsdata(paste("data/data_list.txt",sep="/"))

######################################################################################
##  Cm_B[t,a]: Annual number of marked fish captured at Beauchamps per sea age category; 1: 1SW, 2: MSW    
##  Cum_B[t,a]: Annual number of unmarked fish captured at Beauchamps per sea age category; 1: 1SW, 2: MSW 
###################################################################################### 
fish <- read.table(paste("data/","data_Beauchamps.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
fish <- as.matrix(fish);mode(fish)<- "numeric"
Cm_B=as.matrix(fish[,1:2])
Cum_B=as.matrix(fish[,3:4])

#####################################################################################################
##  C_Eu[t,a]: Annual number of fish captured at Eu per sea age category; 1:1SW, 2:MSW        
##  Cm_Eu[t,a]: Annual number of fish captured and marked at Eu per sea age category; 1:1SW, 2:MSW
#####################################################################################################    
Eu <- read.table(paste("data/","data_Eu.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Eu <- as.matrix(Eu);mode(Eu)<- "numeric"
C_Eu=as.matrix(Eu[,1:2])
Cm_Eu=as.matrix(Eu[,3:4])

######################################################################################################################################
## Mean flow (l/s): - 15 june - 31 august for 1SW ([,1])    
## - 15 april - 30 june for MSW  ([,2])    
## Covariate is standardized within WinBUGS
###################################################################################################################################### 
Q <- read.table(paste("data/","data_flow_sea-age.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Q <- as.matrix(Q);mode(Q)<- "numeric"



data <- list(
  Y=list$Y, Q2pic=list$Q2pic
  , Cm_B=Cm_B,Cum_B=Cum_B,C_Eu=C_Eu, Cm_Eu=Cm_Eu
  , Q=Q
)

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep="")) # sauvegarde des données
bugs.data(data,digits=3, data.file = paste0('data/data_',stade,"_",year,'.txt'))

