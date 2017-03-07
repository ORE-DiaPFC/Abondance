#rm(list=ls())   # Clear memory


##-----------------------------INFO ----------------------------------##
# year <- 2015
# site <- "Nivelle"
# stade <- "tacon"


## WORKING DIRECTORY:
# work.dir<-paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance',site,stade,'data/',sep="/")
# setwd(work.dir)


##-----------------------------DATA ----------------------------------##
data.dir <- paste("data/data-",stade,"-",year,".txt",sep="")
data <- read.bugsdata(data.dir)


save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
