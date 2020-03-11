##-----------------------------INFO ----------------------------------##
year <- "2019"
site <- "Scorff"
stade <- "smolt"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)



##-----------------------------DATA ----------------------------------##
fish <- read.bugsdata(paste("data/data_",stade,".txt",sep=""))

#######################################################################################
# Mean flow observed from 1st April to May 10 (based on days when trap is working) 
# for 1: Moulin des Princes 
# and 2: Moulin de Lesl?
#######################################################################################
Q <- read.table(paste("data/data_flow.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Q <- as.matrix(Q);mode(Q)<- "numeric"


data <- c(fish,Q=list(Q))

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
