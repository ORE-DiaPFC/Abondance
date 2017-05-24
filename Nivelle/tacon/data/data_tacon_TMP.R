##-----------------------------INFO ----------------------------------##
year <- "2016"
site <- "Nivelle"
stade <- "tacon"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)



##-----------------------------DATA ----------------------------------##
data.dir <- paste("data/data-",stade,"-",year,".txt",sep="")
data <- read.bugsdata(data.dir)


save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
