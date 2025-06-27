##-----------------------------INFO ----------------------------------##
# year <- "YEAR"
# site <- "SITE"
# stade <- "STADE"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)



##-----------------------------DATA ----------------------------------##
data.dir <- paste("data/data-",stade,"-",year,".txt",sep="")
data <- read.bugsdata(data.dir)


save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
