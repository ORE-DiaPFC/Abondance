##-----------------------------INFO ----------------------------------##
year <- "2023"
site <- "Oir"
stade <- "smolt"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)


##-----------------------------DATA ----------------------------------##
fish <- read.bugsdata(paste("data/data_",stade,".txt",sep=""))
eff_MC <- read.bugsdata(paste("data/data_effort.txt",sep=""))
Q_MC <- read.bugsdata(paste("data/data_flow.txt",sep=""))
age <- read.bugsdata(paste("data/data_age.txt",sep=""))

data <- c(fish
          ,eff_MC
          ,Q_MC
          ,age
)

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
