##-----------------------------INFO ----------------------------------##
# year <- 2015
# site <- "Oir"
# stade <- "smolt"


##-----------------------------DATA ----------------------------------##
fish <- read.bugsdata(paste("data/data_",stade,".txt",sep=""))
eff_MC <- read.bugsdata(paste("data/data_effort.txt",sep=""))
Q_MC <- read.bugsdata(paste("data/data_flow.txt",sep=""))

data <- c(fish
          ,eff_MC
          ,Q_MC
          )

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
