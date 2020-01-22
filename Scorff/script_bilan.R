#rm(list=ls())   # Clear memory

#setwd("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/")
wdir <- "/media/hdd/mbuoro/ORE-DiaPFC/Abundance/"
setwd(wdir)

#year <- 2018

##________________________SCORFF (starting in 1994)
site <- "Scorff"
#setwd(paste(wdir,site,"/",sep=""))

years <- seq(1993, year, 1)
table <- array(, dim=c(length(years), 7))
colnames(table) <- c( "Year","Parr 0+","Smolts","1SW (tot returns)",	"MSW (tot returns)", 	"1SW (escapment)",	"MSW (escapment)")
rownames(table) <- years
smolt.years <- years
table[,1] <- years


## PARR (from 1993 to now on)
stade <- "tacon"
dir <-  paste(wdir,site,"/",stade,sep="")
if (file.exists(dir)){
  load(paste0(site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
  n_parr <- fit$median$ntot_Sc
  table[,2] <- round(n_parr,0) # 1995 to now
}



## SMOLTS (from 1995 to now on)
stade <- "smolt"
dir <-  paste(wdir,site,"/",stade,sep="")
if (file.exists(dir)){
  load(paste0(site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
  n_smolt <- fit$median$Nesc # escapement from river
table[3:nrow(table),3] <- round(n_smolt,0) # 1995 to now
}


## ADULTS (1984 to now)
stade <- "adult"
dir <-  paste(wdir,site,"/",stade,sep="")
if (file.exists(dir)){
  load(paste0(site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
  
  # n returns
  n_1SW <- fit$median$n_1SW # 1SW
  n_MSW <- fit$median$n_MSW #  MSW
  table[2:nrow(table),4] <- round(n_1SW,0) # 1995 to now
  table[2:nrow(table),5] <- round(n_MSW,0) # 1995 to now
  
  # escapment
e_1SW <- fit$median$e_1SW # 1SW
e_MSW <- fit$median$e_MSW #  MSW
table[2:nrow(table),6] <- round(e_1SW,0) # 1995 to now
table[2:nrow(table),7] <- round(e_MSW,0) # 1995 to now
}


#write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table9_',site,"_",year,'.csv',sep=""))
con <- file(paste(wdir,site,"/Bilan_",site,"_",year,'.csv',sep=""), open="wt")
write.csv( table, con, row.names = FALSE)
close(con)
