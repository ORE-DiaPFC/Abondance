#rm(list=ls())   # Clear memory
## WORKING DIRECTORY:


#year <- 2017

##________________________SCORFF (starting in 1994)
site <- "Scorff"
year <- 2023

work.dir<-paste0("/media/hdd4To/mbuoro/ORE-DiaPFC/Abundance")
setwd(work.dir)
#setwd(paste("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abundance/",site,"/",sep=""))

years <- seq(1993, year, 1)
table <- array(, dim=c(length(years), 6))
colnames(table) <- c( "Year","Parr 0+","All smolts","Smolts 1+", 	"1SW (escapment)",	"MSW (escapment)")
rownames(table) <- years
smolt.years <- years
table[,1] <- years


## PARR (from 1993 to now on)
stade <- "tacon"
dir <-  paste(site,"/",stade,sep="")
if (file.exists(dir)){
  load(paste(site,"/",stade,"/data/data_",stade,"_",year,".Rdata",sep=""))
  load(paste(site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
  n_parr <- fit$median$ntot_Sc
  table[,2] <- round(n_parr,0) # 1995 to now
  

  table_parr <- fit$summary[paste0("ntot_Sc[",1:data$Nyear,"]"),c("2.5%","25%","50%","75%","97.5%")]
  rownames(table_parr) <- 1993:year
  con <- file(paste0(site,"/Table_",site,"_",stade,"_",year,'.csv'), open="wt")
  write.csv( round(table_parr,2), con, row.names = TRUE)
  close(con)
}



## SMOLTS (from 1995 to now on)
stade <- "smolt"
dir <-  paste(site,"/",stade,sep="")
if (file.exists(dir)){
  load(paste(site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
  n_smolt <- fit$median$Ntot # smolt numbers by year of migration
  table[3:nrow(table),3] <- round(n_smolt,0) # 1995 to now
  n_smolt <- fit$median$Nc[,1] # smolt 1+ numbers by year of migration
  table[3:nrow(table),4] <- round(n_smolt,0) # 1995 to now
  
  # Total number of smolt
  load(paste(site,"/",stade,"/data/data_",stade,"_",year,".Rdata",sep=""))
  table_smolt <- fit$summary[paste0("Ntot[",1:data$Nyears,"]"),c("2.5%","25%","50%","75%","97.5%")]
  rownames(table_smolt) <- 1995:year
  con <- file(paste0(site,"/Table_",site,"_",stade,"_",year,'.csv'), open="wt")
  write.csv( round(table_smolt,2), con, row.names = TRUE)
  close(con)
  
  # Number of smolt 1+
  #load(paste(site,"/",stade,"/data/data_",stade,"_",year,".Rdata",sep=""))
  table_smolt1 <- fit$summary[paste0("Nc[",1:data$Nyears,",1]"),c("2.5%","25%","50%","75%","97.5%")]
  rownames(table_smolt1) <- 1995:year
  con <- file(paste0(site,"/Table_",site,"_",stade,"1+_",year,'.csv'), open="wt")
  write.csv( round(table_smolt1,2), con, row.names = TRUE)
  close(con)
}


## ADULTS (1984 to now)
stade <- "adult"
dir <-  paste(site,"/",stade,sep="")
# ,"n_tot" # total annual number of adults entering the river
# ,"n_1SW" # annual number of 1SW
# ,"n_MSW" # annual number of MSW
if (file.exists(dir)){
  load(paste(site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
  e_1SW <-fit$median$e_1SW # 1SW
  e_MSW <- fit$median$e_MSW #  MSW
  table[2:nrow(table),5] <- round(e_1SW,0) # 1995 to now
  table[2:nrow(table),6] <- round(e_MSW,0) # 1995 to now
  
  
  # Total number of anadromous
  load(paste(site,"/",stade,"/data/data_",stade,"_",year,".Rdata",sep=""))
  table_ntot <- fit$summary[paste0("n_tot[",1:data$Y,"]"),c("2.5%","25%","50%","75%","97.5%")]
  rownames(table_ntot) <- 1994:year
  con <- file(paste0(site,"/Table_",site,"_",stade,"_",year,'.csv'), open="wt")
  write.csv( round(table_ntot,2), con, row.names = TRUE)
  close(con)
  
  # Number of 1SW
  #load(paste(site,"/",stade,"/data/data_",stade,"_",year,".Rdata",sep=""))
  table_1SW <- fit$summary[paste0("n_1SW[",1:data$Y,"]"),c("2.5%","25%","50%","75%","97.5%")]
  rownames(table_1SW) <- 1994:year
  con <- file(paste0(site,"/Table_",site,"_",stade,"1SW_",year,'.csv'), open="wt")
  write.csv( round(table_1SW,2), con, row.names = TRUE)
  close(con)
  
  # Number of MSW
  #load(paste(site,"/",stade,"/data/data_",stade,"_",year,".Rdata",sep=""))
  table_MSW <- fit$summary[paste0("n_MSW[",1:data$Y,"]"),c("2.5%","25%","50%","75%","97.5%")]
  rownames(table_MSW) <- 1994:year
  con <- file(paste0(site,"/Table_",site,"_",stade,"MSW_",year,'.csv'), open="wt")
  write.csv( round(table_MSW,2), con, row.names = TRUE)
  close(con)
}


#write.csv(round(table,2), file=paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/CIEM/Table9_',site,"_",year,'.csv',sep=""))
con <- file(paste(site,"/Bilan_",site,"_",year,'.csv',sep=""), open="wt")
write.csv( table, con, row.names = FALSE)
close(con)
