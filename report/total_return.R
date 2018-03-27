
setwd("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abundance/")


year=2017
#mycol=c("#787878", "#1E90FF", "#FF6A6A", "#a1dab4")
COL <- c("#5C5C5C", "#00CD66", "#FF4500", "#00B2EE")

  
## ADULTS
<<<<<<< HEAD
year=2017
sites <- c("Bresle", "Oir","Nivelle", "Scorff")
=======
>>>>>>> c282adc0640fd268ecee36f98e12b2484f610dd9
stade <- "adult"
years <- seq(1984, year, 1)
sites <- c("Bresle", "Oir","Nivelle", "Scorff")
mycol <- COL


SW=returns=list()
total=NULL
 for (site in sites){
<<<<<<< HEAD
   table <- array(, dim=c(length(years), 4))
   colnames(table) <- c( "Year of return","1SW", 	"MSW",	"Total")
   rownames(table) <- years
   
   # load results
    load(paste0(site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
=======
   table <- array(, dim=c(length(years), 4)); colnames(table) <- c( "Year of return","1SW", 	"MSW",	"Total"); rownames(table) <- years
   quant <- array(, dim=c(length(years), 3)); colnames(quant) <- c("2.5%","50%","97.5%"); rownames(quant) <- years

    ### LOAD RESULTS
    load(paste0("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
>>>>>>> c282adc0640fd268ecee36f98e12b2484f610dd9

    
    ### QUANTILES
    tmp <- as.matrix(fit$sims.matrix)
    
    if (site == "Bresle"){ 
    mcmc <- as.matrix(tmp[,paste0("n_tot[",1:34,"]")]) # 1984 to now)
    sum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
    quant[1:length(years),] <- sum
    }
    
    if (site == "Oir"){ 
      mcmc <- as.matrix(tmp[,paste0("n_tot[",1:34,"]")]) # 1984 to now)
      sum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
      quant[1:length(years),] <- sum
    }
    
    if (site == "Scorff"){
      mcmc <- as.matrix(tmp[,paste0("n_tot[",1:24,"]")]) # 1984 to now)
      sum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
      quant[11:length(years),] <- sum
    }
    
    
    if (site == "Nivelle"){ 
      mcmc <- as.matrix(tmp[,paste0("n_tot[",1:34,"]")]) # 1984 to now)
      sum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
      quant[1:length(years),] <- sum
    }
    
    returns[[paste0(site)]] <- quant

    
    ### TABLEAU    
    n_tot <- ceiling(fit$median$n_tot) #annual total number of fish
    n_1SW <- ceiling(fit$median$n_1SW) # annual total number of fish 1SW
    n_MSW <- ceiling(fit$median$n_MSW) # annual total number of fish MSW
    
    table[,1] <- years
    if (site == "Scorff"){
      table[11:length(years),2] <- n_1SW
      table[11:length(years),3] <- n_MSW
      table[11:length(years),4] <- n_tot
    } else {
      table[,2] <- n_1SW
      table[,3] <- n_MSW
      table[,4] <- n_tot
    }

    SW[[paste0(site)]] <- table
    tmp <- table[,4]
    total <- cbind(total, tmp)
    write.csv(table, file=paste('report/Total_return_',site,'.csv',sep=""),row.names = FALSE)
}

#total <- data.frame(total)
colnames(total) <- c(sites)
rownames(total) <- years
write.csv(total, file=paste('report/Total_return_all.csv',sep=""),row.names = TRUE)

### Total number of returns
png("report/total_return.png",width = 780, height = 480)
plot(NULL,xlim=c(1,length(years)),ylim=c(0,1500),bty="n",ylab="Total number of fish",xaxt="n",xlab="Year of return")
axis(side=1,line=1,labels = years,at=1:length(years))
for (site in 1:4) {
  #lines(total[,site],lty=1,lwd=3,col=mycol[site])  
  lines(returns[[site]][,"50%"],lty=1,lwd=3,col=mycol[site]) 
  lower <- returns[[site]][,"2.5%"]
  upper <- returns[[site]][,"97.5%"]
  xx <- c(1:length(years),rev(1:length(years)))
  yy<-c(lower, rev(upper))
  polygon(xx,yy,col=paste0(mycol[site],"40"),border="NA")
}
legend("topright", legend=sites, col=mycol,lty=1,lwd=2,bty="n")
dev.off()


### Proportion MSW
png("report/prop_MSW_return.png",width = 780, height = 480)
plot(NULL,xlim=c(1,length(years)),ylim=c(0,1),bty="n",ylab="Proprotion of MSW",xaxt="n",xlab="Year of return")
axis(side=1,line=1,labels = years,at=1:length(years))
for (site in 1:4) {
  lines(SW[[site]][,"MSW"]/SW[[site]][,"Total"],lty=1,lwd=3,col=mycol[site])  
}
legend("topright", legend=sites, col=mycol,lty=1,lwd=2,bty="n")
dev.off()



## SMOLTS
stade <- "smolt"
years <- seq(1982, year, 1)
sites <- c("Bresle", "Oir", "Scorff")
mycol=COL[c(1,2,4)]


smolts=list()
total=NULL
for (site in sites){
  table <- array(, dim=c(length(years), 3))
  colnames(table) <- c("2.5%","50%","97.5%")
  rownames(table) <- years
  
  # load results
  load(paste0("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
  
  Nesc <- ceiling(fit$median$Nesc) ## escapement from river
  tmp <- as.matrix(fit$sims.matrix)
  
  if (site == "Bresle"){ 
    mcmc <- as.matrix(tmp[,paste0("Nesc[",c(1:6, 11:19,21:nrow(table)),"]")])
    sum <- apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975))
    
    table[1:6,] <- t(sum[,1:6]) # 1982 to 1987
    table[11:19,] <- t(sum[,7:15]) # 1992 to 2000
    table[21:nrow(table),] <- t(sum[,16:dim(sum)[2]]) # 2002 to now
  }

  
  if (site == "Scorff"){ 
    mcmc <- as.matrix(tmp[,paste0("Nesc[",1:23,"]")])    
    sum <- apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975))
    
    table[14:nrow(table),] <- t(sum) # 1995 to now
  }
  
  
  if (site == "Oir"){ 
    mcmc <- as.matrix(tmp[,paste0("Nesc[",1:32,"]")])
    sum <- apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975))
    
    table[5:nrow(table),] <- t(sum) # capture of smolts started in 1986
    }
  
  
  smolts[[paste0(site)]] <- table
  
  #write.csv(table, file=paste('report/Total_return_',site,'.csv',sep=""),row.names = FALSE)
}

### Total number of smolt
png("report/total_smolt.png",width = 780, height = 480)
plot(NULL,xlim=c(1,length(years)),ylim=c(0, 15000),bty="n",ylab="Total number of smolt",xaxt="n",xlab="Year")
axis(side=1,line=1,labels = years,at=1:length(years))
for (site in 1:3) {
  if (site == 1){ # Bresle
  lines(smolts[[site]][,"50%"],lty=1,lwd=3,col=mycol[site]) 
  lower <- smolts[[site]][22:length(years),"2.5%"]
  upper <- smolts[[site]][22:length(years),"97.5%"]
  xx <- c(22:length(years),rev(22:length(years)))
  yy<-c(lower, rev(upper))
  polygon(xx,yy,col=paste0(mycol[site],"40"),border="NA")
  } else {
    lines(smolts[[site]][,"50%"],lty=1,lwd=3,col=mycol[site]) 
    lower <- smolts[[site]][,"2.5%"]
    upper <- smolts[[site]][,"97.5%"]
    xx <- c(1:length(years),rev(1:length(years)))
    yy<-c(lower, rev(upper))
    polygon(xx,yy,col=paste0(mycol[site],"40"),border="NA")
  }

}
legend("topright", legend=sites, col=mycol,lty=1,lwd=2,bty="n")
dev.off()



## TACON
stade <- "tacon"
years <- seq(1982, year, 1)
sites <- c("Oir", "Nivelle","Scorff")
mycol=COL[2:4]


tacon=list()
total=NULL
for (site in sites){
  table <- array(, dim=c(length(years), 3))
  colnames(table) <- c("2.5%","50%","97.5%")
  rownames(table) <- years
  
  # load results
  load(paste0("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
  
  
  if (site == "Scorff"){ 
    ntot_Sc <- ceiling(fit$median$ntot_Sc)
    tmp <- as.matrix(fit$sims.matrix)
    mcmc <- as.matrix(tmp[,paste0("ntot_Sc[",1:25,"]")])    
    sum <- apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975))
    
    table[12:nrow(table),] <- t(sum) # 1995 to now
  }
  
  
  if (site == "Oir"){ 
    ntot_Oir <- ceiling(fit$median$ntot_Oir)
    tmp <- as.matrix(fit$sims.matrix)
    mcmc <- as.matrix(tmp[,paste0("ntot_Oir[",1:31,"]")])
    sum <- apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975))
    
    table[6:nrow(table),] <- t(sum) # capture of smolts started in 1986
  }
  
  
  if (site == "Nivelle"){ 
    sum <- read.table(paste0("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abundance/",site,"/",stade,"/results/YOYnat.txt"), h=TRUE)
    
    table[3:nrow(table),1:3] <- as.matrix(sum[1:nrow(sum),c(3,6,9)])
  }
  
  tacon[[paste0(site)]] <- table
  
  #write.csv(table, file=paste('report/Total_return_',site,'.csv',sep=""),row.names = FALSE)
}

### Total number of smolt
png("report/total_tacon.png",width = 780, height = 480)
plot(NULL,xlim=c(1,length(years)),ylim=c(0, 60000),bty="n",ylab="Total number of parr",xaxt="n",xlab="Year")
axis(side=1,line=1,labels = years,at=1:length(years))
for (site in 1:3) {
  # if (site == 1){ # Bresle
  #   lines(tacon[[site]][,"50%"],lty=1,lwd=3,col=mycol[site]) 
  #   lower <- tacon[[site]][22:length(years),"2.5%"]
  #   upper <- tacon[[site]][22:length(years),"97.5%"]
  #   xx <- c(22:length(years),rev(22:length(years)))
  #   yy<-c(lower, rev(upper))
  #   polygon(xx,yy,col=paste0(mycol[site],"40"),border="NA")
  # } else {
    lines(tacon[[site]][,"50%"],lty=1,lwd=3,col=mycol[site]) 
    lower <- tacon[[site]][,"2.5%"]
    upper <- tacon[[site]][,"97.5%"]
    xx <- c(1:length(years),rev(1:length(years)))
    yy<-c(lower, rev(upper))
    polygon(xx,yy,col=paste0(mycol[site],"40"),border="NA")
  #}
  
}
legend("topright", legend=sites, col=mycol,lty=1,lwd=2,bty="n")
dev.off()
