
#setwd("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/")
setwd("/media/hdd/mbuoro/ORE-DiaPFC/Abundance/")

year=2018
#mycol=c("#787878", "#1E90FF", "#FF6A6A", "#a1dab4")
#COL <- c("#5C5C5C", "#00CD66", "#FF4500", "#00B2EE")

COL <- c("yellowgreen","steelblue1","hotpink2","tomato")
#mycol <- paste0(COL, 50)
mycol <- COL

###################################################################################################################
######## ADULTS
###################################################################################################################
stade <- "adult"
years <- seq(1984, year, 1)
sites <- c("Bresle", "Oir","Nivelle", "Scorff")


SW=returns=list()
total=NULL
 for (site in sites){

   table <- array(, dim=c(length(years), 4)); colnames(table) <- c( "Year of return","1SW", 	"MSW",	"Total"); rownames(table) <- years
   quant <- array(, dim=c(length(years), 3)); colnames(quant) <- c("2.5%","50%","97.5%"); rownames(quant) <- years

    ### LOAD RESULTS
    load(paste0(site,"/",stade,"/results/Results_",stade,"_",year,".RData"))

    
    ### QUANTILES
    tmp <- as.matrix(fit$sims.matrix)
    
    if (site == "Bresle"){ 
    mcmc <- as.matrix(tmp[,paste0("n_tot[",1:35,"]")]) # 1984 to now)
    sum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
    quant[1:length(years),] <- sum
    }
    
    if (site == "Oir"){ 
      mcmc <- as.matrix(tmp[,paste0("n_tot[",1:35,"]")]) # 1984 to now)
      sum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
      quant[1:length(years),] <- sum
    }
    
    if (site == "Scorff"){
      mcmc <- as.matrix(tmp[,paste0("n_tot[",1:25,"]")]) # 1984 to now)
      sum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
      quant[11:length(years),] <- sum
    }
    
    
    if (site == "Nivelle"){ 
      mcmc <- as.matrix(tmp[,paste0("n_tot[",1:35,"]")]) # 1984 to now)
      sum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
      quant[1:length(years),] <- sum
    }
    
    rownames(quant)<- 1984:year
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
    write.csv(table, file=paste('report/Total_return_byage_',site,'.csv',sep=""),row.names = TRUE)
    write.csv(returns[[paste0(site)]], file=paste('report/Total_return_',site,'.csv',sep=""),row.names = TRUE)
    
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
  segments(1:length(years),returns[[site]][,"2.5%"], 1:length(years),returns[[site]][,"97.5%"], col=paste0(mycol[site]))
  lines(returns[[site]][,"50%"],lty=1,lwd=2,col=mycol[site],type="o")
  points(1:length(years),returns[[site]][,"50%"],col=mycol[site],pch=21,bg=paste0(mycol[site]))
  
  # #lines(total[,site],lty=1,lwd=3,col=mycol[site])  
  # lines(returns[[site]][,"50%"],lty=1,lwd=2,col=mycol[site]) 
  # lower <- returns[[site]][,"2.5%"]
  # upper <- returns[[site]][,"97.5%"]
  # xx <- c(1:length(years),rev(1:length(years)))
  # yy<-c(lower, rev(upper))
  # polygon(xx,yy,col=paste0(mycol[site],"25"),border="NA")
  
  #scatter.smooth(years, returns[[site]][,"50%"], span = 2/3, degree = 2)
  df <- data.frame(x=years, y= returns[[site]][,"50%"])
  lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
  pred <- predict(lw1)
  if (site == 4){ pred<-c(rep(NA,10),pred) }
  lines(pred,col=mycol[site],lwd=4)
}
legend("topright", legend=sites, col=mycol,lty=1,lwd=2,bty="n")
dev.off()


### Proportion MSW
png("report/prop_MSW_return.png",width = 780, height = 480)
plot(NULL,xlim=c(1,length(years)),ylim=c(0,.8),bty="n",ylab="Proprotion of MSW",xaxt="n",xlab="Year of return")
axis(side=1,line=1,labels = years,at=1:length(years))
for (site in 1:4) {
  lines(SW[[site]][,"MSW"]/SW[[site]][,"Total"],lty=1,lwd=2,col=mycol[site])  
  
  df <- data.frame(x=years, y= SW[[site]][,"MSW"]/SW[[site]][,"Total"])
  lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
  pred <- predict(lw1)
  if (site == 4){ pred<-c(rep(NA,10),pred) }
  lines(pred,col=mycol[site],lwd=4)
}
legend("topright", legend=sites, col=mycol,lty=1,lwd=2,bty="n")
dev.off()




######## Table 7 - Exploitation rate in the rivers Scorff ######
## SCORFF
site <- "Scorff"
stade <- "adult"

# load dataset
load(paste(site,"/",stade,"/data/data_",stade,"_",year,'.Rdata',sep="")) # chargement des données


## Cm_F[t,a]: Annual number of marked fish caught by fishing per sea age category and showed at Moulin des Princes. 1:1SW, 2:MSW  
## Cum_F[t,a]: Annual number of unmarked fish caught by fishing per sea age category and showed at Moulin des Princes. 1:1SW, 2:MSW
C_F_1SW <- data$Cm_F[,1] + data$Cum_F[,1] # marked + unmarked 1SW fish caugth by fishing
C_F_MSW <- data$Cm_F[,2] + data$Cum_F[,2] # marked + unmarked MSW fish caugth by fishing

# /!\ Annual number of fish caught by fishing per sea age category from 1994 to 2002 / Not all reported then
#data$C_F # 94 -> 2002
C_F_1SW[1:length(data$C_F[,1])] <- data$C_F[,1]
C_F_MSW[1:length(data$C_F[,2])] <- data$C_F[,2]

# load estimations of size popualtions
load(paste0(site,"/",stade,"/results/Results_adult_",year,".RData"))
n_1SW <- fit$median$n_1SW # medians
n_MSW <- fit$median$n_MSW # medians

Expl_rate <- cbind(
  Expl_rate_1SW = (C_F_1SW / n_1SW)*100,
  Expl_rate_MSW = (C_F_MSW / n_MSW)*100  
)

#Expl_rate <- rbind(Expl_rate,colMeans(Expl_rate))

rowname <- c(seq(1994,year,1))#, "Average")
Expl_rate <- cbind(rowname,Expl_rate)
colnames(Expl_rate) <- c("Year","1SW (%)", "MSW (%)")
Expl_rate <- as.data.frame(Expl_rate)

png("report/exploitation_Scorff.png",width = 780, height = 480)
plot(NULL,xlim=c(1,nrow(Expl_rate)),ylim=c(0, 40),bty="n",ylab="Exploitation rate (%)",xaxt="n",xlab="Year")
axis(side=1,line=1,labels = Expl_rate$Year,at=1:nrow(Expl_rate))
    lines(Expl_rate$`1SW (%)`,lty=1,lwd=2,col=paste0(mycol[4],"50")) 
    lines(Expl_rate$`MSW (%)`,lty=1,lwd=2,col=paste0(mycol[4],"90")) 
    #lower <- smolts[[site]][,"2.5%"]
    #upper <- smolts[[site]][,"97.5%"]
    #xx <- c(1:length(years),rev(1:length(years)))
    #yy<-c(lower, rev(upper))
    #polygon(xx,yy,col=paste0(mycol[site],"40"),border="NA")
    
    df <- data.frame(x=years, y1sw= c(rep(NA,10),Expl_rate$`1SW (%)`), yMsw= c(rep(NA,10),Expl_rate$`MSW (%)`))
    lw1 <- loess(y1sw ~ x,span = 0.75, degree = 2, data=df)
    lw2 <- loess(yMsw ~ x,span = 0.75, degree = 2, data=df)
    pred1 <- predict(lw1);pred2 <- predict(lw2)
      #pred1<-c(rep(NA,10),pred1)
      #pred2<-c(rep(NA,10),pred2)
    lines(pred1,col=paste0(mycol[4],"50"),lwd=4)
    lines(pred2,col=paste0(mycol[4]),lwd=4)
    
    legend("topright", legend=c("1SW", "MSW"), col=c(paste0(mycol[4],"50"), mycol[4]),lty=1,lwd=3,bty="n")
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
  load(paste0(site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
  
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
    mcmc <- as.matrix(tmp[,paste0("Nesc[",1:24,"]")])    
    sum <- apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975))
    
    table[14:nrow(table),] <- t(sum) # 1995 to now
  }
  
  
  if (site == "Oir"){ 
    mcmc <- as.matrix(tmp[,paste0("Nesc[",1:33,"]")])
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
  lines(smolts[[site]][,"50%"],lty=1,lwd=2,col=paste0(mycol[site],50)) 
  lower <- smolts[[site]][22:length(years),"2.5%"]
  upper <- smolts[[site]][22:length(years),"97.5%"]
  xx <- c(22:length(years),rev(22:length(years)))
  yy<-c(lower, rev(upper))
  polygon(xx,yy,col=paste0(mycol[site],"25"),border="NA")
  
  df <- data.frame(x=years, y= smolts[[site]][,"50%"])
  lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
  tmp <- predict(lw1)
  pred <- rep(NA, length(years))
  pred[1:6]<-tmp[1:6]
  pred[11:19]<-tmp[7:15]
  pred[21:length(years)]<-tmp[16:length(tmp)]
  
  #pred<-c(rep(NA,21),pred) # Bresle
  lines(pred,col=mycol[site],lwd=4)
  
  } else {
    lines(smolts[[site]][,"50%"],lty=1,lwd=2,col=paste0(mycol[site],50)) 
    lower <- smolts[[site]][,"2.5%"]
    upper <- smolts[[site]][,"97.5%"]
    xx <- c(1:length(years),rev(1:length(years)))
    yy<-c(lower, rev(upper))
    polygon(xx,yy,col=paste0(mycol[site],"25"),border="NA")
    
    df <- data.frame(x=years, y= smolts[[site]][,"50%"])
    lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
    pred <- predict(lw1)
    if (site == 2){ pred<-c(rep(NA,4),pred) } # Oir
    if (site == 3){ pred<-c(rep(NA,13),pred) } # Scorff
    lines(pred,col=mycol[site],lwd=4)
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
  load(paste0(site,"/",stade,"/results/Results_",stade,"_",year,".RData"))
  
  
  if (site == "Scorff"){ 
    ntot_Sc <- ceiling(fit$median$ntot_Sc)
    tmp <- as.matrix(fit$sims.matrix)
    mcmc <- as.matrix(tmp[,paste0("ntot_Sc[",1:26,"]")])    
    sum <- apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975))
    
    table[12:nrow(table),] <- t(sum) # 1995 to now
  }
  
  
  if (site == "Oir"){ 
    ntot_Oir <- ceiling(fit$median$ntot_Oir)
    tmp <- as.matrix(fit$sims.matrix)
    mcmc <- as.matrix(tmp[,paste0("ntot_Oir[",1:32,"]")])
    sum <- apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975))
    
    table[6:nrow(table),] <- t(sum) # capture of smolts started in 1986
  }
  
  
  if (site == "Nivelle"){ 
    sum <- read.table(paste0(site,"/",stade,"/results/YOYnat.txt"), h=TRUE)
    
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
    lines(tacon[[site]][,"50%"],lty=1,lwd=2,col=paste0(mycol[site],50)) 
    lower <- tacon[[site]][,"2.5%"]
    upper <- tacon[[site]][,"97.5%"]
    xx <- c(1:length(years),rev(1:length(years)))
    yy<-c(lower, rev(upper))
    polygon(xx,yy,col=paste0(mycol[site],"25"),border="NA")
  #}
  
    df <- data.frame(x=years, y= tacon[[site]][,"50%"])
    lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
    pred <- predict(lw1)
    if (site == 1){ pred<-c(rep(NA,5),pred) }
    if (site == 2){ pred<-c(rep(NA,3),pred) }
    if (site == 3){ pred<-c(rep(NA,11),pred) }
    lines(pred,col=mycol[site],lwd=4)
}
legend("topright", legend=sites, col=mycol,lty=1,lwd=2,bty="n")
dev.off()
