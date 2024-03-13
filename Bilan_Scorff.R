## ----setup, include=FALSE-----------------------------------------------------
wdir <- "/media/hdd4To/mbuoro/ORE-DiaPFC/Abundance/"
setwd(wdir)

site <- "Scorff"
year <- 2022
years <- seq(1993, year, 1)

#COL <- c("yellowgreen","hotpink2","steelblue1","black")
mycol=c("#787878", "#1E90FF", "#a1dab4", "#FF6A6A")
#mycol <- paste0(COL, 50)
#mycol <- COL
##________________________SCORFF (starting in 1994)



## ----echo = FALSE-------------------------------------------------------------

table <- array(, dim=c(length(years), 6))
colnames(table) <- c( "Année","Tacon 0+","Smolts (tous)","Smolts 1+", 	"1HM (échappement)",	"PHM (échappement)")
rownames(table) <- years
smolt.years <- years
table[,1] <- years


## PARR (from 1993 to now on)
stade <- "tacon"
dir <-  paste(wdir,site,"/",stade,sep="")
if (file.exists(dir)){
  load(paste(wdir,site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
  n_parr <- fit$median$ntot_Sc
  table[,2] <- round(n_parr,0) # 1995 to now
}



## SMOLTS (from 1995 to now on)
stade <- "smolt"
dir <-  paste(wdir,site,"/",stade,sep="")
if (file.exists(dir)){
  load(paste(wdir,site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
  n_smolt <- fit$median$Ntot # smolt numbers by year of migration
  table[3:nrow(table),3] <- round(n_smolt,0) # 1995 to now
  n_smolt <- fit$median$Nc[,1] # smolt 1+ numbers by year of migration
  table[3:nrow(table),4] <- round(n_smolt,0) # 1995 to now
}


## ADULTS (1984 to now)
stade <- "adult"
dir <-  paste(wdir,site,"/",stade,sep="")
if (file.exists(dir)){
  load(paste(wdir,site,"/",stade,"/results/Results_",stade,"_",year,".RData",sep=""))
  e_1SW <-fit$median$e_1SW # 1SW
  e_MSW <- fit$median$e_MSW #  MSW
  table[2:nrow(table),5] <- round(e_1SW,0) # 1995 to now
  table[2:nrow(table),6] <- round(e_MSW,0) # 1995 to now
}


con <- file(paste0(wdir,site,"/","Bilan_",site,"_",year,'.csv'), open="wt")
write.csv( table, con, row.names = FALSE)
close(con)

library(knitr)
kable(table, row.names = FALSE, caption = paste0("Tableau bilan pour le ",site,". Seules les valeurs médianes sont reportées"))



## ----pressure, echo=FALSE-----------------------------------------------------
#### RETURNS

stade <- "adult"
dir <-  paste(wdir,site,"/",stade,sep="")
#if (file.exists(dir)){
# load dataset
load(paste(dir,"/data/data_",stade,"_",year,'.Rdata',sep="")) # chargement des données
load(paste0(dir,"/results/Results_",stade,"_",year,".RData"))
#}


tmp <- as.matrix(fit$sims.matrix)

#png(paste0(site,"/total_return.png"),width = 780, height = 480)
par(mfrow=c(1,1)) 

### Total number of returns
plot(NULL,xlim=c(1,data$Y),ylim=c(0,1500),bty="n",ylab="Nombre total de retour",xaxt="n",xlab="")
axis(side=1,line=1,labels = years[-1],at=1:data$Y, las=2, cex=.5)

# N total
mcmc <- as.matrix(tmp[,paste0("n_tot[",1:data$Y,"]")]) # 1984 to now)
n <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
segments(1:data$Y,n[,"2.5%"], 1:data$Y,n[,"97.5%"], col=paste0(mycol[3]))
lines(n[,"50%"],lty=1,lwd=2,col=mycol[3],type="o")
points(1:data$Y,n[,"50%"],col=mycol[3],pch=21,bg=paste0(mycol[3]))

# Escapement
mcmc <- as.matrix(tmp[,paste0("e_tot[",1:data$Y,"]")]) # 1984 to now)
e <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
segments(1:data$Y,e[,"2.5%"], 1:data$Y,e[,"97.5%"], col=paste0(mycol[4]))
lines(e[,"50%"],lty=1,lwd=2,col=mycol[4],type="o")
points(1:data$Y,e[,"50%"],col=mycol[4],pch=21,bg=paste0(mycol[4]))

legend("topright", legend=c("Nombre total de retour", "Echappement"), col=mycol[3:4],lty=1,lwd=2,bty="n")


## ----echo=FALSE---------------------------------------------------------------


plot(NULL,xlim=c(1,data$Y),ylim=c(0,1500),bty="n",ylab="Nombre total de retour",xaxt="n",xlab="")
axis(side=1,line=1,labels = years[-1],at=1:data$Y, las=2, cex=.5)
mycol=c("#787878", "#1E90FF", "#a1dab4", "#FF6A6A")

# 1SW
mcmc <- as.matrix(tmp[,paste0("n_1SW[",1:data$Y,"]")]) # 1984 to now)
n_1SW <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
segments(1:data$Y,n_1SW[,"2.5%"], 1:data$Y,n_1SW[,"97.5%"], col=paste0(mycol[1]))
lines(n_1SW[,"50%"],lty=1,lwd=2,col=mycol[1],type="o")
points(1:data$Y,n_1SW[,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))

# MSW
mcmc <- as.matrix(tmp[,paste0("n_MSW[",1:data$Y,"]")]) # 1984 to now)
n_MSW <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
segments(1:data$Y,n_MSW[,"2.5%"], 1:data$Y,n_MSW[,"97.5%"], col=paste0(mycol[2]))
lines(n_MSW[,"50%"],lty=1,lwd=2,col=mycol[2],type="o")
points(1:data$Y,n_MSW[,"50%"],col=mycol[2],pch=21,bg=paste0(mycol[2]))

legend("topright", legend=c("1HM","PHM"), col=mycol[1:2],lty=1,lwd=2,bty="n")


#dev.off()




## ----echo=FALSE---------------------------------------------------------------

#### CAPTURE AT MP ####
par(mfrow=c(1,1)) 
mycol=c("#787878", "#1E90FF", "#a1dab4", "#FF6A6A")
mcmc <- as.matrix(tmp[,paste0("pi_MP[",1:data$Y,",1]")]) # 
piMP_1SW <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))*100
mcmc <- as.matrix(tmp[,paste0("pi_MP[",1:data$Y,",2]")]) # 
piMP_MSW <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))*100
### Total number of returns
#png("report/total_return.png",width = 780, height = 480)
plot(NULL,xlim=c(1,data$Y),ylim=c(0,100),bty="n",ylab="Proba de capture (en %)",xaxt="n",xlab="",main="Proba. de capture au Moulin des Princes")
axis(side=1,line=1,labels = years[-1],at=1:data$Y, las=2, cex=.5)

# N total
segments(1:data$Y,piMP_MSW[,"2.5%"], 1:data$Y,piMP_MSW[,"97.5%"], col=paste0(mycol[2]))
lines(piMP_MSW[,"50%"],lty=1,lwd=2,col=mycol[2],type="o")
points(1:data$Y,piMP_MSW[,"50%"],col=mycol[2],pch=21,bg=paste0(mycol[2]))

  df <- data.frame(x=1:data$Y, y= piMP_MSW[,"50%"])
  lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
  pred <- predict(lw1)
  lines(pred,col=mycol[2],lwd=2,lty=2)
  
# N total
segments(1:data$Y,piMP_1SW[,"2.5%"], 1:data$Y,piMP_1SW[,"97.5%"], col=paste0(mycol[1]))
lines(piMP_1SW[,"50%"],lty=1,lwd=2,col=mycol[1],type="o")
points(1:data$Y,piMP_1SW[,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))

  df <- data.frame(x=1:data$Y, y= piMP_1SW[,"50%"])
  lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
  pred <- predict(lw1)
  lines(pred,col=mycol[1],lwd=2,lty=2)
  
legend("topright", legend=c("1HM", "PHM"), col=mycol[1:2],lty=1,lwd=3,bty="n")






## ----echo=FALSE---------------------------------------------------------------

#### CAPTURE AT MP ####
par(mfrow=c(1,1)) 
mycol=c("#787878", "#1E90FF", "#a1dab4", "#FF6A6A")
mcmc <- as.matrix(tmp[,paste0("piD_1SW[",1:data$Y,",1]")]) # 
piD_1SWm <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))*100
mcmc <- as.matrix(tmp[,paste0("piD_1SW[",1:data$Y,",2]")]) # 
piD_1SWum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))*100

mcmc <- as.matrix(tmp[,paste0("piD_MSW[",1:data$Y,",1]")]) # 
piD_MSWm <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))*100
mcmc <- as.matrix(tmp[,paste0("piD_MSW[",1:data$Y,",2]")]) # 
piD_MSWum <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))*100


### Total number of returns
#png("report/total_return.png",width = 780, height = 480)
plot(NULL,xlim=c(1,data$Y),ylim=c(0,100),bty="n",ylab="Proba de mortalité (en %)",xaxt="n",xlab="",main="Proba. de mortalité (hors exploitation)")
axis(side=1,line=1,labels = years[-1],at=1:data$Y, las=2, cex=.5)

# N total
segments(1:data$Y,piD_1SWm[,"2.5%"], 1:data$Y,piD_1SWm[,"97.5%"], col=paste0(mycol[2]))
lines(piD_1SWm[,"50%"],lty=1,lwd=2,col=mycol[2],type="o")
points(1:data$Y,piD_1SWm[,"50%"],col=mycol[2],pch=21,bg=paste0(mycol[2]))

  df <- data.frame(x=1:data$Y, y= piD_1SWm[,"50%"])
  lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
  pred <- predict(lw1)
  lines(pred,col=mycol[2],lwd=2,lty=2)
  
# N total
# segments(1:data$Y,piD_1SWum[,"2.5%"], 1:data$Y,piD_1SWum[,"97.5%"], col=paste0(mycol[1]))
# lines(piD_1SWum[,"50%"],lty=1,lwd=2,col=mycol[1],type="o")
# points(1:data$Y,piD_1SWum[,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))
# 
#   df <- data.frame(x=1:data$Y, y= piD_1SWum[,"50%"])
#   lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
#   pred <- predict(lw1)
#   lines(pred,col=mycol[1],lwd=2,lty=2)
#   
#  legend("topright", legend=c("marked", "unmarked"), col=mycol[1:2],lty=1,lwd=3,bty="n")
  
  
  # MSW
#plot(NULL,xlim=c(1,data$Y),ylim=c(0,100),bty="n",ylab="Proba de mortalité (en %)",xaxt="n",xlab="",main="Proba. de mortalité des PHM")
#axis(side=1,line=1,labels = years[-1],at=1:data$Y, las=2, cex=.5)

# segments(1:data$Y,piD_MSWm[,"2.5%"], 1:data$Y,piD_MSWm[,"97.5%"], col=paste0(mycol[3]))
# lines(piD_MSWm[,"50%"],lty=1,lwd=2,col=mycol[3],type="o")
# points(1:data$Y,piD_MSWm[,"50%"],col=mycol[3],pch=21,bg=paste0(mycol[3]))
# 
#   df <- data.frame(x=1:data$Y, y= piD_MSWm[,"50%"])
#   lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
#   pred <- predict(lw1)
#   lines(pred,col=mycol[3],lwd=2,lty=2)
  
# N total
segments(1:data$Y,piD_MSWum[,"2.5%"], 1:data$Y,piD_MSWum[,"97.5%"], col=paste0(mycol[4]))
lines(piD_MSWum[,"50%"],lty=1,lwd=2,col=mycol[4],type="o")
points(1:data$Y,piD_MSWum[,"50%"],col=mycol[4],pch=21,bg=paste0(mycol[4]))

  df <- data.frame(x=1:data$Y, y= piD_MSWum[,"50%"])
  lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
  pred <- predict(lw1)
  lines(pred,col=mycol[4],lwd=2,lty=2)
  
#legend("topright", legend=c("marked", "unmarked"), col=mycol[1:2],lty=1,lwd=3,bty="n")

legend("topright", legend=c("1HM", "PHM"), col=c(mycol[2],mycol[4]),lty=1,lwd=3,bty="n")

tmp <- data.frame(Years=1993:2021, MSW=piD_MSWum[,"50%"], OneSW=piD_1SWum[,"50%"])
library(knitr)
kable(tmp, row.names = FALSE, caption = paste0("Tableau bilan mortalité naturelle (en %) pour le ",site,". Seules les valeurs médianes sont reportées"))




## ----smolt, echo=FALSE--------------------------------------------------------

stade <- "smolt"
#dir <-  paste(wdir,site,"/",stade,sep="")
#if (file.exists(dir)){
# load dataset
load(paste(stade,"/data/data_",stade,"_",year,'.Rdata',sep="")) # chargement des données
load(paste0(stade,"/results/Results_",stade,"_",year,".RData"))
#}


tmp <- as.matrix(fit$sims.matrix)

#png(paste0(site,"/total_return.png"),width = 780, height = 480)
par(mfrow=c(1,1)) 

### Total number of returns
plot(NULL,xlim=c(1,data$Nyears),ylim=c(0,15000),bty="n",ylab="Nombre total de smolts",xaxt="n",xlab="Année de migration")
axis(side=1,line=1,labels = years[-c(1,2)],at=1:data$Nyears, las=2, cex.axis=.6)

# N total
mcmc <- as.matrix(tmp[,paste0("Ntot[",1:data$Nyears,"]")])    
n <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
segments(1:data$Nyears,n[,"2.5%"], 1:data$Nyears,n[,"97.5%"], col=paste0(mycol[1]))
lines(n[,"50%"],lty=1,lwd=2,col=mycol[1],type="o")
points(1:data$Nyears,n[,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))

# # Escapment
# mcmc <- as.matrix(tmp[,paste0("Nesc[",1:data$Nyears,"]")])    
# n <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
# segments(1:data$Nyears,n[,"2.5%"], 1:data$Nyears,n[,"97.5%"], col=paste0(mycol[2]))
# lines(n[,"50%"],lty=1,lwd=2,col=mycol[2],type="o")
# points(1:data$Nyears,n[,"50%"],col=mycol[2],pch=21,bg=paste0(mycol[2]))

#legend("topright", legend=c("Nombre total de smolts", "Echappement"), col=mycol[1:2],lty=1,lwd=2,bty="n")


par(mfrow=c(1,1)) 

### Total number of returns
plot(NULL,xlim=c(1,data$Nyears),ylim=c(0,15000),bty="n",ylab="Nombre total de smolts",xaxt="n",xlab="Cohorte")
axis(side=1,line=1,labels = years[-c(1,2)],at=1:data$Nyears, las=2, cex.axis=.6)

# N total
mcmc <- as.matrix(tmp[,paste0("N[",1:data$Nyears,"]")])    
n <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
segments(1:data$Nyears,n[,"2.5%"], 1:data$Nyears,n[,"97.5%"], col=paste0(mycol[1]))
lines(n[,"50%"],lty=1,lwd=2,col=mycol[1],type="o")
points(1:data$Nyears,n[,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))



par(mfrow=c(1,1)) 

### Total number of returns
plot(NULL,xlim=c(1,data$Nyears),ylim=c(0,1),bty="n",ylab="Proportion de smolts 1+",xaxt="n",xlab=" Cohorte")
axis(side=1,line=1,labels = years[-c(1,2)],at=1:data$Nyears, las=2, cex.axis=.6)
# N total
mcmc <- as.matrix(tmp[,paste0("p1c[",1:data$Nyears,"]")])    
n <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
segments(1:data$Nyears,n[,"2.5%"], 1:data$Nyears,n[,"97.5%"], col=paste0(mycol[1]))
lines(n[,"50%"],lty=1,lwd=2,col=mycol[1],type="o")
points(1:data$Nyears,n[,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))




## ----capt_smolt, echo=FALSE---------------------------------------------------
par(mfrow=c(1,1)) 

### Total number of returns
plot(NULL,xlim=c(1,data$Nyears),ylim=c(0,100),bty="n",ylab="% smolts capturé aux pièges",xaxt="n",xlab="Année de migration")
axis(side=1,line=1,labels = years[-c(1,2)],at=1:data$Nyears, las=2, cex.axis=.7)

# Moulin des Princes
mcmc <- as.matrix(tmp[,paste0("p_MP[",1:data$Nyears,"]")])    
n <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))*100
segments(1:data$Nyears,n[,"2.5%"], 1:data$Nyears,n[,"97.5%"], col=paste0(mycol[1]))
lines(n[,"50%"],lty=1,lwd=2,col=mycol[1],type="o")
points(1:data$Nyears,n[,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))

# Moulin du Leslé
mcmc <- as.matrix(tmp[,paste0("p_ML[",3:data$Nyears,"]")])    
n <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))*100
n <- rbind(matrix(NA,2,3),n)
segments(1:data$Nyears,n[,"2.5%"], 1:data$Nyears,n[,"97.5%"], col=paste0(mycol[2]))
lines(n[,"50%"],lty=1,lwd=2,col=mycol[2],type="o")
points(1:data$Nyears,n[,"50%"],col=mycol[2],pch=21,bg=paste0(mycol[2]))

legend("topright", legend=c("Moulin des Princes", "Moulin du Leslé"), col=mycol[1:2],lty=1,lwd=3,bty="n")


## ----env, echo=FALSE----------------------------------------------------------

stade <- "smolt"
dir <-  paste(wdir,site,"/",stade,sep="")
#if (file.exists(dir)){
# load dataset
#load(paste(stade,"/data/data_",stade,"_",year,'.Rdata',sep="")) # chargement des données
#load(paste0(stade,"/results/Results_",stade,"_",year,".RData"))
#}

Q <- read.table(paste(dir,"/data/data_flow.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Q <- as.matrix(Q);mode(Q)<- "numeric"


#png(paste0(site,"/total_return.png"),width = 780, height = 480)
par(mfrow=c(1,1)) 

### Total number of returns
plot(NULL,xlim=c(1,data$Nyears),ylim=c(0,20),bty="n",ylab="Mean flow observed from 1st April to May 10",xaxt="n",xlab="")
axis(side=1,line=1,labels = years[-c(1,2)],at=1:data$Nyears, las=2, cex=.5)

lines(Q[,1],lty=1,lwd=2,col="steelblue1",type="o")

#legend("topright", legend=c("Moulin des Princes", "Moulin du Leslé"),col=mycol[1:2],lty=1,lwd=2,bty="n")



## ----parr, echo=FALSE---------------------------------------------------------

# TACONS

stade <- "tacon"
#dir <-  paste(wdir,site,"/",stade,sep="")
#if (file.exists(dir)){
# load dataset
load(paste(stade,"/data/data_",stade,"_",year,'.Rdata',sep="")) # chargement des données
load(paste0(stade,"/results/Results_",stade,"_",year,".RData"))
#}


tmp <- as.matrix(fit$sims.matrix)

#png(paste0(site,"/total_return.png"),width = 780, height = 480)
par(mfrow=c(1,1)) 

### Total number of returns
plot(NULL,xlim=c(1,data$Nyear),ylim=c(0,50000),bty="n",ylab=" Nombre total de tacons 0+",xaxt="n",xlab="Cohort")
axis(side=1,line=1,labels = years,at=1:data$Nyear, las=2, cex.axis=.7)

# N total
mcmc <- as.matrix(tmp[,paste0("ntot_Sc[",1:data$Nyear,"]")])    
n <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))
segments(1:data$Nyear,n[,"2.5%"], 1:data$Nyear,n[,"97.5%"], col=paste0(mycol[1]))
lines(n[,"50%"],lty=1,lwd=2,col=mycol[1],type="o")
points(1:data$Nyear,n[,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))


## ----parr-smolt survival, echo=FALSE------------------------------------------

setwd("/media/hdd4To/mbuoro/ORE-DiaPFC/Abundance/Scorff")


stade <- "smolt"
#dir <-  paste(wdir,site,"/",stade,sep="")
#if (file.exists(dir)){
# load dataset
load(paste(stade,"/data/data_",stade,"_",year,'.Rdata',sep="")) # chargement des données
load(paste0(stade,"/results/Results_",stade,"_",year,".RData"))

tmp <- as.matrix(fit$sims.matrix)
mcmc <- as.matrix(tmp[,paste0("Nc[",1:data$Nyears,",1]")]) # 1984 to now)
smolt <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))

stade <- "tacon"
load(paste(stade,"/data/data_",stade,"_",year,'.Rdata',sep="")) 
load(paste0(stade,"/results/Results_",stade,"_",year,".RData"))
parr <- as.matrix(fit$sims.matrix)
mcmc <- as.matrix(parr[,paste0("ntot_Sc[",1:data$Nyear,"]")]) # 1984 to now)
r <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))



labels=1994:2020
boxes <- sapply(nchar(labels), function(n) 
  paste(rep("\U2588", n), collapse=""))


#png(paste0(site,"/total_return.png"),width = 780, height = 480)
par(mfrow=c(1,1)) 

ratio <- smolt[1:28,"50%"]/r[2:29,"50%"]
plot(ratio,bty="n",ylab="Ratio Smolts/Tacons 0+",xlab="Cohort",xaxt="n",ylim=c(0,1))
axis(side=1,line=0,labels = 1994:2022,at=1:29, las=2, cex=.5)


  df <- data.frame(x=1:28, y= ratio)
  lw1 <- loess(y ~ x,span = 0.75, degree = 2, data=df)
  pred <- predict(lw1)
  lines(pred,col=1,lwd=2,lty=1)
  
  
  

### Total number of returns
plot(NULL,xlim=c(0,50000),ylim=c(0,20000),bty="n",ylab="Smolt 1+",xlab="Tacon 0+")
#axis(side=1,line=1,labels = years,at=1:data$Nyear, las=2, cex=.5)
#points(r[2:28,"50%"],smolt[1:27,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))
segments(r[2:28,"2.5%"],smolt[1:27,"50%"],r[2:28,"97.5%"],smolt[1:27,"50%"], col=paste0(mycol[1]))
segments(r[2:28,"50%"],smolt[1:27,"2.5%"],r[2:28,"50%"],smolt[1:27,"97.5%"], col=paste0(mycol[1]))
#text(r[2:28,"50%"],smolt[1:27,"50%"],labels=boxes, col="#CCCCCC99")
text(r[2:28,"50%"],smolt[1:27,"50%"],labels = years, cex=.7)




# plot(NULL,xlim=c(1,28),ylim=c(0,1),bty="n",ylab="Survie tacon - Smolt",xlab="Cohorte",xaxt="n")
# axis(side=1,line=0,labels = labels,at=1:27, las=2, cex.axis=.7)
# size <- r[2:28,"50%"]/10000
# points(1:27,smolt[1:27,"50%"]/r[2:28,"50%"],col=mycol[1],pch=21, bg=24,lwd=.5,cex=size)


# library(plotly)
# 
# df <- data.frame(parr=r[2:28,"50%"],smolt=smolt[1:27,"50%"])
# df$surv <- df$smolt/df$parr
# df$year <- 1995:2021
# 
# fig <- plot_ly(df, x = ~year, y = ~surv, text = ~year, type = 'scatter', mode = 'markers', color = ~parr, colors = 'Reds',marker = list(size = ~parr/1000, opacity = 0.5))
# fig <- fig %>% layout(title = 'Survie Tacon-Smolt',
#          xaxis = list(showgrid = FALSE),
#          yaxis = list(showgrid = FALSE))
# 
# fig


## ----SR, echo=FALSE-----------------------------------------------------------

stade <- "adult"
load(paste(stade,"/data/data_",stade,"_",year,'.Rdata',sep="")) 
load(paste0(stade,"/results/Results_",stade,"_",year,".RData"))
adu <- as.matrix(fit$sims.matrix)
mcmc <- as.matrix(adu[,paste0("e_tot[",1:data$Y,"]")]) # 1984 to now)
s <- t(apply(mcmc,2,quantile,probs=c(0.025, .5, 0.975)))


#png(paste0(site,"/total_return.png"),width = 780, height = 480)
par(mfrow=c(1,1)) 

### Total number of returns
plot(NULL,xlim=c(0,1000),ylim=c(0,50000),bty="n",ylab="Recruitment (tacon 0+)",xlab="Stock (escapted adults)")

#axis(side=1,line=1,labels = years,at=1:data$Nyear, las=2, cex=.5)
# Stock
segments(s[,"2.5%"], r[2:27,"50%"],s[,"97.5%"],r[2:27,"50%"], col=paste0(mycol[1]))
segments(s[,"50%"], r[2:27,"2.5%"],s[,"50%"],r[2:27,"97.5%"], col=paste0(mycol[1]))
points(s[1:26,"50%"],r[2:27,"50%"],col=mycol[1],pch=21,bg=paste0(mycol[1]))
text(s[1:26,"50%"],r[2:27,"50%"]+2000,labels = years, cex=.5)


