#rm(list=ls())   # Clear memory

##-----------------------------DATA ----------------------------------##
# year <- 2015
# site <- "Scorff"
# stade <- "tacon"

## WORKING DIRECTORY:
# work.dir<-paste('~/Documents/RESEARCH/PROJECTS/ORE/Abundance/',site,sep="")
# setwd(work.dir)

load(paste('data/data_',stade,"_",year,'.Rdata',sep=""))


for (c in 1:2){ # 2 chains
#------------------------INITS----------------------------------##
inits0 <- read.bugsdata(paste("inits/init-",site,"-",stade,as.numeric(year)-1,".txt",sep=""))
#save(inits0,file=paste('inits/inits_',stade,as.numeric(year)-1,'.Rdata',sep=""))
#load(paste('inits/inits_',stade,as.numeric(year)-1,'.Rdata',sep=""))


###################################################
# NO UPDATE
###################################################
inits_fix <- list(
cauchy = runif(1,1,2)#1.757
,coef_PC = runif(1,.1,.5)#0.1979
,int_width = runif(1,-1,-.2)#-0.7736
,mu_ydOir = c(2.413,1.601,1.85,0.3417)
,mup_rem = runif(1,0,1)#0.8043 
,rate_lcpu = runif(1,0,1)#0.7617
,sigma_dOir = runif(1,0,1)#0.7211
,sigma_gryrOir = runif(1,.5,1)#0.6553
,sigma_yOir = runif(1,1,3)#2.066
,width_coef = runif(1,.1,1)#0.8582
)

###################################################
# TO UPDATE
###################################################
#gryr_Oir <- inits0$gryr_Oir
# ajouter 13 "0" à la fin
gryr_Oir_inits <- c(inits0$gryr_Oir, rep(0, 13))

lambdaOir_cpu <- inits0$lambdaOir_cpu
CPUE_IAno <- data$CPUE_IAno # extraire les 5 dernières valeurs
CPUE_inter <- data$CPUE_inter # extraire les 6 dernières valeurs
lambdaOir_cpu_tmp <- c(tail(CPUE_IAno,5), tail(CPUE_inter,6)) +1
lambdaOir_cpu_inits <- as.matrix(cbind(lambdaOir_cpu, lambdaOir_cpu_tmp))

log_dOir <- inits0$log_dOir
vect1 <- rep(NA,34) # 34 premeirs sont des NA
vect2 <- log(((tail(data$C1,23)*2)+10) / (tail(data$Srr,23)+ 0.2*tail(data$Spl,23)))
vect3 <- log(lambdaOir_cpu_tmp)
log_dOir_tmp <- c(vect1,vect2,vect3)
log_dOir_inits <- as.matrix(cbind(log_dOir,log_dOir_tmp))

lp_remgr_inits <- as.matrix(cbind(inits0$lp_remgr,rep(1,13)))

n_LR_inits <- as.integer(c(inits0$n_LR, (.2*data$StotPC[13] + data$StotRR[13])*exp(mean(log_dOir_tmp[35:54]))))

n_MB_inits <- as.integer(c(inits0$n_MB,1000))

n_Oir_gr_inits <- as.matrix(cbind(inits0$n_Oir_gr,as.integer(mean(head(lambdaOir_cpu_tmp,9))*(.2*data$StotPC[1:10] + data$StotRR[1:10]))))

n_PL_inits <- as.integer(c(inits0$n_PL, lambdaOir_cpu_tmp[10]*(.2*data$StotPC[13] + data$StotRR[13])))


vect1 <- rep(NA,34) # 34 premeirs sont des NA
vect2 <- (tail(data$C1,23)*2)+10
vect3 <- lambdaOir_cpu_tmp*2
ntot_tmp <- c(vect1,vect2,vect3)
ntot_inits <- as.matrix(cbind(inits0$ntot,ntot_tmp))

year_dOir_inits <- c(inits0$year_dOir, 0)


C1_inits <- c(inits0$C1, rep(NA,23))


tmpC2 <- tail(data$C2,23) 
tmpC1 <- tail(data$C1,23)
tmp=rep(NA,length(tmpC2))
for (i in 1:length(tmp)){
  if(is.na(tmpC2[i])) tmp[i] <- as.integer(.2*tmpC1[i])
  if(!is.na(tmpC2[i])) tmp[i] <- NA
}
C2_inits <- c(inits0$C2, tmp)
  
  
inits_updated <- list(
  gryr_Oir = gryr_Oir_inits
  , lambdaOir_cpu = lambdaOir_cpu_inits
  , log_dOir = log_dOir_inits
  , lp_remgr = lp_remgr_inits
  , n_LR = n_LR_inits
  , n_MB=n_MB_inits
  , n_Oir_gr=n_Oir_gr_inits
  , n_PL=n_PL_inits
  , ntot=ntot_inits
  , year_dOir=year_dOir_inits
  , C1=C1_inits
  , C2=C2_inits
)

inits <- list(c( inits_fix,inits_updated))

#write(inits[[1]],file=paste("inits/init-",site,"-",stade,year,".txt",sep=""), append=TRUE)
#lapply(inits[[1]], write, paste("inits/init-",site,"-",stade,year,".txt",sep=""), append=TRUE)
#save(inits,file=paste('inits/inits_',stade,'.Rdata',sep=""))
#save(inits,file=paste('inits/inits_',stade,year,'.Rdata',sep=""))
#bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,'.txt',sep=""))
bugs.inits(inits, n.chains=1,digits=3, inits.files = paste('inits/init-',site,'-',stade,year,"_",c,'.txt',sep=""))
} #end loop c
