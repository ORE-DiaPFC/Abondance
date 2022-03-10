##-----------------------------INFO ----------------------------------##
#year <- "2020"
site <- "Nivelle"
stade <- "adult"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)



##-----------------------------DATA ----------------------------------##
# Nombre d'années suivies:
years <- read.bugsdata(paste("data/data_list.txt",sep="")) # Nombre d'années
#years <- read.bugsdata(paste("data/data_list_",year,".txt",sep="")) # Nombre d'années
Y <- years$Y

################################################################################
### THIS FILE DOES NOT NEED TO BE UPDATED
## R_EF[t,g]: Annual number of fish removed for experiment by electric fishing between Uxondoa and Olha per breeding category (from 1984 to 1991);  1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW
## M_EF[t,g]: Annual number of fish removed between Uxondoa and Olha and moved upstream Olha per breeding category (1990 & 1991);  1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW
## Cm_EF[t,g]: Annual number of marked fish captured by EF, angling or found dead between Uxondoa and Olha per breeding category (from 1984 to 1991); 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW 
## Cum_EF[t,g]: Annual number of unmarked fish captured by EF, angling or found dead between Uxondoa and Olha per breeding category (from 1984 to 1991); 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW 
################################################################################
EF <- read.table(paste("data/data_EF.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
EF <- as.matrix(EF);mode(EF)<- "numeric"
R_EF=as.matrix(EF[,1:4])
M_EF=as.matrix(EF[,5:8])
Cm_EF=as.matrix(EF[,9:12])
Cum_EF=as.matrix(EF[,13:16])

################################################################################
## OMEGA[t,z]: annual redd count per zone. 1/ LN1 (from Ascain to Uxondoa), 2/ LN2 (from Uxondoa to Olha), 4/ UN, 5/ LUR
## /!\ Zone 3 does not exist
################################################################################
REDDS <- read.table(paste("data/data_REDDS.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
REDDS <- as.matrix(REDDS);mode(REDDS)<- "numeric"
OMEGA<-matrix(NA,nrow = years$Y, ncol = 5)
OMEGA[,1]<-REDDS[,1]
OMEGA[,2]<-REDDS[,2]
OMEGA[,4]<-REDDS[,3]
OMEGA[,5]<-REDDS[,4]


### Q[t]: annual mean December flow (m3/S)
## Covariate is standardized within BUGS
Q <- read.table(paste("data/data_Q.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Q <- as.numeric(as.vector(Q[,1]))

### Eff_Ux is the ratio of the number of trapping nights over the mean number of nights of trapping from 1984 to 2011 (=302 days)
## eff_Ol is the ratio of the number of trapping nights over the mean number of nights of trapping from 1994 to 2011 (excluded 1992,1993,2000, mean = 294)
eff <- read.bugsdata(paste("data/data_eff.txt",sep=""))
eff_Ux <- eff$eff_Ux 
eff_Ol <- eff$eff_Ol

################################################################################																															
## D_11[t,g]: Annual number of dead fish between Ascain and Uxondoa per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"																															
## D_12[t,g]: Annual number of dead fish between Uxondoa and Olha per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"																															
## Dm_12[t,g]: Annual number of dead fish with a mark between Uxondoa and Olha per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"																															
##   = D_12[t,g] except for the first years (until 1991)																															
## Dum_12[t,g]: Annual number of dead fish without a mark at Olha per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW. Start in 1992"																															
## A_11[t,g]: Annual number of fish caught by angling between Ascain and Uxondoa per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"																															
## A_12[t,g]: Annual number of fish caught by angling between Uxondoa and Olha per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"																															
## Am_12[t,g]: Annual number of marked fish caught by angling between Uxondoa and Olha per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"																															
##   = A_12[t,g] except for the first years (until 1991)																															
## Aum_12[t,g]: Annual number of unmarked fish caught by angling between Uxondoa and Olha per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"																															
################################################################################																															
dead <- read.table(paste("data/data_dead.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
dead <- as.matrix(dead);mode(dead)<- "numeric"
D_11=as.matrix(dead[,1:4])
D_12=as.matrix(dead[,5:8])
Dm_12=as.matrix(dead[,9:12])
Dum_12=as.matrix(dead[,13:16])
A_11=as.matrix(dead[,17:20])
A_12=as.matrix(dead[,21:24])
Am_12=as.matrix(dead[,25:28])
Aum_12=as.matrix(dead[,29:32])

################################################################################							
## Cm_O[t,g]: Annual number of marked fish captured at Olha per breeding category since 2012 (partial trapping);  1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"							
## Cum_O[t,g]: Annual number of unmarked fish captured at Olha per breeding category since 2012 (partial trapping);  1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"							
################################################################################							
Ol <- read.table(paste("data/data_Ol.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Ol <- as.matrix(Ol);mode(Ol)<- "numeric"
Cm_O=as.matrix(Ol[,1:4])
Cum_O=as.matrix(Ol[,5:8])

################################################################################
## C_U[t,g]: Annual number of fish captured at Uxondoa per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW
## Cm_U[t,g]: Annual number of fish captured at Uxondoa and released with mark per breeding category = C_U[t,g] except for 1993 and 1994 ; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW
## Cum_U[t,g]: Annual number of fish captured at Uxndoa but released unmarked (happened only in 1993 and 1994) per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW
################################################################################
Ux <- read.table(paste("data/data_Ux.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
Ux <- as.matrix(Ux);mode(Ux)<- "numeric"
C_U=as.matrix(Ux[,1:4])
Cum_U=as.matrix(Ux[,5:8])
Cm_U=as.matrix(Ux[,9:12])

################################################################################
## ech_1SW_tot[t]: annual number of captured individuals 1SW (sum of 1R/1SW and 2R/1SW)
## ech_1.1SW[t]: annual number of captured individuals 1R/1SW
## ech_MSW_tot[t]: annual number of captured individuals MSW (1R/2SW, 2R/2SW, 1R3SW, 2R3SW, second spawning)
## ech_MSW[,1]: annual number of captured individuals 1R/2SW
## ech_MSW[,2]: annual number of captured individuals 2R/2SW
## ech_MSW[,3]: annual number of captured individuals 1R3SW
## ech_MSW[,4]: annual number of captured individuals 2R3SW
## ech_MSW[,5]: annual number of captured individuals second spawning
################################################################################
age <- read.table(paste("data/data_age.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
age <- as.matrix(age);mode(age)<- "numeric"
ech_1SW_tot=age[,1]
ech_1.1SW=age[,2]
ech_MSW_tot=age[,3]
ech_MSW=as.matrix(age[,4:8])

################################################################################			
### UPDATE WITH NA			
## e_2[t,g]: annual breeding escapement in the high catchment zone (UN + LUR) per breeding category; 1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW"			
##    = number of fish released upstream Olha = NA until 1989, in 2000 and since 2012			
################################################################################			
EHC <- read.table(paste("data/data_EHC.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
EHC <- as.matrix(EHC);mode(EHC)<- "numeric"
e_2=EHC

################################################################################
## NEED TO UPDATE WITH NA
## nm_2[t,g]: Annual number of marked fish captured at Olha per breeding category (when trapping is total, globally from 92 to 2011);  1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW
## num_2[t,g]: Annual number of unmarked fish captured at Olha per breeding category (when trapping is total, globally from 92 to 2011);  1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW
################################################################################
nm_num <- read.table(paste("data/data_nm-num_2.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
nm_num <- as.matrix(nm_num);mode(nm_num)<- "numeric"
nm_2=as.matrix(nm_num[,1:4])
num_2=as.matrix(nm_num[,5:8])

################################################################################
## NB[t,g]: annual breeding escapement per breeding category in the high catchment zone added or removed in the HC zone;  1: male 1SW; 2: female 1SW; 3: male MSW; 4: female MSW 
## = sum of the number of fish removed from the population and the number of fish released upstream Olah. Happened in 2000 and since 2012.
################################################################################
NB <- read.table(paste("data/data_NB.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
NB <- as.matrix(NB);mode(NB)<- "numeric"



data <- list( Y=Y
              ,R_EF=R_EF,M_EF=M_EF
              ,Cm_EF=Cm_EF,Cum_EF=Cum_EF
              ,OMEGA=OMEGA
              ,Q=Q
              ,eff_Ux=eff_Ux,eff_Ol=eff_Ol
              ,D_11=D_11,D_12=D_12
              ,Dm_12=Dm_12,Dum_12=Dum_12
              ,A_11=A_11,A_12=A_12
              ,Am_12=Am_12,Aum_12=Aum_12
              ,Cm_O=Cm_O,Cum_O=Cum_O
              ,C_U=C_U,Cum_U=Cum_U,Cm_U=Cm_U
              ,ech_1SW_tot=ech_1SW_tot,ech_1.1SW=ech_1.1SW,ech_MSW_tot=ech_MSW_tot,ech_MSW=ech_MSW
              ,e_2=e_2
              ,nm_2=nm_2,num_2=num_2
              ,NB=NB)

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep="")) # sauvegarde des données
bugs.data(data,digits=3, data.file = paste0('data/data_',stade,"_",year,'.txt'))
