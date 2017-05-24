##-----------------------------INFO ----------------------------------##
year <- "2016"
site <- "Oir"
stade <- "tacon"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)

##-----------------------------DATA ----------------------------------##
fish <- read.bugsdata(paste("data/data_list.txt",sep=""))


##################################################################################################################################################
## Il s'agit ici des donn?es d'indice d'abondance pour les stations: IAS03; IAS06; IAS07; IAS09; IAS10
## Ces stations ont leur fichier de donn?es ? part des autres stations IAS car elles N'ont PAS servi dans la construction de la relation d'intercalibration
## /!\ Les donn?es de surface sont au 100m? car si elles sont en m?,il y a des probl?mes de mise ? jour car sampling dans de trop petites valeurs
## /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## /!\ Ces stations sont index?s de 2 mani?res diff?rentes: 
## 1: IAno_num : index concernant uniquement les stations suivies par IAS et 
## 2: Site_IA: index concernant toutes les stations ?chantillonn?es (donc pas seulement IAS)
##################################################################################################################################################
## Year_IA: index de l'ann?e
## CPUE_IAno: nombre de smolt captur?s lors des indices d'abondance pour une station donn?e 
## Site_IA: index de la station parmi toutes les stations ?chantillonn?es quelque soit la m?thode /!\ Bien reprendre les bonnes infos selon la station consid?r?e 
## IAno_num: index de la station IAS /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Srr_IA: surface de radiers rapides de la station ?chantillonn?e (en 100m?) /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Group_IA: index du groupe auquel appartient la station ?chantillonn?e /!\ Bien reprendre les bonnes infos selon la station consid?r?e
##################################################################################################################################################
tmp <- read.table(paste("data/data_IAS-autres.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
tmp <- as.matrix(tmp);mode(tmp)<- "numeric"
Year_IA <- tmp[,1]
CPUE_IAno <- tmp[,2]
Site_IA <- tmp[,3]
IAno_num <- tmp[,4]
Srr_IA <- tmp[,5]
Group_IA <- tmp[,6]


## Nombre de donn?es (nombre de lignes) pour les IA sur les sites qui ne servent pas pour la relation d'intercalibration
## IA concern?s: IAS03, IAS09, IAS06, IAS07, IAS10
## Devra ?tre mis ? jour
NIAno = nrow(tmp) # fait dans le script data_tacon.R


##################################################################################################################################################
## Il s'agit ici des tacons captur?s par enl?vements successifs
## Depuis 2009 (Year = 23), seuls les stations de type PTE et celles du ruisseau de la roche (RR...) sont encore suivies par enl?vement successifs
## /!\ Les donn?es de surface sont exprim?es en 100m? car si elles sont en m?,il y a des probl?mes de mise ? jour car sampling dans de trop petites valeurs
## /!\ Bien reprendre les bonnes infos (site, group, lieu, Srr et Spl) selon la station consid?r?e
##################################################################################################################################################
## Year est l'index de l'ann?e
## C1 est le nombre de tacons captur?s lors du premier passage selon la station consid?r?e
## C2 est le nombre de tacons captur?s lors du second passage selon la station consid?r?e
## Site est l'index de la station /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Group est l'index du groupe auquel appartient la station ?chantillonn?e /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Lieu est l'index de la rivi?re (1: Oir; 2: Ruisseau de la Roche; 3: Pont Levesque; 4: Moulin du bois). /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Srr est la surface de radiers rapides de la station ?chantillonn?e (en 100m?) /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Spl est la surface de plats de la station ?chantillonn?e (en 100m?) /!\ Bien reprendre les bonnes infos selon la station consid?r?e
##################################################################################################################################################  
tmp <- read.table(paste("data/data_enlevement-successif.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
tmp <- as.matrix(tmp);mode(tmp)<- "numeric"
Year <- tmp[,1]
C1 <- tmp[,2]
C2 <- tmp[,3]
Site <- tmp[,4]
Group <- tmp[,5]
Lieu <- tmp[,6]
Srr <- tmp[,7]
Spl <- tmp[,8]

## Nombre de donn?es (nombre de lignes) pour les pr?l?vements successifs (soit PE ou PTE)
## Devra ?tre mis ? jour
NPE1 = nrow(tmp) # fait dans le script data_tacon.R




##################################################################################################################################################
## Il s'agit ici des donn?es d'indice d'abondance pour les stations: IAS04; IAS05; IAS02; IAS08; IAS11; IAS12
## Ces stations ont leur fichier de donn?es ? part des autres stations IAS car elles ont servi dans la construction de la relation d'intercalibration
## /!\ Les donn?es de surface sont au 100m? car si elles sont en m?,il y a des probl?mes de mise ? jour car sampling dans de trop petites valeurs
## /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## /!\ Ces stations sont index?s de 2 mani?res diff?rentes: 
## 1: IAinter_num : index concernant uniquement les stations suivies par IAS et 
## 2: Site_IAnter: index concernant toutes les stations ?chantillonn?es (donc pas seulement IAS)
##################################################################################################################################################
## Year_IAinter: index de l'ann?e
## CPUE_inter: nombre de smolt captur?s lors des indices d'abondance pour une station donn?e
## IAinter_num: index de la station IAS /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Site_IAinter: index de la station parmi toutes les stations ?chantillonn?es quelque soit la m?thode /!\ Bien reprendre les bonnes infos selon la station consid?r?e 
## Srr_IAinter: surface de radiers rapides de la station ?chantillonn?e (en 100m?) /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Lieu_IAinter: index de la rivi?re (1: Oir; 2: Ruisseau de la Roche; 3: Pont Levesque; 4: Moulin du bois) /!\ Bien reprendre les bonnes infos selon la station consid?r?e
## Group_IAinter: index du groupe auquel appartient la station ?chantillonn?e /!\ Bien reprendre les bonnes infos selon la station consid?r?e
##################################################################################################################################################
tmp <- read.table(paste("data/data_IAS-intercalibration.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
tmp <- as.matrix(tmp);mode(tmp)<- "numeric"
Year_IAinter <- tmp[,1]
CPUE_inter <- tmp[,2]
IAinter_num <- tmp[,3]
Site_IAinter <- tmp[,4]
Srr_IAinter <- tmp[,5]
Lieu_IAinter <- tmp[,6]
Group_IAinter <- tmp[,7]


## Nombre de donn?es (nombre de lignes) pour les IA sur les sites servant pour la relation d'intercalibration
## IA concern?s: IAS04, IAS05, IAS02,IAS08,IAS11,IAS12
## Devra ?tre mis ? jour
NIAinter = nrow(tmp) # fait dans le script data_tacon.R





############################################################
# Ce fichier n'a pas besoin d'?tre mis ? jour
# W_Oir est la largeur moyenne en m?tre des stations suivies par IAS
############################################################# 
tmp <- read.table(paste("data/data_largeur-IAS.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
tmp <- as.matrix(tmp);mode(tmp)<- "numeric"
W_Oir <- tmp[,1]


#################################################################################################################################################################
## Ce fichier n'a pas besoin d'?tre mis ? jour
## Surface en 100m? de radier/rapide des stations ?chantillonn?es par enl?vement successif et qui ont servi dans la construction de la relation d'intercalibration				
##################################################################################################################################################
tmp <- read.table(paste("data/data_Srr-intercalibration.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
tmp <- as.matrix(tmp);mode(tmp)<- "numeric"
Srr_inter <- tmp[,1]

################################################################################
## Ce fichier n'a pas besoin d'?tre mis ? jour
## Surface au 100m? en radier/rapide et en plat pour chaque secteur (group) consid?r?				
################################################################################	
tmp <- read.table(paste("data/data_SRR-Spl_group.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
tmp <- as.matrix(tmp);mode(tmp)<- "numeric"
StotPC <- tmp[,1]
StotRR <- tmp[,2]




data <- c( fish, list(NIAno=NIAno,NIAinter=NIAinter,NPE1=NPE1,
                      Year_IA=Year_IA,
                      CPUE_IAno=CPUE_IAno,Site_IA=Site_IA,IAno_num=IAno_num,Srr_IA=Srr_IA,Group_IA=Group_IA
                      ,Year=Year,C1=C1,C2=C2,Site=Site,Group=Group,Lieu=Lieu,Srr=Srr,Spl=Spl
                      ,Year_IAinter=Year_IAinter,CPUE_inter=CPUE_inter,IAinter_num=IAinter_num,Site_IAinter=Site_IAinter,Srr_IAinter=Srr_IAinter,Lieu_IAinter=Lieu_IAinter
                      ,Group_IAinter=Group_IAinter
                      ,W_Oir=W_Oir,Srr_inter=Srr_inter,StotPC=StotPC,StotRR=StotRR
                      ))

save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
