##-----------------------------INFO ----------------------------------##
# year <- "YEAR"
# site <- "SITE"
# stade <- "STADE"

## WORKING DIRECTORY:
#work.dir<-paste("Rep",site,stade,sep="/")
#setwd(work.dir)



##-----------------------------DATA ----------------------------------##
fish <- read.bugsdata(paste("data/data_list_CPUE.txt",sep=""))

### Intercalibration Martin/Pulsium
## En ligne, les 6 stations d'intercalibration et les années en colonne (2020 à Y)
CPUE_Sc_inter <- read.table(paste("data/data_inter_MP_Pulsium.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
CPUE_Sc_inter <- as.matrix(CPUE_Sc_inter);mode(CPUE_Sc_inter)<- "numeric"


##################################################################################################################################################																						
## Il s'agit ici de la largeur (en m?tre) au niveau des stations IA avec les stations en ligne et les ann?es en colonne
## Les largeurs ont ?t? mesur?es par Nicolas Jeannot en Juin 2015
## /!\ Ne pas modifier l'ordre des lignes.
## /!\ LA LARGEUR DES DEUX DERNIERES LIGNES (2 STATIONS DU RUISSEAU DE CARADEC) EST POUR LE MOMENT UNE VALEUR HYPOTHETIQUE DE 3M
## /!\ BESOIN DE METTRE A JOUR LA LARGEUR REELLE DE CES STATIONS 
## VOIR RAPPORT SERVANTY & PREVOST (2016) POUR PLUS D'INFORMATIONS (NOTAMMENT TABLEAU 18, P.85)																					
## Mettre ? jour en reprenant les infos de la derni?re colonne														
################################################################################################################################################
#W_Sc <- read.table(paste("data/data_width_CPUE.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
#W_Sc <- as.matrix(W_Sc);mode(W_Sc)<- "numeric"
W_Sc <- read.csv("data/data_width_CPUE.csv",sep=";", comment.char = "#")
W_Sc <- as.matrix(W_Sc);mode(W_Sc)<- "numeric"
##################################################################################################################################################																						
## Il s'agit ici des donn?es de surface ?quivalent radier-rapide (au 100m?) pour chaque station avec les stations en ligne et les ann?es en colonne.																						
## /!\ Si elles sont mises en m?,il y a des probl?mes de mise ? jour car sampling dans de trop petites valeurs	
## /!\ CE FICHIER EST DEJA MIS A JOUR POUR L'ANNEE 2015: certains affluents sont d?sormais colonis?s
## VOIR RAPPORT SERVANTY & PREVOST (2016) POUR PLUS D'INFORMATIONS (NOTAMMENT TABLEAU 18, P.85)																					
## A partir de l'ann?e 2016, mettre ? jour en reprenant les infos de la derni?re colonne																						
## /!\ Ne pas modifier l'ordre des lignes.																						
################################################################################################################################################
#S_Sc <- read.table(paste("data/data_SeqRR100_CPUE.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
#S_Sc <- as.matrix(S_Sc);mode(S_Sc)<- "numeric"
S_Sc <- read.csv("data/data_SeqRR100_CPUE.csv",sep=";", comment.char = "#")
S_Sc <- as.matrix(S_Sc);mode(S_Sc)<- "numeric"

##################################################################################################################################################
## Donn?es d'indice d'abondance sur le Scorff et ses affluents avec les stations en ligne et les ann?es en colonne
## /!\ Ne pas modifier l'ordre des lignes. Les lignes 52 et 53 (Ruisseau Caradec) ont commenc?es ? ?tre ?chantillonn?es en 2015 et sont d?j? incluses dans le mod?le.
################################################################################################################################################
CPUE_Sc <- read.table(paste("data/data_CPUE.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
CPUE_Sc <- as.matrix(CPUE_Sc);mode(CPUE_Sc)<- "numeric"

##################################################################################################################################################
## PAS DE MISE A JOUR
## Il s'agit ici des donn?es d'intercalibration (donn?es de l'article de Pr?vost & Nihouarn 1999)
## C1 is the number fo fish captured during the first pass of successive removals				
## C2 is the number fo fish captured during the second pass of successive removals				
## S is the surface of each station in 100 m? euivalent rapid				
## CPUE is the number of fish captured during CPUE
## Width is the mean width of each station in meter		
##################################################################################################################################################
inter_cali <- read.table(paste("data/data_intercalibration.txt",sep=""),header = TRUE, check.names=FALSE,comment.char = "#",colClasses="character")
inter_cali <- as.matrix(inter_cali);mode(inter_cali)<- "numeric"
C1 <- inter_cali[,1]
C2 <- inter_cali[,2]
S <- inter_cali[,3]
CPUE <- inter_cali[,4]
Width <- inter_cali[,5]

##################################################################################################################################################
# PAS DE MISE A JOUR
# Il s'agit ici des stations qui ont ?t? ?chantillonn?es ? la fois par IA et par enl?vements successifs
# Ces stations servent pour construire la relation d'intercalibration
##################################################################################################################################################
Ninter <- read.bugsdata(paste("data/data_list_intercalibration.txt",sep=""))


data <- c( fish, list(W_Sc=W_Sc, S_Sc=S_Sc,CPUE_Sc=CPUE_Sc, CPUE_Sc_inter=CPUE_Sc_inter,C1=C1,C2=C2,S=S,CPUE=CPUE,Width=Width),Ninter)
save(data,file=paste('data/data_',stade,"_",year,'.Rdata',sep=""))
