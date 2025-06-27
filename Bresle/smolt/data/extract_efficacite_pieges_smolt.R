# Author: CTrehin 
# Extract capture efficiency from dbpoisson
#
# 10.03.2023
###############################################################################

# connection base
#source(paste("~/INRAE/DCF_U3E/Base de donnée/Git/abondance_trm",site,'scripts_generaux/connection_base.R',sep="/"))
source("/media/HDD12To/mbuoro/ORE-DiaPFC/Abundance/AccesDB.R")
#### Pourcentages de fonctionnement du piege

## Piege de Beauchamps (descente)

requete = paste("
with sel as (select * from bresle.t_campagne_cam 
where 
	cam_sec_id = 2  and
	extract(year from cam_date_heure_fin) > 1980 and 
	extract(year from cam_date_heure_fin) < ",as.numeric(year)+1," and 
	((extract(month from cam_date_heure_fin) = 4 and extract(day from cam_date_heure_fin) >= 01) or
	(extract(month from cam_date_heure_fin) = 5 and extract(day from cam_date_heure_fin) <= 10)	or
	extract(month from cam_date_heure_fin) =4)
	and cam_nfo_id not in (2,3,5,7) ),
sel2 as (select 
	extract(year from cam_date_heure_fin) as annee ,
	count(distinct Cast(cam_date_heure_fin as date)) as nb_jours
from sel
group by annee)
select *, 
(cast(nb_jours as float8)) /(max(cast(nb_jours as numeric)) over () ) as efficacite
from sel2
group by annee, nb_jours; 
",sep="")
eff_LD <- dbGetQuery(db_con,requete)

#eff_LD <- eff_LD[-which(eff_LD$annee %in% c(1991,2001)),] #correction specifique

## Piege de Eu (descente)

requete = paste("
with sel as (select * from bresle.t_campagne_cam 
where 
	cam_sec_id = 1 and
	extract(year from cam_date_heure_fin) > 1980 and 
	extract(year from cam_date_heure_fin) < ",as.numeric(year)+1, " and 
	((extract(month from cam_date_heure_fin) = 4 and extract(day from cam_date_heure_fin) >= 01) or
	(extract(month from cam_date_heure_fin) = 5 and extract(day from cam_date_heure_fin) <= 10)	or
	extract(month from cam_date_heure_fin) =4)
	and cam_nfo_id not in (2,3,5,7)),
sel2 as (select 
	extract(year from cam_date_heure_fin) as annee ,
	count(distinct Cast(cam_date_heure_fin as date)) as nb_jours
from sel
group by annee)
select *, 
(cast(nb_jours as float8)) /(max(cast(nb_jours as numeric)) over () ) as efficacite
from sel2
group by annee, nb_jours; 
",sep="")
  
eff_Eu <- dbGetQuery(db_con,requete)
#eff_Eu <- eff_Eu[-which(eff_Eu$annee %in% c(2001)),] #correction specifique

dbDisconnect(db_con)# stop connection

### graphe

eff_LD <- merge(data.frame(annee = c(start_year:year)),eff_LD, all.x = T)
eff_LD[is.na(eff_LD)] <- 0

eff_Eu <- merge(data.frame(annee = c(start_year:year)),eff_Eu, all.x = T)
eff_Eu[is.na(eff_Eu)] <- 0

eff_LD$piege = "Beauchamp"
eff_Eu$piege = "Eu"

eff_tot <- rbind(eff_LD,eff_Eu)

geff <- ggplot(data= eff_tot, aes(x= annee, y =efficacite, col = piege))+
  geom_line(size = 1)+
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12),
        legend.text = element_text(size = 11), legend.title = element_text(size = 12))
geff

## sauvegarde

#Beauchamp
file_path = 'data/Eff_LD-PGD.txt'
my_txt <- c ( '######################################################################################', 
              '##  eff_B[t]: Annual capture efficiency at Beauchamp, calculated as the ratio between the number of functioning days and the maximum possible functioning days during the migration period (April 1st to May 10th)',
              '######################################################################################',
              'annee  eff_B[]'
)

writeLines (my_txt, file_path) 
write.table(eff_LD[,c("annee","efficacite")], file = file_path,sep="\t",row.names = F, col.names = F, append=T)



#Eu
file_path = 'data/Eff_Eu-PGD.txt'
my_txt <- c ( '######################################################################################', 
              '##  eff_Eu[t]: Annual capture efficiency at Eu, calculated as the ratio between the number of functioning days and the maximum possible functioning days during the migration period (April 1st to May 10th)',
              '######################################################################################',
              'annee  eff_Eu[]'
)

writeLines (my_txt, file_path) 
write.table(eff_Eu[,c("annee","efficacite")], file = file_path,sep="\t",row.names = F, col.names = F, append=T)


###################################################################################################

# Graphe fonctionnement par jour

source(paste("..",'scripts_generaux/graphe_fonctionnement.R',sep="/"))

tr_fonctionnement_nature_nfo <- dbGetQuery(db_con,"select * from public.tr_fonctionnement_nature_nfo")

nfo_description<-as.character(tr_fonctionnement_nature_nfo$nfo_description)
color_des<-  c("#FFFF99","#FDBF6F","#E31A1C","#FB9A99","#1F78B4","#B15928","#B2DF8A")

color_fonct = setNames(color_des,nfo_description)


## Piege de Beauchamps (descente)

requete = paste("
select cam_sec_id,
  sec_code,
  cam_date_heure_fin, 
  cam_nfo_id,
  nfo_description,
  extract(year from cam_date_heure_fin) as saison
  from bresle.t_campagne_cam 
  left join bresle.tr_secteur_sec on cam_sec_id = sec_id
  left join public.tr_fonctionnement_nature_nfo on cam_nfo_id = nfo_id
where 
	cam_sec_id = 2 and
	extract(year from cam_date_heure_fin) > 1980 and 
	extract(year from cam_date_heure_fin) < ",as.numeric(year)+1," and 
	((extract(month from cam_date_heure_fin) = 4 and extract(day from cam_date_heure_fin) >= 1) or
	(extract(month from cam_date_heure_fin) = 5 and extract(day from cam_date_heure_fin) <= 10)	or
	extract(month from cam_date_heure_fin) =4)
; 
",sep="")

eff_LD_jours <- dbGetQuery(db_con,requete)

eff_LD_jours$jour_standard <- format(eff_LD_jours$cam_date_heure_fin, "%j")
eff_LD_jours$jour_standard <- as.Date(eff_LD_jours$jour_standard,"%j")

graph_LD_jours <- graph_on_off(eff_LD_jours,unique(eff_Eu_jours$cam_sec_id),unique(eff_Eu_jours$sec_code),annee_min = start_year, annee_max = year)
graph_LD_jours


## Piege de Eu (descente)

requete = paste("
select cam_sec_id,
  sec_code,
  cam_date_heure_fin, 
  cam_nfo_id,
  nfo_description,
  extract(year from cam_date_heure_fin) as saison
  from bresle.t_campagne_cam 
  left join bresle.tr_secteur_sec on cam_sec_id = sec_id
  left join public.tr_fonctionnement_nature_nfo on cam_nfo_id = nfo_id
where 
	cam_sec_id = 1 and
	extract(year from cam_date_heure_fin) > 1980 and 
	extract(year from cam_date_heure_fin) < ",as.numeric(year)+1, " and 
	((extract(month from cam_date_heure_fin) = 4 and extract(day from cam_date_heure_fin) >= 01) or
	(extract(month from cam_date_heure_fin) = 5 and extract(day from cam_date_heure_fin) <= 10)	or
	extract(month from cam_date_heure_fin) =4)
  ; 
",sep="")

eff_Eu_jours <- dbGetQuery(db_con,requete)

eff_Eu_jours$jour_standard <- format(eff_Eu_jours$cam_date_heure_fin, "%j")
eff_Eu_jours$jour_standard <- as.Date(eff_Eu_jours$jour_standard,"%j")

graph_Eu_jours <- graph_on_off(eff_Eu_jours,unique(eff_Eu_jours$cam_sec_id),unique(eff_Eu_jours$sec_code),annee_min = start_year, annee_max = year)
graph_Eu_jours
