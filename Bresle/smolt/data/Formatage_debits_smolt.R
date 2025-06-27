# TODO: Add comment
# 
# Author: CTréhin from lbeaulaton
# Format flow data for abundance estimation model for Bresle TRM 
#
# 10.03.2023
###############################################################################


## data
work.dir <- "/media/HDD12To/mbuoro/ORE-DiaPFC/Abundance/Bresle/"

# apres 2000 sur la bresle: 
Numero_Stations <- c("G017042010") # code Bresle (a partir de 2000)
startyear <- 2000
endyear <- year
source(paste0(work.dir,"Extraction_DebitJournalier (002).R")) # actualiser les donnees
debit_bresle <- read.csv(paste("data/DebitJournalier_",Numero_Stations,"_",startyear,"_",endyear,".csv",sep=""), sep = ";", dec= ".") ## load data

# avant 2000 sur l'hyere: 
Numero_Stations <- c("G111041010") # code Yères (de 1978 à 1999 pour la Bresle)
start_year=1982
startyear <- start_year
endyear <- 1999
source(paste0(work.dir,"Extraction_DebitJournalier (002).R")) # actualiser les donnees
debit_yeres <- read.csv(paste("data/DebitJournalier_",Numero_Stations,"_",startyear,"_",endyear,".csv",sep=""), sep = ";", dec= ".") ## load data

# apres 2000 sur l'yeres: 
startyear <- 2000
endyear <- year
source(paste0(work.dir,"Extraction_DebitJournalier (002).R")) # actualiser les donnees
debit_yeres_recent <- read.csv(paste("data/DebitJournalier_",Numero_Stations,"_",startyear,"_",endyear,".csv",sep=""), sep = ";", dec= ".") ## load data


# format
debit_yeres$date <- as.Date(debit_yeres$date, format = "%Y-%m-%d")
debit_bresle$date <- as.Date(debit_bresle$date, format = "%Y-%m-%d")
debit_yeres_recent$date <- as.Date(debit_yeres_recent$date, format = "%Y-%m-%d")

####################
# SMOLTS

# Debit moyen sur l'Yeres sur la periode de migration (1er avril au 10 mai) pour la période 

period1 <- c(start_year:1999)
debit_moyen_Ye_p1 <- c()

for(i in period1){
  select_year <- subset(debit_yeres, debit_yeres$year == i)
  
  select_year$date <- as.Date(select_year$date, format = "%Y-%m-%d")
  
  select_period <- subset(select_year, select_year$date >= as.Date(paste(i,"-04-01",sep=""), format = "%Y-%m-%d") &
                            select_year$date <= as.Date(paste(i,"-05-10",sep=""), format = "%Y-%m-%d"))
  
  debit_moyen_Ye_p1 <- c(debit_moyen_Ye_p1, mean(select_period$resultat_obs_elab))
}


# Debit moyen sur la Bresle sur la periode de migration (15 mars au 15 mai)

period2 <- c(2000:year)
debit_moyen_Br_p2 <- c()

for(i in period2){
  select_year <- subset(debit_bresle, debit_bresle$year == i)
  
  select_year$date <- as.Date(select_year$date, format = "%Y-%m-%d")
  
  select_period <- subset(select_year, select_year$date >= as.Date(paste(i,"-04-01",sep=""), format = "%Y-%m-%d") &
                            select_year$date <= as.Date(paste(i,"-05-10",sep=""), format = "%Y-%m-%d"))
  
  debit_moyen_Br_p2 <- c(debit_moyen_Br_p2, mean(select_period$resultat_obs_elab))
}



# Debit moyen sur l'Yeres sur la periode de migration (15 mars au 15 mai)

debit_moyen_Ye_p2 <- c()

for(i in period2){
  select_year <- subset(debit_yeres_recent, debit_yeres_recent$year == i)
  
  select_year$date <- as.Date(select_year$date, format = "%Y-%m-%d")
  
  select_period <- subset(select_year, select_year$date >= as.Date(paste(i,"-04-01",sep=""), format = "%Y-%m-%d") &
                            select_year$date <= as.Date(paste(i,"-05-10",sep=""), format = "%Y-%m-%d"))
  
  debit_moyen_Ye_p2 <- c(debit_moyen_Ye_p2, mean(select_period$resultat_obs_elab))
}


# Reconstitution du débit de la Bresle sur la période 1984-1999

d <- data.frame(y = log(debit_moyen_Br_p2), x = log(debit_moyen_Ye_p2))
gflow_lm <- ggplot(data = d, aes(y=y , x=x))+
  geom_point(color="green3")+
  geom_smooth(method = lm,color="green4")+
  labs(x="Log débit Yeres", y = "Log débit Bresle")
gflow_lm

summary(lm(log(debit_moyen_Br_p2)~log(debit_moyen_Ye_p2))) #log
summary(lm(debit_moyen_Br_p2~debit_moyen_Ye_p2))

mod <- lm(log(debit_moyen_Br_p2)~log(debit_moyen_Ye_p2))

mod$coefficients[1]
mod$coefficients[2]

log_debit_moyen_Br_p1 <- mod$coefficients[1] + log(debit_moyen_Ye_p1)*mod$coefficients[2]

debit_moyen_Br_p1 <- exp(log_debit_moyen_Br_p1)

# Format tableau final

debit_moyen <- c(debit_moyen_Br_p1,debit_moyen_Br_p2)

data_flow <- data.frame(Q_Eu = debit_moyen, annee = c(period1,period2))

data_flow$type <- "debit brut"
data_flow$type[data_flow$annee %in% period1] <- "debit reconstitué"

gflow <- ggplot(data=data_flow, aes(x=annee,y=Q_Eu,linetype=type))+
  geom_line(color="#00BA38", size = 1.2)+
  labs(y= "Q (m3/j)")+
  theme(axis.text = element_text(size=12))
gflow

# Fichier: data_flow


file_path = paste('data/data_flow_',stade,'.txt',sep="")

my_txt <- c ( '#### Debit pour la Bresle (l/s). Moyenne sur la periode 1 Avril - 10 mai	', 
              '#### Covariate is standardized within WinBUGS'
)
bugs.data(
  list(Q_Beau = data_flow$Q_Eu,
       Q_Eu = data_flow$Q_Eu),
  data.file=file_path)

write(my_txt, file_path, append = T) 


