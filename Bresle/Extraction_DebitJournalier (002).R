invisible(lapply(c("data.table", "httr", "jsonlite"),function(pk){
  if(!pk %in% row.names(installed.packages())){install.packages(pk)}
  library(pk,character.only=T)}))


# regarder: https://hubeau.eaufrance.fr/sites/default/files/api/demo/hydro/index.htm
# Numero_Stations <- c("I925301001","I928000101") code Oir
# Numero_Stations <- c("G017042010") # code Bresle (a partir de 2000)
# Numero_Stations <- c("G111041010") # code Yères (de 1978 à 1999 pour la Bresle)
# Numero_Stations <- c("G017042010") # code Bresle (a partir de 2000)
#Numero_Stations <- c("G111041010", "G017042010") 

for (NumStat in Numero_Stations){
  Station <- data.table()
  extract <- fromJSON(paste0("https://hubeau.eaufrance.fr/api/v1/hydrometrie/obs_elab?code_entite=", NumStat,
                             "&grandeur_hydro_elab=QmJ", sep = ""))
  while(nrow(Station) != extract$count){
    Station <- rbind(Station,extract$data)
    if(!is.null(extract$`next`)) {extract <- fromJSON(extract$`next`)}
  }
  Station[, date := as.Date(date_obs_elab)][, `:=`(year = format(date, "%Y"), month = format(date, "%m"), day = format(date, "%d"))]
  Station <- Station[year %in% startyear:endyear]
  
  assign(x = paste0("QmJ_", NumStat), value = Station)
  write.table(get(paste0("QmJ_", NumStat)), file = paste0("data/DebitJournalier_",NumStat,"_",startyear,"_",endyear,".csv"), sep = ";", row.names = F)
}


