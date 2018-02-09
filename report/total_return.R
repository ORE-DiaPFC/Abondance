

## ADULTS
year=2016
sites <- c("Bresle", "Oir","Nivelle", "Scorff")
stade <- "adult"

# Nivelle: start 1984
# Oir: 1984
# Bresle: 1984 to now)
years <- seq(1984, year, 1)
res=list()
total=NULL
 for (site in sites){
   table <- array(, dim=c(length(years), 4))
   colnames(table) <- c( "Year of return","n_1SW", 	"n_MSW",	"n_tot")
   rownames(table) <- years
   
   # load results
    load(paste0("~/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abundance/",site,"/",stade,"/results/Results_",stade,"_",year,".RData"))

    n_tot <- ceiling(fit$median$n_tot) #annual total number of fish
    n_1SW <- ceiling(fit$median$n_1SW) # annual total number of fish 1SW
    n_MSW <- ceiling(fit$median$n_MSW) # annual total number of fish MSW
    
    table[,1] <- years
    if (site == "Scorff"){
      table[11:length(years),2] <- n_MSW
      table[11:length(years),3] <- n_1SW
      table[11:length(years),4] <- n_tot
    } else {
      table[,2] <- n_MSW
      table[,3] <- n_1SW
      table[,4] <- n_tot
    }   
    
    tmp <- table[,4]

   #res[[paste0(site)]] <- table
   total <- cbind(total, tmp)

write.csv(table, file=paste('report/Total_return_',site,'.csv',sep=""),row.names = FALSE)
}

#total <- data.frame(total)
colnames(total) <- c(sites)
rownames(total) <- years
write.csv(total, file=paste('report/Total_return_all.csv',sep=""),row.names = FALSE)


png("report/total_return.png",width = 780, height = 480)
mycol=c("#787878", "#1E90FF", "#FF6A6A", "#BCEE68")
plot(NULL,xlim=c(1,length(years)),ylim=range(total,na.rm = TRUE),bty="n",ylab="Annual total number of fish",xaxt="n",xlab="Year of return")
axis(side=1,line=1,labels = years,at=1:length(years))
for (site in 1:4) {
  lines(total[,site],lty=1,lwd=3,col=mycol[site])  
}
legend("topright", legend=sites, col=mycol,lty=1,lwd=2,bty="n")
dev.off()




