
e_1SW <- as.matrix(fit.mcmc[,paste("e_1SW[",1:22,"]",sep="")])
e_MSW <- as.matrix(fit.mcmc[,paste("e_MSW[",1:22,"]",sep="")])

write.csv(e_1SW,file="results/e_1SW.csv",sep=";")
write.csv(e_MSW,file="results/e_MSW.csv",sep=";")