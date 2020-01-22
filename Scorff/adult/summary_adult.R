fit.mcmc <- as.mcmc(fit$sims.matrix) # using bugs
e_1SW <- as.matrix(fit.mcmc[,paste0("e_1SW[",1:data$Y,"]")])
e_MSW <- as.matrix(fit.mcmc[,paste0("e_MSW[",1:data$Y,"]")])

write.csv(e_1SW,file="results/e_1SW.csv")#,sep=";")
write.csv(e_MSW,file="results/e_MSW.csv")#,sep=";")
