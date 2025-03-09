p_male <- fit$sims.list$p_male
# Quantiles for each column across the samples
p_male_quantiles <- apply(p_male, c(2, 3), quantile, probs = c(0.025,0.25, 0.5, 0.75,0.975))
p_male_smolt1<-p_male_quantiles[,,1]
p_male_smolt2<-p_male_quantiles[,,2]
colnames(p_male_smolt1)<-colnames(p_male_smolt2)<- c(1995:2023)
save(p_male_smolt1, p_male_smolt2, file="p_male_smolt_scorff.Rdata")
print(p_male_quantiles)
