invlogit<-function(x) {1/(1+exp(-(x)))} # inverse logit
logit<-function(x) {log(x/(1-x))}