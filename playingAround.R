csv = read.csv(file="thresholds.csv",head=TRUE,sep=",")
head(csv)

library(mgcv)

v = csv[,1]
d = csv[,12]
plot(d,v)