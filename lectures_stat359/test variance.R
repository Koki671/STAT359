ice<- read.table(file ='~/Desktop/stat359/data/latent_heat.txt', sep="",header=TRUE)
a<-ice$Method_A[!is.na(ice$Method_A)]
b<-ice$Method_B[!is.na(ice$Method_B)]
var.test(a,b)
iron<-read.table(file ='~/Desktop/stat359/data/iron.txt', sep="",header=TRUE)
iron
a<-iron$Fe3
b<-iron$Fe2