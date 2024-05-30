weightss<-read.table(file ='~/Desktop/stat359/data/growth.txt',header=TRUE,sep="")
attach(weightss)
names(weightss)

weights<-read.table(file ='~/Desktop/stat359/data/growth.csv',header=TRUE,sep="")
library(knitr)
kable(weights, caption = 'Animal Weight Gain',align='l')
