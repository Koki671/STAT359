fish<-read.table(file = '~/Desktop/stat359/data/fish.txt', sep="",header=TRUE)

library(knitr)
kable(fish, caption = 'Mercury study of fish')
summary(fish)
plot(fish$Selective_Reduction,fish$Permanganate,xlab='Selective Reduction', ylab = 'Permanganate')

cor(fish$Selective_Reduction,fish$Permanganate)
wilcox.test(fish$Selective_Reduction,fish$Permanganate,paired=TRUE)
t.test(fish$Selective_Reduction,fish$Permanganate,paired=TRUE)

D<-fish$Selective_Reduction - fish$Permanganate
qqnorm(D)