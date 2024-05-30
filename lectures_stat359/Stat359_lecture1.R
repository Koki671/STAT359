worms<-read.table(file ='~/Desktop/stat359/data/worms.txt', sep="",header=TRUE, na.strings="NA")
names(worms)
library(knitr)
kable(worms, caption = 'Data table')
#attachによって＄サインいらなくなる
attach(worms)
summary(worms)
mean(Aera)
Area[3]
Area[Area<3]
Area[Area>mean(Area)]
length(Area[Area>mean(Area)])
worms[2,2]
worms[,3]
worms[1:3,3]
worms[Area > 3 & Slope < 3,]
order(Area)
worms[order(area)]
detach(worms)

memory<-read.table(file = '~/Desktop/stat359/data/memory.txt',sep="", header=TRUE,na.strings="NA")

kable(memory, caption = 'Data Table')
boxplot(memory$ginkgo,memory$Placebo, col='green',names=c('ginkgo','Placebo'))
title('Memory Scores')

summary(memory)
attach(memory)
length(ginkgo)
length(Placebo)
ginkgo<-ginkgo[!is.na(ginkgo)]
Placebo<-Placebo[!is.na(Placebo)]


mean(ginkgo)
median(ginkgo)

sum(ginkgo)/length(ginkgo)

mean(Placebo)


sample.median<-function(y)
{
  # sample size
  n<-length(y)
  # order the data values
  y.ordered<-sort(y)
  # determine if n is odd
  odd<-n%%2 ## is one if n is odd
  if (odd == 1)
  {
    index<-ceiling(n/2)
    return.value<-y.ordered[index]
  }
  else
  {
    index1<-n/2
    index2<-(n/2)+1
    return.value<-(y.ordered[index1]+y.ordered[index2])/2
  }
  return.value # return the value contained in this variable
}

sample.median(Placebo)



y<-rnorm(n = 20,mean = 2, sd = 1)
hist(y)
boxplot(y)
mean(y)
# create a new sample by changing only one value of y
y2<-y
# find the index of the maximum value
index.of.max<-which.max(y2)
y2[index.of.max]<-10000 # take the maximum value and replace it with 10000

# compare the two samples using side by side boxplots
boxplot(y,y2)

boxplot(y,y2,outline=F)

c(mean(y),mean(y2))

c(median(y),median(y2))



