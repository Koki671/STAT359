
cars<-read.table(file ='~/Desktop/stat359/data/car_speeds.txt', sep="",header=TRUE)
cars<-read.table(file ='~/Desktop/stat359/data/car_speeds.txt', sep="",header=TRUE)
cars
names(cars)
summary(cars)
speed.up <- cars$speed[cars$direction == 'Up']
> speed.down<-cars$speed[cars$direction=='Down']
> boxplot(speed.up,speed.down, col='green',names=c('Speed - Up','Speed - Down'))
> 
  > summary(speed.up)
sqrt(var(speed.up))/sqrt(length(speed.up))
sqrt(var(speed.down))/sqrt(length(speed.down))
alpha<-0.05
> lower<-mean(speed.up) - mean(speed.down) - qnorm(1-(alpha/2))*sqrt( (var(speed.up)/length(speed.up)) +  (var(speed.down)/length(speed.down)))
> upper<-mean(speed.up) - mean(speed.down) + qnorm(1-(alpha/2))*sqrt( (var(speed.up)/length(speed.up)) +  (var(speed.down)/length(speed.down)))
> c(lower,upper)

T.obs<-(mean(speed.up) - mean(speed.down))/sqrt( (var(speed.up)/length(speed.up)) +  (var(speed.down)/length(speed.down)))
> T.obs
p.value<-2*(1-pnorm(T.obs))
> p.value

stereograms<-read.table(file ='~/Desktop/stat359/data/stereograms.txt', sep="",header=TRUE)

time.NV<-stereograms$fusion_time[stereograms$group=='NV']
time.VV<-stereograms$fusion_time[stereograms$group=='VV']
summary(time.NV)
summary(time.VV)
sqrt(var(time.NV))/sqrt(length(time.NV))
