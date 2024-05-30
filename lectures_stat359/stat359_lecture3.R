
x<-seq(0.01,12,0.01)
y.df2<-dchisq(x,df=2)
y.df3<-dchisq(x,df=3)
y.df4<-dchisq(x,df=4)
plot(c(0,12),c(0,max(y.df2,y.df3,y.df4)),type='n', ylab='Density Function',xlab='x')
title('Density of Chi-Square(df)')
lines(x,y.df2,col='blue')
lines(x,y.df3,col='green')
lines(x,y.df4,col='red')
legend(x=c(6,9),y=c(0.4,.2),legend=c('df=2','df=3','df=4'),fill=c('blue','green','red'))

y.prob <- 1- pchisq(q = 4, df = 3)
y.prob

x.y.prob<- 1 - pchisq(q=4, df=7)
x.y.prob

q5<-qchisq(p=0.5,df=4)
q5

y.chisq2<-rchisq(n=1000,df=2)
y.chisq2
y.chisq3<-rchisq(n=1000,df=3)
y.chisq4<-rchisq(n=1000,df=4)
y.chisq7<-rchisq(n=1000,df=7)
par(mfrow=c(2,2))
hist(y.chisq2,main='Chi-Square(2) Draws')
hist(y.chisq3,main='Chi-Square(3) Draws')
hist(y.chisq4,main='Chi-Square(4) Draws')
hist(y.chisq7,main='Chi-Square(7) Draws')

