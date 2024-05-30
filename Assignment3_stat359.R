#1. The following data represent the running times of films produced by 
#two motion-picture companies:

#Since both of the sample sizes are pretty small, and we do not know sigmas,
#I use t distribution.
#Test the hypothesis that the average running time of films produced by company
#2 exceeds the average running time of films produced by company 1 by 10
#minutes against the one-sided alternative that the difference is less than 10
#minutes. Use a 0.1 level of significance. Please consider carefully assumptions
#made on the data.
par(mar=c(1,1,1,1))
#companydataframe<- data.frame(Company <- c(1,2), Time(minutes) <- c(c(102, 86, 98, 109, 92),c(81, 165, 97, 134, 92, 87, 114)))
c1<-c(102, 86, 98, 109, 92)
c2<-c(81, 165, 97, 134, 92, 87, 114)
summary(c1)
summary(c2)
boxplot(c1,c2, names = c("Company 1", "Company 2"),
         main = "Running time of films",sub = "Written by Koki")
qqnorm(c1)
qqnorm(c2)
#To decide which t-test I will use, I need to know if the sample 
#variance is the same or not 
var.test(c1,c2,alternative = "two.sided",conf.level	= 0.90)

#Since P value = 0,003298 <= a = 0.1, this is a significant evidence against Ho.
#Therefore, the variance is different.

t.test(c1,c2,alternative = "less",mu = 10, var.equal = FALSE,conf.level	= 0.90)

#Since the p-value = 0.05085 <= a = 0.1, we reject Ho.
#There is a significant evidence that the average running time of films produced
#by company 2 exceeds the average running time of films produced by company 1
#by 10 minutes against the one-sided alternative that the difference is less
#than 10 minutes


#Question 2
#Six different machines are being considered for use in manufacturing rubber
#seals. The machines are being compared with respect to tensile strength of the
#product. A random sample of four seals from each machine is used to
#determine whether the mean tensile strength varies from machine to
#machine. The following are the tensile-strength measurements in kilograms
#per square centimeter 10E-01.

#k = 6, n = 4, N =24, a = 0.05

#an analysis of variance at the 0.05 significance level.
#The hypothesis is Ho: u1 = u2 = ...= u6 Ha: at least one u is different
dataframe<-data.frame(strength = c(17.5, 16.9,15.8,18.6,16.4,19.2,17.7,15.4,
20.3,15.7,17.8,18.9,14.6,16.7,20.8,18.9,17.5,19.2,
16.5,20.5,18.3,16.2,17.5,20.1),
Machines= c("M1","M1","M1","M1","M2","M2","M2","M2","M3","M3","M3","M3","M4",
            "M4","M4","M4","M5","M5","M5","M5","M6","M6","M6","M6"))
dataframe
attach(dataframe)
Strength<-tapply(strength,Machines,mean)
Strength
par(mfrow=c(2,3))
barplot(Strength,col = "Green", ylim = c(0,30),ylab = "Machines",
        xlab = "Tensile strength of products", sub = "Writen by Koki")
                      
                      
error.bars<-function(y,z){
  x<-barplot(y, plot=F)
  n<-length(y)
  for (i in 1:n)
  {
    arrows(x[i],y[i]-z[i],x[i],y[i]+z[i],code=3,angle=90,length=0.15)
  }
}
sigma.hat<-summary.lm(aov(strength~Machines))$sigma
sigma.hat
table(Machines)
se.mean<-sigma.hat/sqrt(4)
se.mean


barplot(Strength, col="green", ylim=c(0,20),ylab="mean strength", xlab = "Machines")
bar.half.width<-rep(se.mean,6)
error.bars(Strength,bar.half.width)

#According to the barplot with error bars, it is clear that the error bars are
#overlaped. This means all means seem to be the same.

#For certainly, I also use the least significant difference method.

LSD<-2*sqrt(2)*se.mean
LSD.bars<-rep(LSD,6)/2
barplot(Strength, col="green", ylim=c(0,20),ylab="mean Strength", xlab =
          "Machines")
error.bars(Strength,LSD.bars)
#From this graph, I can see that the error bars are overlaped as well.


summary(aov(strength~Machines))

#According to the ANOVA table, the p-value is 0.902.
#Since the p-value >= 0.05 = a, we fail to reject Ho.
#There is a insignificant evidence that at least one mean of 6 machines are
#different.


resid.plant<-resid(aov(strength~Machines))
boxplot(resid.plant[Machines=="M1"],resid.plant[Machines=="M2"],
        resid.plant[Machines=="M3"],resid.plant[Machines=="M4"],
        resid.plant[Machines=="M5"],resid.plant[Machines=="M6"],
        names=c('M1','M2','M3','M4','M5', 'M6'),
        col="green")


qqnorm(resid.plant)
qqline(resid.plant)

#Q3
#The data in the following table represent the number of hours of relief
#provided by five different brands of headache tablets administered to 25
#ubjects experiencing fevers of 38 degrees Celsius or more. Perform the
#analysis of variance and test the hypothesis at the 0.05 level of significance
#that the mean number of hours of relief provided by the tablets is the same for
#all five brands. Discuss the results.


data_hours<-data.frame(Hours = c(5.2,4.7,8.1,6.2,3.0,9.1,7.1,8.2,6.0,9.1,
                                    3.2,5.8,2.2,3.1,7.2,2.4,3.4,4.1,1.0,4.0,
                                    7.1,6.6,9.3,4.2,7.6),
                      Brands= c("A","A","A","A","A","B","B","B","B","B","C","C","C",
                                  "C","C","D","D","D","D","D","E","E","E","E","E"))



attach(data_hours)
mhours<-tapply(Hours,Brands,mean)
par(mfrow=c(2,3))
barplot(mhours,col = "Green", ylim = c(0,10),xlab = "Brands",
        ylab = "hours of relief ", sub = "Writen by Koki")


error.bars<-function(y,z){
  x<-barplot(y, plot=F)
  n<-length(y)
  for (i in 1:n)
  {
    arrows(x[i],y[i]-z[i],x[i],y[i]+z[i],code=3,angle=90,length=0.15)
  }
}
sigma.hat<-summary.lm(aov(Hours~Brands))$sigma
sigma.hat
table(Brands)
se.mean<-sigma.hat/sqrt(5)
se.mean


barplot(mhours, col="green", ylim=c(0,10),ylab="hours of relief", xlab = "Brands")
bar.half.width<-rep(se.mean,6)
error.bars(mhours,bar.half.width)

#According to the barplot with error bars, it is clear that the error bars of C
#and D are quite small and do not overlap with the other error bars.
#overlaped. This means all means seem to be the same.

#For certainly, I also use the least significant difference method.

LSD<-2*sqrt(2)*se.mean
LSD.bars<-rep(LSD,6)/2
barplot(mhours, col="green", ylim=c(0,10),ylab="hours of relief ", xlab =
          "Brands", sub = "Written by Koki")
error.bars(mhours,LSD.bars)
#From this graph, I can see that the all error bars are not overlaped as well.


summary(aov(Hours~Brands))

#According to the ANOVA table, the p-value is 0.0015.
#Since the p-value >= 0.05 = a, we reject Ho.
#There is a significant evidence that at least one mean that the number of hours
#of relief provided by five different brands of headache tablets is different


resid.plant<-resid(aov(Hours~Brands))
boxplot(resid.plant[Brands=="A"],resid.plant[Brands=="B"],
        resid.plant[Brands=="C"],resid.plant[Brands=="D"],
        resid.plant[Brands=="E"],resid.plant[Brands=="F"],
        names=c('A','B','C','D','E','F'),
        col="green")
#From the residual plots, we can see that mean od c is pretty low.


qqnorm(resid.plant)
qqline(resid.plant)

#From the qq plot, the data make the stright line.
#This means the data is normally distributed.












#1 やり方あってる
#zenbuattetu
