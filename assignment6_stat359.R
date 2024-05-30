Stat 359 Assignment 6

#A geriatrics researcher studied the effects of two interventions on the frequency of falls. Subjects were at least 65 years of age and in reasonably good health. The variables found in the dataset geriatric.txt are: number of falls, intervention (0=education, 1=education and aerobics), gender (0=female), balance index, strength index.


#oktb
#(a) Fit a Poisson regression model based on u = exp(b0+ b1x1 + b2x2 + b3x3+ B4x4). Produce
#a table of estimated coefficients, their estimated standard errors, and the corresponding
#confidence intervals.
geriatric<-read.table(file='~/Desktop/stat359/data/geriatric.txt', header = T)
head(geriatric)


table(geriatric$Fall)
hist(geriatric$Fall)

fit1<-glm(Fall~ Int + Sex + BI + SI,data = geriatric)
summary(fit1)
df<-data.frame(estimate = c(1.614138,-2.971077,-0.157435,0.029104,0.023976),
               est_sderror = c(
  0.999148,0.375428,0.380966,0.009635,0.013237),ci = confint(fit1) 
)
df
par(mfrow = c(3,1))
plot(fit1,which = c(1,2,4))






#(b) Obtain the model deviance and perform a goodness-of-fit test. State your conclusion.

deviance_fit1<-deviance(fit1)
deviance_fit1
res_df_fit1<-(fit1$df.residual)
res_df_fit1
1-pchisq(deviance_fit1,res_df_fit1)
#Ho:Model is adequate, Ha: Model not adequate.
#Dispersion parameter for gaussian family taken to be 3.398396.
#But,model deviance is 322.83 and degreees of freedom is 95.
#THese numbers are not close. so My conclusion is the model is not
#good fit.
#Also, according to the chi-square test, the p-value < 0.05, this means that 
#we reject Ho and there is a significant evidence that the model is not adequate.

dev_res <- residuals.glm(fit1,"deviance")


#菊
#(c) Plot the deviance residuals (versus their index). Do there appear to be any outlying cases?
plot(seq_along(dev_res),dev_res,
     main="Deviance Residuals vs. Index", 
     xlab="Index", 
     ylab="Deviance Residuals")
#From the graph, almost all of the data are gathered around 0 at deviance residuals.
#However there are some outliers whose deviance residuals are approximately 6 and -4.

#(d) Use a deviance test to test the hypothesis that gender can be dropped from the model. What is your conclusion?
#I compute the differrence in deviance as a test statistic and compute the p-value:
#Ho: b2 = 0 Ha: b2 != 0
#remove Sex and the fit the smaller model
fit2<- glm(Fall~ Int + BI + SI,data = geriatric)
#assess the significance of Sex using a deviance test
anova(fit2,fit1,test = "Chi")

1-pchisq(deviance(fit2)-deviance(fit1), df = 1)

#Since p-value > a = 0.05, Based on the likelihood ratio we would not reject Ho.
#Therefore there is a insignificant evidence that the gender is related to the number 
#of Fall.
par(mfrow = c(3,1))
plot(fit2,which = c(1,2,4))
#ok
#(e) Fit a model without ‘gender’. Obtain an approximate 95% confidence interval for §; (thecoefficient for intervention’) and interpret the confidence interval.
fit3<-update(fit1,.~. -Sex)
summary(fit3)
confint(fit3, "Int")
par(mfrow = c(3,1))
plot(fit2,which = c(1,2,4))
# The 95% confidence interval for the intervention is between -3.7198 and -2.2801.
#the range of the interval is lower than 0. This means that there is a negative relationships between Fall and intervention. So, If the more the intervention is,
#the more the number of falls decreases.

#(f) Is aerobic exercise associated with a reduction in the frequency of falls
#when controlling for balance and strength?

#intが1の時

boxplot(geriatric$Fall[geriatric$Int == 1], geriatric$Fall[geriatric$Int == 0],
        names = c("aerobic exercise", "No aerobic excersise"), main = "
        Falls by aerobic excersise")


fit4<-glm(Fall~ Int + BI + SI,data = geriatric)
summary(fit4)

confint(fit4)

#According to the summary of the fit4 poisson regression,the p-value is 
#1.25e-12 <<0.05, so It is significant that aerobic exercise is assorciated 
#with the frequency of falls. Since the estimate of the Int is -2.999965, also
#the 95% of the confidence interval of Int is between -3.7197929927 -2.28013697.
#Therefore, aerobic exercise is associated with a reduction in the frequency of falls
#when controlling for balance and strength.




