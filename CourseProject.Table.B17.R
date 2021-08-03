#Course Project
#Data from Table B.17 page 570 of Textbook - Intro to Linear Regression Analysis (Montogomery, et al 2012)
#Standard libraries
library("ggplot2")
library(readxl)
library (MPV)
library (olsrr)
library("ggpubr")
library("Hmisc")
library(caret)
library (lattice)
library(leaps)
library(MASS)
library(alr3)
library(rms)
library(ppcor)
library(ggthemes)
library(data.table)
library(ISLR)
library(tibble)
library(aod)
library(tidyverse)
library(modelr)
library(broom)
library(WVPlots)
library(lmtest)
library(drc)
library(nlme)
library(aomisc)
library(scatterplot3d)
library(dplyr)
#install.packages("pastecs")
library(pastecs)
#install.packages("psych")
library(psych)
#install.packages("moments")
library(moments)

#Import Data
data_table_B17 <- read_excel("Desktop/JHU/Data Science/Statistical Models and Regression/Course Project/data-table-B17.xls")
View(data_table_B17)
attach(data_table_B17)

#Create data frame
#Target/Response Variable = Satisfaction
#Predictor Variables = Age, Severity, Surgical-Medical, Anxiety
data <- data.frame(Satisfaction, Age, Severity, `Surgical-Medical`, Anxiety)

#Run EDA
summary(data)
#Get standard deviations
sapply(data, sd)

#Get n, nmiss, unique, mean, 5, 10, 25, 50, 75, 90, 95th percentiles
#5 lowest & 5 highest scores
describe(data)

#nbr.val, nbr.null, nbr.na, min max, range, sum
#median, mean, SE.mean, CI.mean, var, st.dev, coef.var
stat.desc(data)

#n, mean, sd, median, trimmed, mad, min, max, range, skew, kurtosis, se
describeBy(data, group=NULL)

#Get Tukey's 5 (minimum, lower-hinge, median, upper-hinge, maximum)

#Satisfaction
fivenum(data$Satisfaction, na.rm = TRUE)
#[1]  26  52  70  83 102
#Get interquartile range
IQR(data$Satisfaction, na.rm = TRUE)
#IQR = 31
#Skewness
skewness(data$Satisfaction)
#-0.307575
#very slight or moderate left/negative skew
#Kurtosis
kurtosis(data$Satisfaction)
#2.120311
#value is less than 3 = low kurtosis


#Age
#Get Tukey's 5 (minimum, lower-hinge, median, upper-hinge, maximum)
fivenum(data$Age, na.rm = TRUE)
#[1]24 39 51 61 79
#Get interquartile range
IQR(data$Age, na.rm = TRUE)
#IQR = 22
#Skewness
skewness(data$Age)
#-0.01654419
#Almost no skew
#Kurtosis
kurtosis(data$Age)
#2.160039
#value is less than 3 = low kurtosis

#Severity
fivenum(data$Severity, na.rm = TRUE)
#[1] 24 38 42 58 71
#Get interquartile range
IQR(data$Severity, na.rm = TRUE)
#IQR = 20
#Skewness
skewness(data$Severity)
#[1] 0.282166
#Almost no skew
#Kurtosis
kurtosis(data$Severity)
#2.047968
#value is less than 3 = low kurtosis

#Anxiety
fivenum(data$Anxiety, na.rm = TRUE)
#[1] 1.9 2.4 3.3 5.1 7.8
#Get interquartile range
IQR(data$Anxiety, na.rm = TRUE)
#IQR = 2.7
#Skewness
skewness(data$Anxiety)
#[1] 0.7346762
#Value is close to positive 1, indicating some skew
#Kurtosis
kurtosis(data$Anxiety)
#2.454605
#value is close to 3, indicating nearly normal shape

#Get Surgical-Medical Counts
table(data$Surgical.Medical)
#Number of Observations for 0=Surgical: 11
#Number of Observations for 1=Medical: 14
#Examine distribution of Surgical-Medical
ggplot(data = data) + geom_bar(mapping=aes(x=data$Surgical.Medical))
histogram(`Surgical-Medical`, data=data)

#Histogram for each variable
#Satisfaction
ggplot(data=data)+geom_histogram(mapping=aes(x=data$Satisfaction))
ggplot(data=data)+geom_histogram(mapping=aes(x=data$Satisfaction), binwidth = 10)
SatisfactionPlot <-ggplot(data, aes(x=Satisfaction))
#Density Plot
#y axis scale = density
SatisfactionPlot +geom_density()+geom_vline(aes(xintercept=mean(Satisfaction)), linetype = "dashed", size = 0.6)
#Change y axis to count instead of density
SatisfactionPlot +geom_density(aes(y=..count..), fill="lightgray")+geom_vline(aes(xintercept=mean(Satisfaction)), linetype="dashed", size=0.6, color="#FC4E07")
#Histogram plot (counts)
SatisfactionPlot+geom_histogram(bins=10, color="black", fill="gray")+geom_vline(aes(xintercept=mean(Satisfaction)),linetype="dashed",size=0.6)
#Histogram plot (density)
SatisfactionPlot+geom_histogram(aes(y=..density..), color="black", fill="white")+geom_density(alpha=0.2,fill="#FF6666")
SatisfactionPlot+geom_histogram(bins=10, aes(y=..density..), color="black", fill="gray")
#Basic frequency polygon
SatisfactionPlot+geom_freqpoly(bins=10)

#Age
ggplot(data=data)+geom_histogram(mapping=aes(x=data$Age))
ggplot(data=data)+geom_histogram(mapping=aes(x=data$Age), binwidth = 10)
AgePlot <-ggplot(data, aes(x=Age))
#Density Plot
#y axis scale = density
AgePlot +geom_density()+geom_vline(aes(xintercept=mean(Age)), linetype = "dashed", size = 0.6)
#Change y axis to count instead of density
AgePlot +geom_density(aes(y=..count..), fill="lightgray")+geom_vline(aes(xintercept=mean(Age)), linetype="dashed", size=0.6, color="#FC4E07")
#Histogram plot (counts)
AgePlot+geom_histogram(bins=10, color="black", fill="gray")+geom_vline(aes(xintercept=mean(Age)),linetype="dashed",size=0.6)
#Histogram plot (density)
AgePlot+geom_histogram(bins=10, aes(y=..density..), color="black", fill="white")+geom_density(alpha=0.2,fill="#FF6666")
AgePlot+geom_histogram(bins=10, aes(y=..density..), color="black", fill="white")
#Basic frequency polygon
AgePlot+geom_freqpoly(bins=10)

#Severity
ggplot(data=data)+geom_histogram(mapping=aes(x=data$Severity))
ggplot(data=data)+geom_histogram(mapping=aes(x=data$Severity), binwidth = 5)
SeverityPlot <-ggplot(data, aes(x=Severity))
#Density Plot
#y axis scale = density
SeverityPlot +geom_density()+geom_vline(aes(xintercept=mean(Severity)), linetype = "dashed", size = 0.6)
#Change y axis to count instead of density
SeverityPlot +geom_density(aes(y=..count..), fill="lightgray")+geom_vline(aes(xintercept=mean(Severity)), linetype="dashed", size=0.6, color="#FC4E07")
#Histogram plot (counts)
SeverityPlot+geom_histogram(bins=10, color="black", fill="gray")+geom_vline(aes(xintercept=mean(Severity)),linetype="dashed",size=0.6)
#Histogram plot (density)
SeverityPlot+geom_histogram(bins=10, aes(y=..density..), color="black", fill="white")+geom_density(alpha=0.2,fill="#FF6666")
SeverityPlot+geom_histogram(bins=10, aes(y=..density..), color="black", fill="white")
#Basic frequency polygon
SeverityPlot+geom_freqpoly(bins=10)

#Anxiety
ggplot(data=data)+geom_histogram(mapping=aes(x=data$Anxiety))
ggplot(data=data)+geom_histogram(mapping=aes(x=data$Anxiety), binwidth = 1)
AnxietyPlot <-ggplot(data, aes(x=Anxiety))
#Density Plot
#y axis scale = density
AnxietyPlot +geom_density()+geom_vline(aes(xintercept=mean(Anxiety)), linetype = "dashed", size = 0.6)
#Change y axis to count instead of density
AnxietyPlot +geom_density(aes(y=..count..), fill="lightgray")+geom_vline(aes(xintercept=mean(Anxiety)), linetype="dashed", size=0.6, color="#FC4E07")
#Histogram plot (counts)
AnxietyPlot+geom_histogram(bins=10, color="black", fill="gray")+geom_vline(aes(xintercept=mean(Anxiety)),linetype="dashed",size=0.6)
#Histogram plot (density)
AnxietyPlot+geom_histogram(bins=10, aes(y=..density..), color="black", fill="white")+geom_density(alpha=0.2,fill="#FF6666")
AnxietyPlot+geom_histogram(bins=10, aes(y=..density..), color="black", fill="white")
#Basic frequency polygon
AnxietyPlot+geom_freqpoly(bins=10)


#Plot each IV against the DV
ggplot(data=data,aes(x=Age,y=Satisfaction))+geom_point()
ggplot(data=data,aes(x=Severity,y=Satisfaction))+geom_point()
plot(Satisfaction[][`Surgical-Medical`==0]~`Surgical-Medical`[][`Surgical-Medical`==0])
plot(Satisfaction[][`Surgical-Medical`==1]~`Surgical-Medical`[][`Surgical-Medical`==1])
ggplot(data=data,aes(x=Anxiety,y=Satisfaction))+geom_point()

#Correlations
pairs(data)
cor(data)
#Age has a strong (negative) correlation with Satisfaction (r = -0.871)
#Severity has a strong (negative) correlation with Satisfaction (r = -653)
#Surgical-Medical has a weak correlation with Satisfaction (r = -0.182)
#Anxiety has strong correlation with Satisfaction (r = -0.513)
#Where, strong correlation is r >= |0.5|
#May have a multicollinearity problem: 
#Severity & Age r = 0.529
#Anxiety & Age r = 0.621
corr.test(Satisfaction, Age)
#r=-0.87 (strong negative)
corr.test(Satisfaction, Age, method = "spearman")
#rho = -0.85 (strong negative)
corr.test(Satisfaction, Severity)
#r=-0.65 (strong negative)
corr.test(Satisfaction, Severity, method = "spearman")
#rho = -0.6 (strong negative)
corr.test(Satisfaction, `Surgical-Medical`)
#r=-0.18 (weak negative)
corr.test(Satisfaction, `Surgical-Medical`, method = "spearman")
#rho = -0.17 (weak negative)
corr.test(Satisfaction, Anxiety)
#r=-0.51 (strong negative)
corr.test(Satisfaction, Anxiety, method = "spearman")
#rho = -0.42 (medium negative)

#Start with All-In Model (First Order Linear Multiple Regression, Main Effects)
AllInModel <-lm(Satisfaction~Age+Severity+`Surgical-Medical`+Anxiety, data = data)

#Obtain results
summary(AllInModel)
#Satisfaction = 140.1689 - (1.1428*Age) - (0.4699*Severity) + (2.2259*Surgical-Medical) + (1.2673*Anxiety)
#R^2 = 0.8183, Ra^2 = 0.7819
#Beta Coefficients for Age & Severity have p<0.05
#Confirm coefficients
coefficients(AllInModel) #model coefficients
#Get standard deviations
sapply(data, sd)
#Confirm with ols package
ols_regress(AllInModel)
#Remember, Fc = (SSR/k) / (SSE/n-k+1)
#Alpha = .05
#Fo = F(1-alpha, df, df)
#Fo = F(1-.05,4,20)
qf(0.95,4,20)
#Fo = 2.866081

#Confidence Intervals (95%)
confint(AllInModel)
#Age: Small Range, does not include 0
#Severity: Small range, does not include 0
#Surgical-Medical: Large range, includes 0
#Anxiety: Large range, includes 0
#Another way to calculate confidence intervals:
b <- summary(AllInModel)$coef[,1]
b
seb <-summary(AllInModel)$coef[,2]
seb
alpha <- 0.05
tval <- qt(1-alpha/8,20)
lower.lim <-b-tval*seb
lower.lim
upper.lim<-b+tval*seb
upper.lim

#R^2
R2 <-summary(AllInModel)$r.squared
R2
#R^2 = 0.8182508

#Plot All In Model
plot(Age+Severity+`Surgical-Medical`+Anxiety, Satisfaction, data=data)
lines(lowess(Age+Severity+`Surgical-Medical`+Anxiety, Satisfaction))
#Appears linear
#Can also plot with ols library
ols_plot_reg_line(Satisfaction,Age+Severity+`Surgical-Medical`+Anxiety)
ols_plot_response(AllInModel)

#Obtain Fitted Values
fitted(AllInModel) #predicted values

#Obtain regression diagnostics for each observation: y_hat, coefficients, sigma, weighted residual
influence(AllInModel) #regression diagnostics
#Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) #optional 4 graphs/page 
plot(AllInModel)
#Regression Diagnostics Battery
ols_plot_diagnostics(AllInModel)

#Obtain residuals
residuals(AllInModel) #residuals
e <-residuals(AllInModel)
e
boxplot(e, ylab = "Residuals")

#Plot residuals against Y_hat
yhat <- fitted(AllInModel)
plot(yhat, e, xlab = "Fitted Values", ylab = "Residuals", ylim = c(-20,20))
abline(h=0, lty = 2)

#Plot residuals against each of the predictor variables
#Age Residuals
plot(Age,e, xlab="Age", ylab = "Residuals", ylim = c(-20,20))
abline(h=0, lty=2)
#Severity Residuals
plot(Severity, e, xlab = "Severity", ylab = "Residuals", ylim = c(-20,20))
abline(h=0, lty=2)
#Surgical-Medical Residuals
plot(`Surgical-Medical`, e, xlab = "Surgical-Medical", ylab = "Residuals", ylim = c(-20,20))
abline(h=0,lty=2)
#Anxiety
plot(Anxiety,e, xlab = "Anxiety", ylab = "Residuals", ylim = c(-20,20))
abline(h=0, lty=2)

#3d Scatterplot
scatterplot3d(Age, Severity, e, xlab = "Age", ylab = "Severity", zlab = "Residuals")
scatterplot3d(Age,`Surgical-Medical`, e, xlab = "Age", ylab = "Surgical-Medical", zlab = "Residuals")
scatterplot3d(Age,Anxiety, e, xlab = "Age", ylab = "Anxiety", zlab = "Residuals")
scatterplot3d(Severity, `Surgical-Medical`, e, xlab = "Severity", ylab = "Surgical-Medical", zlab = "Residuals")
scatterplot3d(Severity, Anxiety, e, xlab = "Severity", ylab = "Anxiety", zlab = "Residuals")
scatterplot3d(`Surgical-Medical`, Anxiety, e, xlab = "Surgical-Medical", ylab = "Anxiety", zlab = "Residuals")

#More Residuals
summary(AllInModel)$residuals
ols_plot_resid_stud(AllInModel)
#Residual Normality Test
ols_test_normality(AllInModel)
#Correlation Between Observed Residuals and Expected Residuals Under Normality
ols_test_correlation(AllInModel)
#Correlation = 0.9483583
#Residual vs Fitted Values Plot
ols_plot_resid_fit(AllInModel)
#Residual Histogram
ols_plot_resid_hist(AllInModel)
#Studentized Residuals vs Leverage Plot
ols_plot_resid_lev(AllInModel)
#Deleted Studentized residual vs predicted values
ols_plot_resid_stud_fit(AllInModel)
#Residuals
r<-resid(AllInModel)
r
#QQ Plot
qqnorm(resid(AllInModel))
ols_plot_resid_qq(AllInModel)
#Predictively adjusted residuals
(pr<-resid(AllInModel)/(1-lm.influence(AllInModel)$hat))
#Construct residual vs predicted response
plot(predict(AllInModel), resid(AllInModel))
#Cross-validated residuals
#Regular RSS is
sum(r^2)
#PRESS is
sum(pr^2)
#2869.485
#Note PRESS is bigger because predicting is harder than fitting
#Another way to calculate the PRESS statistic
PRESS<-function(AllInModel){
  pr<-residuals(AllInModel)/(1-lm.influence(AllInModel$hat)
                             sum(pr^2)
                             }
PRESS(AllInModel)
#Same result: 2869.485

#standardized residuals
ols_plot_resid_stand(AllInModel)

#Normal Probability Plot of Residuals
n <-length(e)
MSE <-sum(e^2)/(n-4)
RankofRes <-rank(e)
Zscore <- qnorm((RankofRes-0.375)/(n+0.25))
ExpRes <- Zscore * sqrt(MSE)
plot(ExpRes, e, xlab = "Expected Score", ylab = "Residuals")
abline(a = 0, b = 1)

#Detect Influence with Leverage
#The observed value of y_i is influential if h_i > [2(k+1)]/n
#Where h_i = leverage for the ith obervation
#k = # of betas in the model (excluding b_0)
# [2(k+1)]/n = [2(5+1)/25] = 12/25 = 0.48
ols_leverage(AllInModel)

# VIF = (1/1-R^2). VIF > 5 indicates associated regression coefficients are poorly estimated b/c multicollinearity
ols_vif_tol(AllInModel)
#Result: All VIFs < 5 ?
vif(AllInModel)

#Check for Collinearity
ols_coll_diag(AllInModel)
rmatrix <- rcorr(as.matrix(data)) #can be pearson or spearman
rmatrix
vcov(AllInModel) # covariance matrix for model parameters 

#Cook’s Distance: Combines leverage and residuals
#Higher value, the better
#Lowest Value = 0
#Conventional Cut off is 4/n
ols_plot_cooksd_bar(AllInModel)
ols_plot_cooksd_chart(AllInModel)
cooks.distance(AllInModel)

#dfbetas:measures the difference in each parameter estimate with and without the influential point
ols_plot_dfbetas(AllInModel)
dfbeta(AllInModel)

#lack of fit
ols_test_f(AllInModel)
#Fail to reject H0; p = 0.7596821


#Another tyoe of Lack of Fit Testing
#Pure Error Analysis of Variance: For a linear model object, finds the sum of squares for lack of fit and the sum of 
#squares for pure error. These are added to the standard anova table to give a test for lack of fit. If there is no 
#pure error, then the regular anova table is returned.
#For regression models with one predictor, say y ~ x, this method fits y ~ x + factor(x) and prints the anova table. 
#With more than one predictor, it computes a random linear combination L of the terms in the mean function and then 
#gives the anova table for update(mod, ~.+factor(L)).
pureErrorAnova(AllInModel) #include pure error
anova(AllInModel)

#Verify Assumption of Constant Variance (Breusch-Pagan test)
SSE <- sum(e^2)
SSE
#1968.533
n <-length(e)
reg2 <-lm (e^2~Age+Severity+`Surgical-Medical`+Anxiety, data = data)
y2hat<-fitted(reg2)
SSR2 <-sum((y2hat-mean(y2hat))^2)
SSR2
#43186.72
chiBP <-(SSR2/2)/(SSE/n)^2
chiBP
#3.482692
chiTAB<-qchisq(0.99,4)
chiTAB
#13.2767
chiTab <-qchisq(0.95,4)
chiTab
#9.4877729

#Predictions
#Interval Estimate of the mean satisfaction when X_h1 = #, X_h2 = #, X_h3 = #, X_h4 = #
#95% Confidence Coefficient
X <- cbind(1, Age, Severity, `Surgical-Medical`, Anxiety)
Y <-matrix(Satisfaction, ncol=1)
XX <-t(X) %*% X
XY <-t(X) %*% Y
b <-solve(XX) %*% XY
Xh <-matrix(c(1,35,45,1, 2.2), nrow=1) #given values of x
EYh <- Xh %*%b
EYh
#84.03949
VarEYh <- (Xh %*% solve(XX) %*% t(Xh)) * MSE
VarEYh
#15.34136
alpha <- 0.05
tval <- qt(1-alpha/3,20)
tval
#2.285497
c(EYh - tval *sqrt(VarEYh), EYh+tval*sqrt(VarEYh))
#75.08764 92.99133

#Obtain a prediction interval for a new patient's satisfaction when X_h1 = #, X_h2 = #, X_h3 = #, X_h4=#
pred <-Xh %*% b
pred
#84.03949
VarPred <- (1+(Xh %*% solve(XX) %*% t(Xh))) *MSE
VarPred
#109.081
c(pred-tval*sqrt(VarPred), pred+tval*sqrt(VarPred))
#60.16933 107.90964

#TRY ELIMINATING VARIABLE SURGICAL MEDICAL
#Remember, for AllInModel:
#R^2 = 0.818
#Ra^2 = 0.782
#MSE = 98.427 

#No Surgical-Medical Variable (Model is First Order Linear Multiple Regression, MEs only)
modelNoSurgMed <-lm(Satisfaction~Age+Severity+Anxiety, data = data)

#Obtain Results
summary(modelNoSurgMed)
#Satisfaction = 140.3193 - (1.1233*Age) - (0.4629*Severity) + (1.2126*Anxiety)
#Fo < .05 (significant model)
#Anxiety coefficient nonsignificant
#R^2 = 0.816 (Decreased)
#Ra^2 = 0.789 (Increased)
#MSE = 95.094 (Decreased)
#Confirm with ols
ols_regress(modelNoSurgMed)

#TRY ELIMINATING ANXIETY (RE-INSERT SURGICAL MEDICAL)
#No Anxiety Variable (Model is First Order Linear Multiple Regression, MEs only)
modelNoAnxiety <-lm(Satisfaction~Age+Severity+`Surgical-Medical`, data = data)

#Obtain Results
summary(modelNoAnxiety)
#Satisfaction = 139.7722 - (1.0605*Age) - (0.4410*Severity) + (1.9865*SurgicalMedical)
#Fo < .05 (significant model)
#SurgicalMedical coefficient nonsignificant
#R^2 = 0.812 (decreased)
#Ra^2 = 0.740 (decreased)
#MSE = 97.120 (Increased)
#Confirm with ols
ols_regress(modelNoAnxiety)

#TRY ELIMINATING SURGICAL MEDICAL & ANXIETY
#No SurgMed No Anxiety Variable (Model is First Order Linear Multiple Regression, MEs only)
modelNoSurgMedNoAnxiety <-lm(Satisfaction~Age+Severity, data = data)

#Obtain Results
summary(modelNoSurgMedNoAnxiety)
#Satisfaction = 139.9233 - (1.0462*Age) - (0.4359*Severity)
#Fo < .05 (significant model)
#Both coefficients significant
#R^2 = 0.810 (decreased)
#Ra^2 = 0.792 (Increased)
#MSE = 93.740 (Decreased)
#Confirm with ols
ols_regress(modelNoSurgMedNoAnxiety)

#Test Fit of AllInModel
ols_step_all_possible(AllInModel)
#Best Models: 
#Satisfaction ~ Age+Severity
#Ra^2 = 0.792
#Mallow's Cp = 1.95
#Satisfaction ~ Age+Severity+Anxiety
#Ra^2 = 0.789
#Mallow's Cp = 3.29


#Perform All Possible Regressions (for Variable Selection)
#Use leaps() b/c it performs an exhaustive search for the best subsets of the variables in x for predicting y in a linear regression
fitAll = regsubsets(Satisfaction~.,data=data, nbest =1, nvmax = NULL, force.in = NULL, force.out = NULL, method = 'exhaustive')
summary(fitAll)
fitAllOutput = summary(fitAll)
names(fitAllOutput)
as.data.frame(fitAllOutput$outmat)
View(fitAllOutput)
#Which model has the highest R^2?
which.max(fitAllOutput$rsq)
#The model with all 4 variables has the highest R^2
#See which model
#Variables marked with TRUE are the chosen ones
fitAllOutput$which[4,]
#Age + Severity + Surgical-Medical + Anxiety

#Plot Best R^2
par(mfrow = c(2,2))
plot(fitAllOutput$rsq, xlab = "Number of Variables", ylab = "R^2", type = "b")
best.r2=which.max(fitAllOutput$rsp)
points(best.r2,fitAllOutput$rsq[best.r2], col = "red", cex = 2, pch =20)

#Which model has the highest Ra^2?
which.max(fitAllOutput$adjr2)
#The model with 2 variables has the highest Ra^2
#See which model
#Variables marked with TRUE are the chosen ones
fitAllOutput$which[4,]
#Variables Age, Severity, Surgical-Medical, & Anxiety are TRUE

#Plot Best Ra^2
plot(fitAllOutput$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "b")
best.adjr2=which.max(fitAllOutput$adjr2)
points(best.adjr2,fitAllOutput$adjr2[best.adjr2], col = "red", cex = 2, pch =20)

#Do regression with the best model
best.model <- lm(Satisfaction~Age+Severity, data=data)
summary(best.model)
#p = 1.93e-08; Ra^2 = 0.7923;both significant t-values
#Satisfaction = 139.9233 - (1.0462*Age) - (0.4359*Severity)

#Another way to run leaps()
#Take a design matrix as an argument; throw away the intercept column
xs <- model.matrix(AllInModel)[,-1]
#Look at R^2 of all subsets
r2.leaps <-leaps(xs,data$Satisfaction, nbest=1, method='r2') #nbest = 1 (1 best model for each number of predictors)
plot(r2.leaps$size, r2.leaps$r2, pch=23, bg='blue', cex=2)
#Which variable has the highest R^2?
best.model.r2 <-r2.leaps$which[which((r2.leaps$r2 == max(r2.leaps$r2))),]
print(best.model.r2)
#All variables fit

#Adjusted R^2 of all subsets
adjr2.leaps <-leaps(xs,data$Satisfaction, nbest =1, method='adjr2') #nbest = 1 (1 best model for each number of predictors)
plot(adjr2.leaps$size, adjr2.leaps$adjr2, pch=23, bg = 'blue', cex=2)
best.model.adjr2 <-adjr2.leaps$which[which((adjr2.leaps$adjr2 == max(adjr2.leaps$adjr2))),]
print(best.model.adjr2)
#Columns 1 (Age) and 2 (Severity) only

#Validate RMSE
set.seed(12022019)
numVars = ncol(data)-1
trnIdx = sample(c(TRUE, FALSE), nrow(data), rep = TRUE)
tstIdx = (!trnIdx)
fitAllValidateRMSE = regsubsets(Satisfaction~., data=data[trnIdx,], nvmax = numVars)
testMat = model.matrix(Satisfaction~., data=data[tstIdx,])

testError = rep(0, times = numVars)
for (i in seq_along (testError)){
  coefs = coef(fitAllValidateRMSE, id = i)
  pred = testMat[,names(coefs)] %*% coefs
  testError[i] <- sqrt(mean((data$Satisfaction[tstIdx]-pred)^2))
}
testError
#8.904410 6.679043 7.010029 7.001217

#Plot
plot(testError, type = 'b', ylab = "Test Set RMSE", xlab = "Number of Predictors")

#Which variable has the lowest error?
which.min(testError)
#2 variable model has the lowest error

#Determine which 2 variables it is
coef(fitAllValidateRMSE, which.min(testError))
#Age (-1.0260027)
#Severity (-0.3419519)

#Another way to run all possible regressions using olsr:
ols_step_all_possible(AllInModel)
ols_step_all_possible_betas(AllInModel)
ols_step_best_subset(AllInModel)
#Same results as when using leap()

#Try Severity Cubed
corr.test(Satisfaction, Severity)
#[1] -0.65
data$SeverityCubed <-data$Severity*data$Severity*data$Severity
cor.test(Satisfaction, data$SeverityCubed)
#-0.6873331  (improvement)
corr.test(Age, data$SeverityCubed)
#Age & Severity r = 0.529
#Create model
modelSeverityCubed <-lm(Satisfaction~Age+SeverityCubed+`Surgical-Medical`+Anxiety, data = data)
#Obtain Result
summary(modelSeverityCubed)
#Ra^2 = 0.7846
#Compare to AllInModel: Ra^2 = 0.7819
#Improvment
#Lack of Fit
anova(modelSeverityCubed, AllInModel)

ols_test_f(modelSeverityCubed)
#p = 0.8936556 

#Try Anxiety Cubed
corr.test(Satisfaction, Anxiety)
#[1] -0.51
cor.test(Satisfaction, data$AnxietyCubed)
#-0.4908339 (not an improvement)
cor.test(Age, data$AnxietyCubed)
#
#Anxiety & Age r = 0.621
data$AnxietyCubed <-data$Anxiety*data$Anxiety*data$Anxiety
modelAnxietyCubed <-lm(Satisfaction~Age+Severity+`Surgical-Medical`+AnxietyCubed, data=data)
#Obtain Result
summary(modelAnxietyCubed)
#Ra^2=0.7842
#Compare to AllInModel: Ra^2 = 0.7819
#Improvment
#Improvment
#Lack of Fit
anova(modelAnxietyCubed, AllInModel)

ols_test_f(modelAnxietyCubed)
#p = 0.8936556 

#Try Anxiety Cubed AND Severity Cubed
modelAnxietyAndSeverityCubed <-lm(Satisfaction~Age+SeverityCubed+`Surgical-Medical`+AnxietyCubed,data=data)
#Obtain Result
summary(modelAnxietyAndSeverityCubed)
#Ra^2 = 0.7841
#Compare to AllInModel: Ra^2 = 0.7819
#Improvment
#AnxietyCubed Nonsignificant
#Lack of Fit
anova(modelAnxietyAndSeverityCubed, AllInModel)

ols_test_f(modelAnxietyCubed)
#p = 0.7551041 

#Confirm coefficients
coefficients(modelAnxietyAndSeverityCubed) #model coefficients
#Intercept: 1.285138e+02
#Age: -1.131516e+00
#SeverityCubed: -6.268721e-05
#SurgicalMedical: 2.823530e+00
#AnxietyCubed: 1.668141e-02
#Confirm with ols package
ols_regress(modelAnxietyAndSeverityCubed)
#Remember, Fc = (SSR/k) / (SSE/n-k+1)
#Alpha = .05
#Fo = F(1-alpha, df, df)
#Fo = F(1-.05,4,20)
qf(0.95,4,20)
#Fo = 2.866081

#Confidence Intervals (95%)
confint(modelAnxietyAndSeverityCubed)
#Age: Small Range, does not include 0
#Severity: Small range, does not include 0
#Surgical-Medical: Large range, includes 0
#Anxiety: Large range, includes 0

#Test Fit of modelAnxietyAndSeverityCubed
ols_step_all_possible(modelAnxietyAndSeverityCubed)
#Best Models: 
#Satisfaction ~ Age+SeverityCubed
#Ra^2 = 0.795
#Mallow's Cp = 1.88
#Satisfaction ~ Age+SeverityCubed+AnxietyCubed
#Ra^2 = 0.788
#Mallow's Cp = 3.64
#AllVariables
#Ra^2 = 0.784
#Mallow's Cp 5

#Perform All Possible Regressions (for Variable Selection)
#Use leaps() b/c it performs an exhaustive search for the best subsets of the variables in x for predicting y in a linear regression
fitAllWithCubes = regsubsets(Satisfaction~.,data=data, nbest =1, nvmax = NULL, force.in = NULL, force.out = NULL, method = 'exhaustive')
summary(fitAllWithCubes)
fitAllWithCubesOutput = summary(fitAllWithCubes)
names(fitAllWithCubesOutput)
as.data.frame(fitAllWithCubesOutput$outmat)
View(fitAllWithCubesOutput)
#Which model has the highest R^2?
which.max(fitAllWithCubesOutput$rsq)
#The model with all variables has the highest R^2
#See which model
#Variables marked with TRUE are the chosen ones
fitAllWithCubesOutput$which[6,]
#Age + Severity + Surgical-Medical + Anxiety

#Plot Best R^2
par(mfrow = c(2,2))
plot(fitAllWithCubesOutput$rsq, xlab = "Number of Variables", ylab = "R^2", type = "b")
best.r2=which.max(fitAllWithCubesOutput$rsp)
points(best.r2,fitAllWithCubesOutput$rsq[best.r2], col = "red", cex = 2, pch =20)

#Which model has the highest Ra^2?
which.max(fitAllWithCubesOutput$adjr2)
#The model with 2 variables (Age, SeverityCubed) has the highest Ra^2
#See which model
#Variables marked with TRUE are the chosen ones
fitAllWithCubesOutput$which[6,]
#Variables Age, Severity, Surgical-Medical, Anxiety, AnxietyCubed, SeverityCubed are TRUE

#Plot Best Ra^2
plot(fitAllWithCubesOutput$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "b")
best.adjr2=which.max(fitAllWithCubesOutput$adjr2)
points(best.adjr2,fitAllWithCubesOutput$adjr2[best.adjr2], col = "red", cex = 2, pch =20)

#Do regression with the best model
revisedPolynomialModel <- lm(Satisfaction~Age+SeverityCubed, data=data)
summary(revisedPolynomialModel)
#p = 1.027e-08 (significant); Ra^2 = 0.7951 (higher);both significant t-values
#Satisfaction = 125.957 - (1.018*Age) - (6.256002e-05*Severity)
#Confirm coefficients
coefficients(revisedPolynomialModel) #model coefficients
#Confirm with ols package
ols_regress(revisedPolynomialModel)
#Confidence Intervals
confint(revisedPolynomialModel)

#Previous Best Model:
#p = 1.93e-08; Ra^2 = 0.7923;both significant t-values
#Satisfaction = 139.9233 - (1.0462*Age) - (0.4359*Severity)

#REDO ALL RESIDUAL ANALYSIS
View(data)

#Plot New Model
plot(data$Age+data$SeverityCubed, data$Satisfaction, data=data)
lines(lowess(data$Age+data$SeverityCubed, Satisfaction))
#Appears linear
#Can also plot with ols library
ols_plot_reg_line(data$Satisfaction,data$Age+data$SeverityCubed)
ols_plot_response(revisedPolynomialModel)

#Obtain Fitted Values
fitted(revisedPolynomialModel) #predicted values

#Obtain regression diagnostics for each observation: y_hat, coefficients, sigma, weighted residual
influence(revisedPolynomialModel) #regression diagnostics
#Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) #optional 4 graphs/page 
plot(revisedPolynomialModel)
#Regression Diagnostics Battery
ols_plot_diagnostics(revisedPolynomialModel)

#Obtain residuals
residuals(revisedPolynomialModel) #residuals
e <-residuals(revisedPolynomialModel)
e
boxplot(e, ylab = "Residuals")

#Plot residuals against Y_hat
yhat <- fitted(revisedPolynomialModel)
plot(yhat, e, xlab = "Fitted Values", ylab = "Residuals", ylim = c(-20,20))
abline(h=0, lty = 2)

#Plot residuals against each of the predictor variables
#Age Residuals
plot(Age,e, xlab="Age", ylab = "Residuals", ylim = c(-20,20))
abline(h=0, lty=2)
#Severity Residuals
plot(data$SeverityCubed, e, xlab = "SeverityCubed", ylab = "Residuals", ylim = c(-20,20))
abline(h=0, lty=2)

#3d Scatterplot
scatterplot3d(data$Age, data$SeverityCubed, e, xlab = "Age", ylab = "SeverityCubed", zlab = "Residuals")

#More Residuals
summary(revisedPolynomialModel)$residuals
ols_plot_resid_stud(revisedPolynomialModel)
#Residual Normality Test
ols_test_normality(revisedPolynomialModel)
#Correlation Between Observed Residuals and Expected Residuals Under Normality
ols_test_correlation(revisedPolynomialModel)
#Correlation = 0.9452152
#Residual vs Fitted Values Plot
ols_plot_resid_fit(revisedPolynomialModel)
#Residual Histogram
ols_plot_resid_hist(revisedPolynomialModel)
#Studentized Residuals vs Leverage Plot
ols_plot_resid_lev(revisedPolynomialModel)
#Deleted Studentized residual vs predicted values
ols_plot_resid_stud_fit(revisedPolynomialModel)
#Residuals
r<-resid(revisedPolynomialModel)
r
#QQ Plot
qqnorm(resid(revisedPolynomialModel))
ols_plot_resid_qq(revisedPolynomialModel)
#Predictively adjusted residuals
(pr<-resid(revisedPolynomialModel)/(1-lm.influence(revisedPolynomialModel)$hat))
#Construct residual vs predicted response
plot(predict(revisedPolynomialModel), resid(revisedPolynomialModel))
#Cross-validated residuals
#Regular RSS is
sum(r^2)
#2034.492
#PRESS is
sum(pr^2)
#2583.56
#Note PRESS is bigger because predicting is harder than fitting
#Another way to calculate the PRESS statistic
PRESS<-function(revisedPolynomialModel){pr<-residuals(revisedPolynomialModel)/(1-lm.influence(revisedPolynomialModel$hat)
                             sum(pr^2)}
PRESS(revisedPolynomialModel)
#Same result: 2583.56

#standardized residuals
ols_plot_resid_stand(revisedPolynomialModel)

#Normal Probability Plot of Residuals
n <-length(e)
MSE <-sum(e^2)/(n-4)
RankofRes <-rank(e)
Zscore <- qnorm((RankofRes-0.375)/(n+0.25))
ExpRes <- Zscore * sqrt(MSE)
plot(ExpRes, e, xlab = "Expected Score", ylab = "Residuals")
abline(a = 0, b = 1)

#Detect Influence with Leverage
#The observed value of y_i is influential if h_i > [2(k+1)]/n
#Where h_i = leverage for the ith obervation
#k = # of betas in the model (excluding b_0)
# [2(2+1)]/n = [2(3)/25] = 6/25 = 0.5
ols_leverage(revisedPolynomialModel)

# VIF = (1/1-R^2). VIF > 5 indicates associated regression coefficients are poorly estimated b/c multicollinearity
ols_vif_tol(revisedPolynomialModel)
#Result: All VIFs < 5
#Age Tolderance = 0.675
#Age VIF = 1.48
#SeverityCubed Tolerance = 0.675
#SeverityCubed VIF = 1.48

#Check for Collinearity
ols_coll_diag(revisedPolynomialModel)
vcov(revisedPolynomialModel) #covariance matrix for model parameters 

#Cook’s Distance: Combines leverage and residuals
#Higher value, the better
#Lowest Value = 0
#Conventional Cut off is 4/n
ols_plot_cooksd_bar(revisedPolynomialModel)
ols_plot_cooksd_chart(revisedPolynomialModel)
cooks.distance(revisedPolynomialModel)

#dfbetas:measures the difference in each parameter estimate with and without the influential point
ols_plot_dfbetas(revisedPolynomialModel)
dfbeta(revisedPolynomialModel)

#lack of fit
ols_test_f(revisedPolynomialModel)
#Fail to reject H0; p = 0.7719677 


