#***************Importing Libraries ***************

library(MASS)
library(ISLR)
library(scales)

#************** SIMPLE LINEAR REGRESSION **********
#*** DATASET BOSTAN ***
# names(Boston) [names of the variables]
# dim(Boston) [dimensions of dataset]
# fix(Boston) [editing the dataset]

# ?Boston [inbuild dataset]
# str(Boston) [STRUCTURE of object]
# attach(Boston)

dataset = data.frame(Boston$lstat,Boston$medv)

#**************************************************

#*** GRAPHICAL ANALYSIS *** #1
# Visualize the linear relationship between the predictor and response
scatter.smooth(x =Boston$lstat, y = Boston$medv ,col ='steel blue' 
                                    ,pch = 20)

# To spot any outlier observations
boxplot(x =Boston$lstat, y = Boston$medv ,col ='steel blue' 
                                    ,pch = 20)

# To see the distribution of the predictor variable
plot(density(x =Boston$lstat, y = Boston$medv))
polygon(density(Boston$lstat), col="dark green")

# Correlation
cor(x =Boston$lstat, y = Boston$medv)
corrplot(cor(x =Boston$lstat, y = Boston$medv) 
                          ,method = 'number') #plotting

#**************************************************

# Analysis before outliers removal

#FITTING
lm.fit = lm(Auto$mpg~. ,data = dataset)
summary(lm.fit)

# PLOTTING ANALYSIS
par(mfrow = c(2,2))
plot(lm.fit, pch =20 ,col ='firebrick')

#**************************************************

# ***OUTLINER REMOVAL***
#BOSTON>LSTAT
quantiles = quantile(dataset$Boston.lstat,probs = c(.25,.75))
range = 1.5*IQR(dataset$Boston.lstat)
n_dataset = subset(dataset,dataset$Boston.lstat > (quantiles[1] - range) & 
                  dataset$Boston.lstat < (quantiles[2] + range))
#Outliers replacement
#dataset$Boston.lstat <- squish(dataset$Boston.lstat, 
#                 quantile(dataset$Boston.lstat, c(.05, .95)))

#BOSTON>MEDV
quantiles = quantile(n_dataset$Boston.medv,probs = c(.25,.75))
range = 1.5*IQR(n_dataset$Boston.medv)
m_dataset = subset(n_dataset,n_dataset$Boston.medv > (quantiles[1] - range) & 
                     n_dataset$Boston.medv < (quantiles[2] + range))

# Analysing via plotting #2
scatter.smooth(x =dataset$Boston.lstat, y = dataset$Boston.medv ,
                  col ='steel blue' ,pch = 20)
length(m_dataset$Boston.lstat)
boxplot(dataset$Boston.lstat,horizontal = TRUE ,xlab = 'lstat')
boxplot(m_dataset$Boston.medv,horizontal = TRUE,xlab ="medv")

#**************************************************

# Split Dataset [ Method-1 ]
library(caTools)
set.seed(123)
split = sample.split(m_dataset$Boston.medv, SplitRatio = 0.7)
training_set = subset(m_dataset , split==TRUE )
test_set = subset(m_dataset , split==FALSE )

#**************************************************

# *** Building MODEL ***
#Fitting
lm.fit = lm(Boston.medv~Boston.lstat ,data = training_set) ; print(lm.fit)
scatter.smooth(x =training_set$Boston.lstat, y = training_set$Boston.medv,
                  col ='steel blue' ,pch = 20)
#Linear Regression Analysis
summary(lm.fit)

# Statistics________________________________Criteriom
#p-Value_________________________________more the stars beside the variable's
#                                         p-Value, the more significant
#Std. Error______________________________Closer to zero the better
#R-Squared, Adj R-Squared _______________Higher the better (> 0.70)
#F-Statistic_____________________________Higher the better
#AIC, BIC________________________________Lower the better
#Mallows cp______________________________Should be close to the number
#                                         of predictors in model

#*******************************************************

#PREDICTING
pred =predict(lm.fit ,newdata = test_set)
scatter.smooth(x =test_set$Boston.lstat, y = test_set$Boston.medv,
               col ='magenta' ,pch = 20)
summary(pred)

#*******************************************************

# PREDICTION ACCURACY
actual_preds = data.frame(cbind(actuals =test_set$Boston.medv , predicteds =pred))

# Statistics_________________________________________Criterion

#Corellation_________________________________________closer to extreme the better
#                                                       collrelation [-1,1]
#MAPE (Mean absolute percentage error)_______________Lower the better
#MSE (Mean squared error)____________________________Lower the better
#Min_Max Accuracy => mean(min(actual, predicted)
#                     /max(actual, predicted))_______Higher the better
# 1
correlation_accuracy = cor(actual_preds)
# 2
mape = mean(abs(actual_preds$predicteds - actual_preds$actuals)/actual_preds$actuals)
# 3
min_max_accuracy = mean(apply(actual_preds, 1, min) / apply(actual_preds, 1, max))

#RESIDUAL ANALYSIS
plot(predict(lm.fit),residuals(lm.fit),col = 'dark green',pch =20)
abline(0,0,lty = 3)#line type(lty) ,line width(lwd)
plot(hatvalues(lm.fit),col = 'orange3',pch =20)
which.max(hatvalues(lm.fit))

#**************************************************************
#**************************************************************

# when we donot want the intercept 
# --> lm.fit( y ~ x + 0 )