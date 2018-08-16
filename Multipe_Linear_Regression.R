#***************Importing Libraries ***************

library(MASS)
library(ISLR)
library(plotly)
library(corrplot)

#************** MULTIPLE LINEAR REGRESSION **********
#*** DATASET BOSTAN ***
# fix(Boston)
names(Boston)
# To find more about dataset
# ?Boston [inbuild dataset]
# str(Boston) [STRUCTURE of object]
# attach(Boston)
dataset = data.frame(Boston$lstat,Boston$age,Boston$medv)

#**************************************************

#*** GRAPHICAL ANALYSIS *** #1
# Visualize the linear relationship between the predictor and response
scatter.smooth(x =Boston$lstat, y = Boston$medv ,col ='steel blue' ,pch = 20)
# To spot any outlier observations
boxplot(x =Boston$lstat, y = Boston$medv ,col ='steel blue' ,pch = 20)
# To see the distribution of the predictor variable
plot(density(x =Boston$lstat, y = Boston$medv))
polygon(density(Boston$lstat), col="dark green")
# Correlation
cor(x =Boston$lstat, y = Boston$medv)
corrplot(cor(x =Boston$lstat, y = Boston$medv) ,method = 'number') #plotting

#**************************************************

# Split Dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$Boston.medv, SplitRatio = 0.7)
training_set = subset(dataset , split==TRUE )
test_set = subset(dataset , split==FALSE )

#******************************************************
# *** Building MODEL ***

# MULTIPLE LINEAR Analysis
# Accessing individual componentc(summary)
# summary()$r.sq------------->R SQUARE
# summary()$sigma----------->RSE

#VIF()-------->car pakage,Varinace Inflation Factor

#Fitting
lm.fit_0 = lm(Boston.medv~Boston.lstat+Boston.age ,data = training_set)
summary(lm.fit_0)

# INTERACTION TERMS
# (LSTAT * AGE)---->VARIABLES-->LSTAT,AGE,LSTAT:AGE
lm.fit_1 = lm(Boston.medv~Boston.lstat*Boston.age ,data = training_set)
summary(lm.fit_1)

# Non Linear Transformation Predictors
lm.fit_2 = lm(Boston.medv~Boston.lstat + I(Boston.lstat^2) + Boston.age ,
                                         data = training_set)
# can use polynomial function----->ploy(lstat , 5 )

summary(lm.fit_2)

# Non Linear Transformation Predictors(LOGARITHMIC)
lm.fit_3 = lm(Boston.medv~log(Boston.lstat),
              data = training_set)
summary(lm.fit_3)

#*******************************************************

# ANOVA-------------->(To quantify superiority b/w linear & quadratic fit)
anova(lm.fit_2,lm.fit_3)

# PLOTTING ANALYSIS
par(mfrow = c(2,2))
plot(lm.fit_3, pch =20 ,col ='blue4')

corrplot(cor(training_set) ,method = 'number')# Plot a correlation graph

#*******************************************************

#PREDICTING
pred =predict(lm.fit_2 ,newdata = test_set)
summary(pred)

#*******************************************************

# PREDICTION ACCURACY
actual_preds = data.frame(cbind(actuals =test_set$Boston.medv , predicteds =pred))

plot(predict(lm.fit_2),residuals(lm.fit_2),col = 'yellowgreen',pch =20)
abline(0,0,lty =3)#line type(lty) ,line width(lwd)

#*******************************************************