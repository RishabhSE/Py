#***************Importing Libraries ***************

library(MASS)
library(ISLR)
library(plotly)
library(corrplot)

#************** MULTIPLE LINEAR REGRESSION **********
#*** DATASET BOSTAN ***
# names(Boston) [names of the variables]
# dim(Boston) [dimensions of dataset]
# fix(Boston) [editing the dataset]

# ?Boston [if--> inbuild dataset]
# str(Boston) [STRUCTURE of object]
# attach(Boston)
# sum( is.na( Boston$lstat )) [count no. of missing values]

dataset = data.frame(Boston$lstat,Boston$age,Boston$medv)

#**************************************************

#*** GRAPHICAL ANALYSIS *** #1

# Scatterplot matrix of all variables
plot(Boston , col ='steelblue4' ,pch = 20 )

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

# Analysis before outliers removal
#Fitting
lm.fit = lm(Auto$mpg~. ,data = dataset)
summary(lm.fit)

# PLOTTING ANALYSIS
par(mfrow = c(2,2))
plot(lm.fit, pch =20 ,col ='firebrick')

#**************************************************

# Split Dataset [ Method-1 ]
library(caTools)
set.seed(123)
split = sample.split(dataset$Boston.medv, SplitRatio = 0.7)
training_set = subset(dataset , split==TRUE )
test_set = subset(dataset , split==FALSE )

# [ Method-2 ]
#set.seed(101) 
# Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
#sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)),
#                                         replace = F)
#train <- data[sample, ]
#test  <- data[-sample, ]

#******************************************************
# *** Building MODEL ***

# MULTIPLE LINEAR Analysis
# Accessing individual componentc(summary)
# summary()$r.sq------------->R SQUARE
# summary()$sigma----------->RSE
# summary()&coef----------->Coefficient

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