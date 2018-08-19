#***************Importing Libraries ***************

library(MASS)
library(ISLR)
library(plotly)
library(corrplot)

#************** MULTIPLE LINEAR REGRESSION **********
#*** DATASET AUTO ***
# names(Auto)
# fix(Auto)
# To find more about dataset
# ?Auto [inbuild dataset]
# str(Auto) [STRUCTURE of object]
# attach(Auto)
dataset = data.frame(Auto[ , -9])

#**************************************************

#*** GRAPHICAL ANALYSIS *** #1

# Scatterplot matrix of all variables
plot(dataset , col ='steelblue4' ,pch = 20 )

# Visualize the linear relationship between the predictor and response
scatter.smooth(x =Auto$displacement, y = Auto$mpg ,
                              col ='slateblue4' ,pch = 20)

# To spot any outlier observations
boxplot(x =Auto$year, y = Auto$mpg ,
                              col ='steel blue' ,pch = 20)

# To see the distribution of the predictor variable
plot(density(x =Auto$displacement, y = Auto$mpg))
polygon(density(Auto$displacement), col="deeppink3")

# Correlation
cor(x =Auto$displacement, y = Auto$mpg)
corrplot(cor(dataset), method = 'number') #plotting
# RESULT 
# outliers -->horsepower,acceleration

#**************************************************
#Fitting

# INTERACTION TERMS
# (LSTAT * AGE)---->VARIABLES-->LSTAT,AGE,LSTAT:AGE
lm.fit_1 = lm(mpg~ weight*year*origin*displacement ,data = dataset)
summary(lm.fit_1)$r.sq #0.86 --->best prediction

# Non Linear Transformation Predictors
lm.fit_2 = lm(mpg ~ I(weight^2)+I(year^2)+I(origin^2)+I(displacement^2) 
              , data = dataset)
summary(lm.fit_2)$r.sq # 0.78

# Logarithmic transformation
lm.fit_3 = lm(mpg~log(weight) +log(year)+ log(origin)+log(displacement)
                                ,data = dataset)
summary(lm.fit_3)$r.sq #0.8399


# PLOTTING ANALYSIS
par(mfrow = c(2,2))
plot(lm.fit_3, pch =20 ,col ='coral4')

# ANOVA-------------->(To quantify superiority b/w linear & quadratic fit)
anova(lm.fit_2,lm.fit_3)
#*************************************************

#PREDICTING
pred =predict(lm.fit_3 ,newdata = dataset)
summary(pred)

#*******************************************************

# PREDICTION ACCURACY
actual_preds = data.frame(cbind(actuals =dataset$mpg , 
                                predicteds =pred))

plot(predict(lm.fit),residuals(lm.fit),col = 'yellowgreen',pch =20)
abline(0,0,lty =3)#line type(lty) ,line width(lwd)






