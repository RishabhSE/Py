#********************************************************* 
#***************Importing Libraries ***************

library(ISLR)
library(splines)

#********************************************************* 

# *** DATA EXPLORATION *** 

# names(Wage) [names of the variables]
# dim(Wage) [dimensions of Wage]
# fix(Wage) [editing the Wage]

# ?Wage [inbuild Wage]
# str(Wage) [STRUCTURE of object]
# attach(Wage)

#********************************************************* 

# *** SPLINES ***

# Creating grid of values at which we want predictions
# i.e Generating test data
agelims = range(age)
age.grid = seq( from =agelims[1], to =agelims[2])

# FITTING
fit = lm( wage~bs( age ,knots = c(25 ,40 ,60)) ,data =Wage); summary(fit)
# bs() : It generates the entire matrix of "basic fn." for spline
# with the specified "no. of knots"

# PREDICTING
pred = predict( fit ,newdata = list( age =age.grid) ,se =T)

# PLOTING
plot(age,wage ,col="grey" ,xlab="Age" ,ylab="Wages")
# LINE 1
lines(age.grid ,pred$fit ,col="red" ,lwd=2 ,type="l")
# LINE 2
lines(age.grid ,pred$fit+ 2*
        pred$se ,col="darkgreen" ,lwd=2 ,type="l")
# LINE 3
lines(age.grid ,pred$fit - 2*
        pred$se ,col="blue" ,lwd=2 ,type="l")
#adding cutpoints
abline(v=c(25,40,60), lty=2, col="darkgreen")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Using df() option to produce spline at Quantiles of the data
dim(bs( age ,knots = c(25 ,40 ,60)))
dim( bs( age ,df = 6))
# same dimensions

# Chossing knots at Quantiles
attr( bs( age ,df =6) ,"knots")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# IF we want to fit with natural spline

fit_2 = lm( wage~ns( age , df = 4) ,data =Wage) 
summary(fit_2)
pred_2 = predict( fit ,newdata = list( age =age.grid) ,se =T)

# PLOTING
plot(age,wage ,col="grey" ,xlab="Age" ,ylab="Wages")
lines(age.grid ,pred_2$fit ,col="brown" ,lwd=2 ,type="l")
abline(v=attr( bs( age ,df =6) ,"knots"), lty=2, col="darkgreen")

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# SMOOTHING SPLINES
# Done using smooth.splie() method

# Ist Call to Smooth.spline( specifing df =16)
# The fn. determines which value of lambda leads df =16
fit_3 = smooth.spline(age , wage, df =16)

# IInd Call to Smooth.spline
# We Select Smoothness Level by C.V method
fit_4 = smooth.spline(age , wage, cv =T); fit_4$df

# PLOTING
plot(age,wage ,col="grey" ,xlab="Age" ,ylab="Wages")
title("SMOOTHING SPLINE")

lines( fit_3 ,col ="red" ,lwd =2)
lines( fit_4 ,col ="blue" ,lwd =2)

legend("topright", legend = c(" 16 DF" ," 6.8 DF"),
        col =c("red","blue"), lty =1 ,lwd = 2, cex = 0.8 )

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# LOCAL REGRESSION
# Done using loess() method
# Alternative : locfit() Library

# FITTING
# span : Smoothing Parameter for controling bias-variance trade-off
# Span = 0.2 : each neighboor consistes of 20% of obser.
# larger the span --> smoother the fit*
fit_5 = loess(age~wage, span =0.2, data = Wage)
fit_6 = loess(age~wage, span =0.5, data = Wage)

# PREDICTING
pred_5 = predict( fit_5 ,data.frame( age =age.grid))
pred_6 = predict( fit_6 ,data.frame( age =age.grid))

# PLOTING
plot(age , wage,xlim = agelims ,cex = 0.5, col ="darkgrey" ,
          xlab="Age" ,ylab="Wages")
title("LOCAL REGRESSION")

lines( age.grid ,pred_5 ,col ="red" ,lwd =2)
lines( age.grid ,pred_6,col ="blue" ,lwd =2)

legend("topright", legend = c("Span = 0.2" ,"Span = 0.5"),
       col =c("red","blue"), lty =1 ,lwd = 2, cex = 0.8 )

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# *****************************************************

# *** GENERALIZED ADDITIVE MODELS ***











