#***************Importing Libraries ***************

library(MASS)
library(ISLR)
library(scales)

#************** DATA ANALYSIS *********************
#*** DATASET HITTERS ***
# names(Hitters) [names of the variables]
# dim(Hitters) [dimensions of dataset]
# fix(Hitters) [editing the dataset]

# ?Hitters [inbuild dataset]
# str(Hitters) [STRUCTURE of object]
# attach(Hitters)

# sum( is.na( Hitters$Salary )) [count no. of missing values]
# Hitters = na.omit(Hitters)
dataset = data.frame(Hitters)

#**************************************************

# *** BEST SUBSET SELECTION ***

library(leaps)
# Used to perform 'Best Subset Selection'--->regsubset()
# Best is Qualified using RSS

regfit.full = regsubsets( Salary ~. , Hitters ,nvmax = 19)
# by Default nvmax = 8[ N no. of variable model]
summary(regfit.full)
# the best set of variables for each model size indicated by '*'
reg.summary = summary(regfit.full)

# Plotting [RSS,AD.R^2,Cp,BIC] At Once for all Models**********
par( mfrow = c(2,2))
plot( reg.summary$rss ,xlab = "No. of Var.", ylab = "RSS", type = "l")

# For Adjusted R-square********************
plot( reg.summary$adjr2 ,xlab = "No. of Var.", ylab = "Ad. R-Square"
          , type = "l")
for_best = which.max( reg.summary$adjr2 )# find MAX. point of a vector(location)
# Adjusted R2 tells us that the best model is that with 11 variables
#plot this point
points( for_best,reg.summary$adjr2[for_best] ,col ='red' ,cex =2,pch = 20)

# Similarly for Cp************************
plot( reg.summary$cp ,xlab="No. of Variables",ylab ="Cp",type="l")
for_best=which.min(reg.summary$cp)
points(for_best , reg.summary$cp[for_best],col ="red",cex =2,pch=20)

# Similarly for BIC**********************
plot( reg.summary$bic ,xlab="No. of Variables",ylab ="BIC",type="l")
for_best = which.min(reg.summary$bic)
points(for_best , reg.summary$bic[for_best],col ="red",cex =2,pch=20)

# Displays the selected variables for the best model
plot(regfit.full , scale = "r2", col ='brown4')
plot(regfit.full , scale = "Cp")
plot(regfit.full , scale = "bic")
plot(regfit.full , scale = "adjr2")

# *************************************************

# *** Forward & Backward Stepwise Selection ***

# FORWARD SEL.
regfit.fwd = regsubsets( Salary ~. , Hitters ,nvmax = 19 ,method = "forward")
summary(regfit.fwd)

# FORWARD SEL.
regfit.bwd = regsubsets( Salary ~. , Hitters ,nvmax = 19 ,method = "backward")
summary(regfit.bwd)

# *************************************************

# ***Chosing Among Models ---->VSA ***

# The Determination of which model of given size is best
# must be made using only the " TRAINING SET OBSEVATION "

# If full dataset is used--> VS & CV Errors obtained will not
# be Accurate estimate of test errors

# Splitting
set.seed(1)
train=sample(c(T,F), nrow(Hitters), rep =T )
test =( !train )

regfit.best = regsubsets( Salary ~. , Hitters ,nvmax = 19)

# Now, Compute the VS Error for best model of each size 

# Creating matrix of size 'X' from data
test.mat = model.matrix(Salary~. ,data = Hitters[test ,]) 

val.errors = rep( NA,19 )
# Running loop for each size i,
# extracting coeff. for best model of that size &
# Multiplying them into appropriate column
# To form "prediction" & compute MSE
for( i in 1:19)
  {
  coeff =coef(regfit.best ,id = i )
  pred = test.mat[ , names(coeff) ]%*%coeff
  val.errors[i] = mean(( Hitters$Salary[test] - pred)^2)
  }
# Finding Best model Coefficients
coeff = coef(regfit.best , which.min( val.errors ))

# *************************************************

# ***Chosing Among Models ---->CV ***







