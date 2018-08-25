# *******************************************************
# *** DATA EXPLORATION *** 

# names(Hitters) [names of the variables]
# dim(Hitters) [dimensions of Hitters]
# fix(Hitters) [editing the Hitters]

# ?Hitters [inbuild Hitters]
# str(Hitters) [STRUCTURE of object]
# attach(Hitters)

#********************************************************* 
#***************Importing Libraries ***************

library(MASS)
library(ISLR)
library(glmnet)

#********************************************************* 

# sum( is.na( Hitters$Salary )) #[count no. of missing values]
Hitters = na.omit(Hitters)

# In R.R & LASSO we create x as matrix & y as vector [diff. formating]
x = model.matrix( Salary~. ,Hitters)[ ,-1 ]
y = Hitters$Salary

# *** RIDGE REGRESSION ***
# Obective = RSS + lambda*(sum of sq. of mag. of Coeff.)

grid = 10^seq( from = 10,to = -2, length = 100)# Lamda values in range(particular)
ridge.mod = glmnet( x, y, alpha = 0, lambda = grid )# alpha---> which model ?
# glmne() : It Standardize the variables on same scale( By default )

# dim( coef( ridge.mod))
ridge.mod$lambda[50]
# We excepts Coeff. Estimates to be much smaller( L2 norms )
# when : large value of lambda is used compared to small(lambda) value

coef( ridge.mod)[,50]
# Coeff. when lambda resulted in previous step

#Esimation test error of R.R  *********************
# Splitting
set.seed(1)
train=sample( 1:nrow(x) ,size = nrow(x)/2)
test =( -train )
y.test = y[test]

# Fitting R.R on training set
rigid.mod = glmnet( x[train] ,y[train] ,alpha = 0, lambda = grid,
                    thresh = 1e-12)
# Prediction using lambda = 4
rigid.pred = predict( rigid.mod ,s=4, newx = x[test ,])

# Calculation of test MSE
mean(( rigid.pred - y.test)^2)

# As we know that : *******************************
# at lambda = 0 : model acts as least sq. model
# at lambda = 1e10 : model acts as NULL model containing only intercept

# M-1
# test MSE with just an intercept
mean((mean( y[train]) -y.test)^2)
# For Large value
rigid.pred = predict( rigid.mod ,s= 1e10, newx = x[test ,])
mean(( rigid.pred - y.test)^2)
# same MSE as O/P

# M-2
# test MSE with least sq. model
lm( y~x , subset = train)
predict( rigid.mod, s=0 ,exact=T, type ="coefficients")[1:20, ]
# For small( ZERO ) value
rigid.pred = predict( rigid.mod ,s= 0, newx = x[test ,])
mean(( rigid.pred - y.test)^2)
# same MSE as O/P

# 





















