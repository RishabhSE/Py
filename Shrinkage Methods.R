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

grid = 10^seq( from = 10,to = -2, length = 100)
ridge.mod = glmnet( x, y, alpha = 0, lambda = grid )
# glmne() : It Standardize the variables on same scale( By default )

# dim( coef( ridge.mod))
ridge.mod$lambda[ 50 ]; coef( ridge.mod)[ ,50 ]
# Coeff. Estimates to be much smaller( l2 norms )
# when : large value of lambda is used compared to small(lambda) value
















