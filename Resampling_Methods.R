# *** RESAMPLING METHODS ***

#*** DATASET AUTO ***
# names(Auto) [names of the variables]
# fix(Auto) [editing the dataset]

# ?Auto [inbuild dataset]
# str(Auto) [STRUCTURE of object]
# attach(Auto)

# *** CROSS VALIDATION ( ** VALIDATION SET APPROACH ** ) ***

library(ISLR)

# Split Dataset
set.seed(1)
train = sample( 392 , 196 )
# Change the dataset and again fit the model

# Fitting Set **************************************
# FITTING---1
lm.fit = lm(mpg ~horsepower , data = Auto , subset = train )
mean((mpg - predict( lm.fit ,Auto) )[ -train]^2 )# Calculating MSE

# FITTING---2
lm.fit_2 = lm(mpg ~ poly(horsepower,2) , data = Auto , subset = train )
mean((mpg - predict( lm.fit_2 ,Auto) )[ -train]^2 )# Calculating MSE

# FITTING---3
lm.fit_3 = lm(mpg ~poly(horsepower,3) , data = Auto , subset = train )
mean((mpg - predict( lm.fit_3 ,Auto) )[ -train]^2 )# Calculating MSE

#***************************************************
# whem compared MSE results are consistent,fit data with appropriate model

# *** CROSS VALIDATION ( ** LEAVE-ONE-OUT CV APPROACH ** ) ***

library( boot )

# Performing Linear R. using glm() function
glm.fit = glm(mpg ~horsepower , data = Auto)
cv.err = cv.glm(Auto , glm.fit)

cv.err$delta # cross validation result

# Iterative Solution
cv.error = rep(0,5) # vector for storing errors
for( i in 1:5)
{
  glm.fit = glm(mpg ~poly(horsepower, i ) , data = Auto) 
  cv.error[i] = cv.glm( Auto , glm.fit)$delta[1] # storing errors
}
cv.error # Output printing


#***************************************************

# *** CROSS VALIDATION ( ** K-FOLD CV APPROACH ** ) ***

set.seed(17)
cv.error_k = rep(0 , 10) # vector for storing errors

# ITERATION
for (i in 1:10 ) 
{ 
  glm.fit = glm(mpg ~poly(horsepower, i ) , data = Auto) 
  cv.error_k[i] = cv.glm( Auto , glm.fit ,
                          K = 10 )$delta[1] # storing errors
  
}

cv.error_k # Output printing\

# NOTE :--> Computation time much shorter than in "LOOCV"

#***************************************************

# *** THE BOOTSTRAP METHOD ***

# Can be applied to any situation
# No complex mathematical calculation

library( boot )

# STEP 1 : Create fn. to return an estimated 'alpha'
# Eg. function which minimizes the risk/Varinace
alpha.fn = function( data , index )
{ 
  
  X = data$X[ index ]
  Y = data$Y[ index ]
    return(( var(Y) - cov( X,Y ) ) / ( var( X ) + 
                                var( Y ) - 2* cov( X , Y )))
}

# STEP 2 :
# Method-1
# to estimate 'alpha' for all 100 observation
alpha.fn( Portfolio , 1:100 )

# Method-2
# randomly select with replacement
set.seed(1)
alpha.fn( Portfolio , sample( 100 ,100 ,replace = T))

# Method-3
# Performing above method n-number of times &
#calculating corresponding value of 'alpha' 
# USING BOOT FUNCTION

boot_obj = boot(Portfolio , alpha.fn , R = 1000 )
# boot( ) calls the statistic function R times. Each time, 
# it generates a set of random indices, with replacement\

print(boot_obj); plot(boot_obj) # printing & ploting

# boot.ci( ) function to obtain "Confidence Intervals"
boot.ci(boot_obj , conf = 0.95 , type = 'norm' )

#**********************************************

# Estimating the accuracy of Linear R. Model --->Using BootStraping
# STEP 1 : Create fn. to return an estimated value
boot.fn = function(data , index )
  return( coef( lm( mpg~horsepower ,data = data ,subset = index ) ) )

# STEP 2 : 
# Estimate for intercept and slope terms( To full set )
boot.fn( Auto , 1:392 )

# Estimate for intercept and slope terms( Random Sampling with replacement )
set.seed(1)
boot.fn( Auto , sample(392 ,392 , replace = TRUE))

# Estimating Standard Errors. 
boot( Auto , boot.fn , 1000 )



