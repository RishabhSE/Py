# *******************************************************

# *** DATA EXPLORATION *** 

# names(Hitters) [names of the variables]
# dim(Hitters) [dimensions of Hitters]
# fix(Hitters) [editing the Hitters]

# ?Hitters [inbuild Hitters]
# str(Hitters) [STRUCTURE of object]
# attach(Hitters)

#********************************************************* 
library(pls)
library(ISLR)

# sum( is.na( Hitters$Salary )) #[count no. of missing values]
Hitters = na.omit(Hitters)

x = model.matrix( Salary~. ,Hitters)[ ,-1 ]
y = Hitters$Salary

# Splitting
set.seed(1)
train=sample( 1:nrow(x) ,size = nrow(x)/2)
test =( -train )
y.test = y[test]
#********************************************************* 
# ***PRINCIPAL COMPONENTS REGRESSION ***

# FITTING
set.seed(2)
pcr.fit = pcr( Salary~. ,data =Hitters ,subset =train ,scale =T ,validation ="CV")
# syntax similar to lm()
# validation ="CV" :Causes pcr() to compute C-V Errors for each
# value of M( the no. of Principal Components)
summary(pcr.fit)

# PLOTING C-V MSE 
validationplot( pcr.fit, val.type ="MSEP" ,type = "b",pch =20)

# PRIDICTION
pcr.predict = predict( pcr.fit ,x[test ,] ,ncomp = 7)
mean(( pcr.predict -y.test)^2)

# Finally, fit PCR on the full data set
pcr.fit = pcr( y~x ,scale =T ,ncomp = 7)
summary(pcr.fit)

#********************************************************* 
# ***PARTIAL LEAST SQUARE ***
# syntax similar to pcr()

# FITTING
set.seed(1)
pls.fit = plsr( Salary~. ,data =Hitters ,subset =train ,scale =T ,validation ="CV")
summary(pls.fit)

# PLOTING C-V MSE 
validationplot( pls.fit, val.type ="MSEP" ,type = "b",pch =20)

# PRIDICTION
pls.predict = predict( pls.fit ,x[test ,] ,ncomp = 2)
mean(( pls.predict -y.test)^2)

# Finally, fit PLS on the full data set
pls.fit = plsr( y~x ,scale =T ,ncomp = 2)
summary(pls.fit)

#********************************************************* 




















