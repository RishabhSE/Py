# ***************************************************
# *** CLASSIFICATION TREE ***

library(tree)
library(ISLR)

attach(Carseats)
str(Carseats)

# If sales value > 8 :YES ,If not : NO
High = ifelse( Sales <=8, "No", "Yes")
# Merging
Carseats = data.frame( Carseats ,High)

# SPLITING
set.seed(2)
train = sample(1:nrow( Carseats) ,200)
Carseats.test = Carseats[ -train, ]
High.test = High[-train]

# FITTING
tree.carseats = tree( High~. -Sales ,Carseats ,subset = train)
summary( tree.carseats) 
# training error rate = 10.5%
# Small Deviance --->Good Fit on Training Data

# PREDICTION
tree.pred = predict( tree.carseats, Carseats.test ,type = "class")
table(tree.pred , High.test)
mean( tree.pred == High.test )

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# CROSS VALIDATION ( PRUNING )

# FITTING

set.seed(3)
cv.carseats = cv.tree( tree.carseats, FUN = prune.misclass)
# prune.misstree : to indicate that we want classf. error rate
# to guide the CV & Pruning process,
# rather than default( cv.tree) ---> deviance
names( cv.carseats)
# size : no. of terminal node of each tree
# dev : Corresponding error rate
# k : value of cost complexity parameter used

# PLOTTING
# Error rate corresponding to fn. of both "size" & "k"
par(mfrow = c(1,2))
plot( cv.carseats$size ,cv.carseats$dev ,type ="b", pch = 20)
plot( cv.carseats$k ,cv.carseats$dev ,type ="b",pch = 20)

# Prune the 9-node tree
prune.carseats = prune.misclass( tree.carseats , best =9)
plot(prune.carseats)
text(prune.carseats ,pretty = 0)

# PREDICTION
tree.pred = predict( prune.carseats, Carseats.test ,type = "class")
table(tree.pred , High.test)
mean( tree.pred == High.test )

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# ***************************************************

# *** REGRESSION TREES ***

library(MASS)

set.seed(1)
train = sample( 1:nrow(Boston) ,nrow(Boston)/2)
tree.boston = tree( medv~. ,Boston ,subset =train)
summary(tree.boston)
# In context to R. Tree : 
# Deviance is  simply ---> "sum of sq. of errors" for tree

# PLOTTING
plot(tree.boston)
text(tree.boston ,pretty = 0)

# CROSS VALIDATION ( PRUNING )

cv.boston = cv.tree( tree.boston)
plot( cv.boston$size ,cv.boston$dev ,type ="b" ,pch =20)

# Prune the 5-node tree
prune.boston = prune.tree( tree.boston ,best = 5)
plot(prune.boston)
text(prune.boston ,pretty = 0)

# PREDICTION
yhat = predict( tree.boston ,newdata = Boston[-train,])
boston.test = Boston[ -train,"medv"]
plot(yhat ,boston.test)
abline(0,1)
mean((yhat -boston.test)^2)# test-set MSE associated with R. Trees

# ***************************************************

# *** RANDOM FOREST & BAGGING ***
# As Bagging is "special case" of R.F

library(randomForest)
set.seed(1)

# FITTING
bag.boston = randomForest( medv~. ,data =Boston ,subset =train,
                           mtry = 13, importance = T)

# mtry = 13 : all 13 predictors should be considered 
# for each split of tree i.e. "BAGGING" should be done

# PREDICTION
yhat.bag = predict( bag.boston, newdata = Boston[-train,])
plot(yhat.bag ,boston.test ,pch =20)
mean((yhat.bag-boston.test)^2) # test MSE

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Changing No. fo trees grown : ntree()

bag.boston = randomForest( medv~. ,data =Boston ,subset =train,
                           mtry = 13, ntree =25 )

yhat.bag = predict( bag.boston, newdata = Boston[-train,])
plot(yhat.bag ,boston.test ,pch =20)
mean((yhat.bag-boston.test)^2) # test MSE

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# RANDOM FOREST

set.seed(1)
# FITTING
rf.boston = randomForest( medv~. ,data =Boston ,subset =train,
                           mtry = 6, importance = T)
# mtry :
# GENERALLY : By Default while building a R.F.
# Of R. Trees ----> Uses p/3 variables
# Of Class. Trees ----> Uses sqrt(p) variables

yhat.rf = predict( rf.boston, newdata = Boston[-train,])
plot(yhat.rf ,boston.test ,pch =20)
mean((yhat.rf-boston.test)^2)

# View the importance of each variable
importance(rf.boston)
# 














