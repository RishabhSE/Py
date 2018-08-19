#***************Importing Libraries ***************

library(ISLR)
library(corrplot)
library(MASS)

#************** LOGISTIC REGRESSION **********
#*** DATASET SMARKET ***
# names(Smarket) [names of the variables]
# fix(Smarket) [editing the dataset]
# dim(Smarket) [Dimension of dataset]
# ?Smarket [inbuild dataset]
# str(Smarket) [STRUCTURE of object]
# attach(Smarket)
dataset = data.frame(Smarket)

#**************************************************

# Correlation
cor(x =Boston$lstat, y = Boston$medv)
corrplot(cor(dataset[ , - 9 ]) 
         ,method = 'number') #plotting
# Ploting
plot(dataset[, - 9] , col = 'coral4' ,pch = 20 )

#**************************************************

# Split Dataset [ Method-1 ]
library(caTools)
set.seed(123)
split = sample.split(dataset$Direction, SplitRatio = 0.7)
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

#************** LOGISTIC REGRESSION ***************

# FITTING
glm.fit = glm(Direction ~Lag1 + Lag2  
              ,data = training_set ,family = binomial)
# when family args not passed glm() it performs like lm() function
summary(glm.fit)

# PREDICTING
glm.probs = predict(glm.fit , type = "response" ,newdata = test_set)

contrasts(Direction) #View the constrast associated with a factor

# Creating class of predctions based on predicted probability
glm.pred = rep("Down" , 375 ) # Creates a vectors of 'Down' elememts
glm.pred[glm.probs > 0.5 ] = "Up" # Transforms to 'Up' --> probability > 0.5

table(glm.pred , test_set$Direction) # Confusion Matrix
mean( glm.pred == test_set$Direction ) # Fraction of correct predictions

#**************************************************
#**************************************************

# *** LINEAR DISCRIMINANT ANALYSIS ***

lda.fit = lda(Direction ~ Lag1 + Lag2  
              ,data = training_set)
lda.fit # CALL

# Ploting
plot(lda.fit , col = 'red4' ,pch = 20 )


