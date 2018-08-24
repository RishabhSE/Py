#***************Importing Libraries ***************

library(MASS)
library(ISLR)
library(scales)

#************** SIMPLE LINEAR REGRESSION **********
#*** DATASET BOSTAN ***
# names(Boston) [names of the variables]
# dim(Boston) [dimensions of dataset]
# fix(Boston) [editing the dataset]

# ?Boston [inbuild dataset]
# str(Boston) [STRUCTURE of object]
# attach(Boston)

dataset = data.frame(Boston$lstat,Boston$medv)

#**************************************************

#*** GRAPHICAL ANALYSIS *** #1
# Visualize the linear relationship between the predictor and response
scatter.smooth(x =Boston$lstat, y = Boston$medv ,col ='steel blue' 
                                    ,pch = 20)

# To spot any outlier observations
boxplot(x =Boston$lstat, y = Boston$medv ,col ='steel blue' 
                                    ,pch = 20)

# To see the distribution of the predictor variable
plot(density(x =Boston$lstat, y = Boston$medv))
polygon(density(Boston$lstat), col="dark green")

# Correlation
cor(x =Boston$lstat, y = Boston$medv)
corrplot(cor(x =Boston$lstat, y = Boston$medv) 
                          ,method = 'number') #plotting

#**************************************************

# *** MISSING VALUE TREATMENT ***
# sum( is.na( Boston$lstat )) [count no. of missing values]

# METHOD-1 [ Deleting The Observations ]
if(FALSE)
  dataset = na.omit(dataset)

# METHOD-2 [ Imputation with mean / median / mode ]
# It is a is a crude way of treating missing values**
if(FALSE) # for multi-line comment
{
  library(Hmisc)
  impute(Dataset$Column, mean)  # replace with mean
  impute(Dataset$Column, median)  # median
  impute(Dataset$Column, 20)  # replace specific number
  
  # Imputing manually
  Dataset$Column[is.na(Dataset$Column)] <- 
                mean(Dataset$Column, na.rm = T)  # not run

# Computing the accuracy when it is imputed with mean
  library(DMwR)
  actuals <- original_dataset$Column[is.na(Dataset$Column)]
  predicteds <- rep(mean(Dataset$Column, na.rm=T), length(actuals))
  
  regr.eval(actuals, predicteds)
}
# Check for mean absolute percentage error (mape) improvement

# METHOD-3 [ PREDICTION ]
# 3.1 [ Knn Imputaion ]
if(FALSE)
{
  library(DMwR)
  knnOutput <- knnImputation(Dataset[, !names(Dataset) %in% "medv"])  
  # Perform knn imputation.
  anyNA(knnOutput) # Checking

# Compute the accuracy.
  actuals <- original$Col_numeric[is.na(Dataset$Column_numeric)]
  predicteds <- knnOutput[is.na(Dataset$Column_numeric), "Col_numeric"]
  regr.eval(actuals, predicteds)
}

# 3.2 [ rpart --> "Recursive Partitioning and Regression Trees" ]

# Knn  may not be appropriate to use when the missing value 
# comes from a factor variable

# In  rpart
# For factor variable ----> method=class 
# For numerics ----> method=anova

if(FALSE)
{ 
  library(rpart)
  # since Col_factor is a factor
  class_mod <- rpart(Col_factor ~ . - medv, data=Dataset[!is.na(Dataset$Col_factor), ],
                     method="class", na.action=na.omit)  
  # since Col_numeric is numeric.
  anova_mod <- rpart(Col_numeric ~ . - medv, data=Dataset[!is.na(Dataset$Column_numeric), ],
                     method="anova", na.action=na.omit)  
  
  # Accuracy Measurement
  Col_factor <- predict(class_mod, Dataset[is.na(Dataset$Col_factor), ])
  Col_numeric_pred <- predict(anova_mod, Dataset[is.na(Dataset$Column_numeric), ])
  
  actuals <- original$Col_numeric[is.na(Dataset$Column_numeric)]
  predicteds <- Col_numeric_pred
  regr.eval(actuals, predicteds)
}

# 3.3 [ mice--> "Multivariate Imputation by Chained Equations" ]
#  Provides advanced features for missing value treatment. 
if(FALSE)
{
  library(mice)
  miceMod <- mice(Dataset[, !names(Dataset) %in% "medv"], method="rf")  # perform mice imputation, based on random forests.
  miceOutput <- complete(miceMod)  # generate the completed data.
  anyNA(miceOutput)
  #> FALSE
  
  # Compute the accuracy of Col_numeric.
  
  actuals <- original$Col_numeric[is.na(Dataset$Col_numeric)]
  predicteds <- miceOutput[is.na(Dataset$Col_numeric), "Col_numeric"]
  regr.eval(actuals, predicteds)
  
  # Compute the accuracy of Col_factor
  
  actuals <- original$Col_factor[is.na(Dataset$Col_factor)]
  predicteds <- miceOutput[is.na(Dataset$Col_factor), "Col_factor"]
  mean(actuals != predicteds)
}

#**************************************************

# Analysis before outliers removal

#FITTING
lm.fit = lm(Auto$mpg~. ,data = dataset)
summary(lm.fit)

# PLOTTING ANALYSIS
par(mfrow = c(2,2))
plot(lm.fit, pch =20 ,col ='firebrick')

#**************************************************

# ***OUTLINER REMOVAL***
#BOSTON>LSTAT
quantiles = quantile(dataset$Boston.lstat,probs = c(.25,.75))
range = 1.5*IQR(dataset$Boston.lstat)
n_dataset = subset(dataset,dataset$Boston.lstat > (quantiles[1] - range) & 
                  dataset$Boston.lstat < (quantiles[2] + range))
#Outliers replacement
#dataset$Boston.lstat <- squish(dataset$Boston.lstat, 
#                 quantile(dataset$Boston.lstat, c(.05, .95)))

#BOSTON>MEDV
quantiles = quantile(n_dataset$Boston.medv,probs = c(.25,.75))
range = 1.5*IQR(n_dataset$Boston.medv)
m_dataset = subset(n_dataset,n_dataset$Boston.medv > (quantiles[1] - range) & 
                     n_dataset$Boston.medv < (quantiles[2] + range))

# Analysing via plotting #2
scatter.smooth(x =dataset$Boston.lstat, y = dataset$Boston.medv ,
                  col ='steel blue' ,pch = 20)
length(m_dataset$Boston.lstat)
boxplot(dataset$Boston.lstat,horizontal = TRUE ,xlab = 'lstat')
boxplot(m_dataset$Boston.medv,horizontal = TRUE,xlab ="medv")

#**************************************************

# Split Dataset [ Method-1 ]
library(caTools)
set.seed(123)
split = sample.split(m_dataset$Boston.medv, SplitRatio = 0.7)
training_set = subset(m_dataset , split==TRUE )
test_set = subset(m_dataset , split==FALSE )

# [ Method-2 ]
#set.seed(101) 
# Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
#sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)),
#                                         replace = F)
#train <- data[sample, ]
#test  <- data[-sample, ]

#**************************************************

# *** Building MODEL ***
#Fitting
lm.fit = lm(Boston.medv~Boston.lstat ,data = training_set) ; print(lm.fit)
scatter.smooth(x =training_set$Boston.lstat, y = training_set$Boston.medv,
                  col ='steel blue' ,pch = 20)
#Linear Regression Analysis
summary(lm.fit)

# Statistics________________________________Criteriom
#p-Value_________________________________more the stars beside the variable's
#                                         p-Value, the more significant
#Std. Error______________________________Closer to zero the better
#R-Squared, Adj R-Squared _______________Higher the better (> 0.70)
#F-Statistic_____________________________Higher the better
#AIC, BIC________________________________Lower the better
#Mallows cp______________________________Should be close to the number
#                                         of predictors in model

#*******************************************************

#PREDICTING
pred =predict(lm.fit ,newdata = test_set)
scatter.smooth(x =test_set$Boston.lstat, y = test_set$Boston.medv,
               col ='magenta' ,pch = 20)
summary(pred)

#*******************************************************

# PREDICTION ACCURACY
actual_preds = data.frame(cbind(actuals =test_set$Boston.medv , predicteds =pred))

# Statistics_________________________________________Criterion

#Corellation_________________________________________closer to extreme the better
#                                                       collrelation [-1,1]
#MAPE (Mean absolute percentage error)_______________Lower the better
#MSE (Mean squared error)____________________________Lower the better
#Min_Max Accuracy => mean(min(actual, predicted)
#                     /max(actual, predicted))_______Higher the better
# 1
correlation_accuracy = cor(actual_preds)
# 2
mape = mean(abs(actual_preds$predicteds - actual_preds$actuals)/actual_preds$actuals)
# 3
min_max_accuracy = mean(apply(actual_preds, 1, min) / apply(actual_preds, 1, max))

#RESIDUAL ANALYSIS
plot(predict(lm.fit),residuals(lm.fit),col = 'dark green',pch =20)
abline(0,0,lty = 3)#line type(lty) ,line width(lwd)
plot(hatvalues(lm.fit),col = 'orange3',pch =20)
which.max(hatvalues(lm.fit))

#**************************************************************
#**************************************************************

# when we donot want the intercept 
# --> lm.fit( y ~ x + 0 )



