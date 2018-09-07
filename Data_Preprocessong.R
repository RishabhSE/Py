# ********************************************************
# suppressPackageStartupMessages(library(PACKAGE))
# To supress package messages

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(ggcorrplot))



# *********************************************************
# *********************************************************

# *** DATA EXPLORATION *** 

# names(Dataset) [names of the variables]
# dim(Dataset) [dimensions of dataset]
# fix(Dataset) [editing the dataset]

# ?Dataset [inbuild dataset]
# str(Dataset) [STRUCTURE of object]
# glimpse(Dataset) [ "DPLYR Pakage" Get a glimpse of your data.(better than 'str()')]
# attach(Dataset)

# sum( is.na( Dataset$Column )) [count no. of missing values]
# Dataset = na.omit(Dataset)

#**********************************************************
# *********************************************************

#*** GRAPHICAL ANALYSIS ***

# Visualize the linear relationship between the predictor and response
# METHOD-1 [ BASE ]
scatter.smooth(x =Boston$lstat, y = Boston$medv ,col ='steel blue' 
               ,pch = 20)
# METHOD-2 [ GGPLOT2 ]
gs <- ggplot( Boston ,aes(x = Boston$lstat ,y = Boston$medv)) # SETUP
gl <- gs + geom_point() + geom_smooth() # Adding Scatterplot & Smoothing geoms
glb <- gl + labs(title ='Lstat vs Medv', xlab ='LSTAT', ylab = 'MEDV' ) # Labeling


# To spot any outlier observations
# METHOD-1 [ BASE ]
boxplot(x =Boston$lstat, y = Boston$medv ,col ='steel blue' ,pch = 20)

# To see the distribution of the predictor variable
# METHOD-1 [ BASE ]
plot(density(x =Boston$lstat, y = Boston$medv))
polygon(density(Boston$lstat), col="dark green")

# Correlation [ find strength of relationship ]
# METHOD-1 [ BASE ]
cor(x =Boston$lstat, y = Boston$medv)
corrplot(cor(x =Boston$lstat, y = Boston$medv) 
         ,method = 'number') #plotting

# METHOD-2 [ GGPLOT2 ]
ggcorrplot(cor(Boston),method = 'circle',hc.order = T,
           type ='lower',colors = c('red','azure','cadetblue1')) #plotting


# If missing values [ use = complete.obs]

#  Understand distribution across categories[ frequency table ]
attach(iris)
table(iris$Species)
# 2-Way Cross Tabulation
library(gmodels)
CrossTable(mydata$myrowvar, mydata$mycolvar)



# *********************************************************
# *********************************************************

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
  Col_factor_pred <- predict(class_mod, Dataset[is.na(Dataset$Col_factor), ])
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


# ********************************************************
# *********************************************************

# ***OUTLIER REMOVAL***
#BOSTON>LSTAT

quantiles = quantile(dataset$Boston.lstat,probs = c(.25,.75))
range = 1.5*IQR(dataset$Boston.lstat)
n_dataset = subset(dataset,dataset$Boston.lstat > (quantiles[1] - range) & 
                     dataset$Boston.lstat < (quantiles[2] + range))
#Outliers replacement
#dataset$Boston.lstat <- squish(dataset$Boston.lstat, 
#                 quantile(dataset$Boston.lstat, c(.05, .95)))


# *********************************************************
# *********************************************************

# *** SPLIT DATASET ***

# [ Method-1 ]
if(F)
{
  library(caTools)
  set.seed(123)
  split = sample.split(m_dataset$Boston.medv, SplitRatio = 0.7)
  training_set = subset(m_dataset , split==TRUE )
  test_set = subset(m_dataset , split==FALSE ) 
}

# [ Method-2 ]
if(F)
{
  set.seed(101) 
  #Set Seed so that same sample can be reproduced in future also
  #Now Selecting 75% of data as sample from total 'n' rows of the data  
  sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)),
                                           replace = F)
  train <- data[sample, ]
  test  <- data[-sample, ]
}

# [ METHOD-3 ] indices method
if(F)
{
  set.seed(1)
  train=sample( 1:nrow(x) ,size = nrow(x)/2)# indices for train
  test =( -train )
  y.test = y[test]# indices for test
}

# *********************************************************
# *********************************************************
