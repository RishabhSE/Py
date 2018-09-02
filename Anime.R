library(corrplot)
dataset = data.frame(anime[c(-2,-3,-4)])

# sum( is.na( n_dataset )) [count no. of missing values]
# dataset = na.omit(n_dataset)


#**************************************************

#*** GRAPHICAL ANALYSIS *** #1
# Visualize the linear relationship between the predictor and response
scatter.smooth(x =dataset$rating, y = dataset$members ,col ='steel blue' 
               ,pch = 20)

# To spot any outlier observations
boxplot(x =dataset$rating, y = dataset$episodes,col ='steel blue' 
        ,pch = 20,horizontal = T)

# To see the distribution of the predictor variable
plot(density(x =dataset$anime_id, y = dataset$members))
polygon(density(dataset$anime_id), col="dark green")

# Correlation
cor(x =dataset$members, y = dataset$anime_id)
corrplot(cor(dataset) ,method = 'number') #plotting

#**************************************************
# ***OUTLIER REMOVAL***

quantiles = quantile(dataset$rating,probs = c(.25,.75))
range = 1.5*IQR(dataset$rating)
n_dataset = subset(dataset,dataset$rating > (quantiles[1] - range) & 
                     dataset$rating < (quantiles[2] + range))

#**************************************************

dataset = merge(n_dataset,anime[,c(1,3)])

ls = data.frame()
ls1 = data.frame()
for( i in range(1,11650))
{
  ls1 = strsplit(dataset$genre[3],",")
  ls= merge(ls,ls1)
}



