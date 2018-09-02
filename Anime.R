#*****************************************************************

library(corrplot)
dataset = data.frame(anime[c(-2)])

# sum( is.na( dataset)) [count no. of missing values]
# dataset = na.omit(dataset)
# str(dataset)
# summary(dataset)
# 

#*****************************************************************

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
corrplot(cor(dataset,use = "complete.obs") ,method = 'number') #plotting

#*****************************************************************
# ***OUTLIER REMOVAL***

quantiles = quantile(dataset$rating,probs = c(.25,.75))
range = 1.5*IQR(dataset$rating)
n_dataset = subset(dataset,dataset$rating > (quantiles[1] - range) & 
                     dataset$rating < (quantiles[2] + range))

#*****************************************************************

# *****TYPE OF ANIME ANALYSIS*****

# ONE HOT ENCODING ***
# Creating a new dataset for types
dataset_type = (dataset[1])
# HOT-Encoding
for(unique_value in unique(dataset$type))
  {
    dataset_type[paste("type", unique_value, sep = ".")] <- 
      ifelse(dataset$type == unique_value, 1, 0)
}

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Anime Distribution by TYPE
Type_of_anime=data.frame(c(sum(dataset_type$type.Movie == 1),
                            sum(dataset_type$type.TV == 1),
                            sum(dataset_type$type.OVA == 1),
                            sum(dataset_type$type.Special == 1),
                            sum(dataset_type$type.Music == 1),
                            sum(dataset_type$type.ONA == 1)))
colnames(Type_of_anime) = "Watches"

names = data.frame(c("Movie","TV","OVA","Special","Music","ONA"))
colnames(names) = "Type"
Type_of_anime = cbind(names,Type_of_anime)

# PLOTTING
pie(x = Type_of_anime$Watches,labels = Type_of_anime$Type,col = rainbow(6),
      main = "Anime Distribution by TYPE")
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Anime Distribution by MEMBERS

library(dplyr)

mem = data.frame() # To store selected anime type "dataset"
total_mem =matrix(ncol = 1) # to store the total members

# LOOPING : To count the total no. of members of a particular type
for (i in 1:nrow(Type_of_anime)) 
{
  mem = filter(dataset,dataset$type == Type_of_anime$Type[i])
  total_mem[i] =summarise(mem,sum(mem$members))
}

# Before combining , we have to unname and unlist the "total_mem"
mem_df = unname(unlist(data.frame(total_mem)))
# Column Binding
Type_of_anime =cbind(Type_of_anime,mem_df)
colnames(Type_of_anime) = c("Type","watchers","member_total")

# PLOTTING
pie(x = Type_of_anime$member_total,labels = Type_of_anime$Type,
    col = rainbow(6),main = "Anime Distribution by MEMBERS",
    start = -pi/6)

# PLOTTING 3D
library(plotrix)
pie3D(x = Type_of_anime$member_total,labels = Type_of_anime$Type,
      col = rainbow(6),main = "Anime Distribution by MEMBERS",
      theta = pi/4,start = -pi/6,explode = 0.1)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#*****************************************************************

# *****GENRE OF ANIME ANALYSIS*****
genre = as.data.frame(dataset$genre ,stringsAsFactors = F)

# HOT-Encoding
















