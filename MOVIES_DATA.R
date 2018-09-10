# *********************************************************
library("parallel")
library("foreach")
library("doParallel")

# Create your computing cluster
# "detectCores" fn. will return the number of cores in your computer
cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl, cores = detectCores() - 2)

data = foreach(i = 1:100, .packages = 
                 c("ncdf4","chron","stats"),
               .combine = rbind) %dopar% {
                 try({
                   data_exp = filter(data_set, anime_id == i )
                   rating_median = median(data_exp$rating)
                   anime_id = i
                   df_new_n = data.frame(anime_id , rating_median)
                   df_new = rbind(df_new ,df_new_n)
                 })
               }

# Once you're done, clean up with:
stopCluster(cl)
# *********************************************************
library(dplyr)

data_set = data.frame(rating)
data_set = arrange(data_set,data_set$user_id)

# Counting user not given rating 
sum(data_set[,3] == -1 )

# Removing them
data_set = filter(data_set, rating != -1)

data_exp = filter(data_set, anime_id ==1 )
rating_median = median(data_exp$rating)
anime_id = 1
df_new = data.frame(anime_id , rating_median)

for (i in 1:34475) 
{
  data_exp = filter(data_set, anime_id == i )
  rating_median = median(data_exp$rating)
  anime_id = i
  df_new_n = data.frame(anime_id , rating_median)
  df_new = rbind(df_new ,df_new_n)
}

# sum( is.na( df_new$rating_median )) [Count no. of missing values]

for (i in 2:100) 
{
  if(is.na( is.na(df_new[4,2]) ))
  {
    data_exp = filter(data_set, anime_id == 4 )
    rating_median = mean(data_exp$rating)
    anime_id = 4
    df_new_n = data.frame(anime_id , rating_median)
    df_new = rbind(df_new ,df_new_n)
  }
}

# *********************************************************

