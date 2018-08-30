# *********************************************************
library("parallel")
library("foreach")
library("doParallel")

# Create your computing cluster
# "detectCores" fn. will return the number of cores in your computer
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

data = foreach(i = 2:131262, .packages = 
                 c("ncdf4","chron","stats"),
               .combine = rbind) %dopar% {
                 try({
                   df_rating_exp = filter(df_rating, movieid == i)
                   rating_avg = sum(df_rating_exp$rating)/nrow(df_rating_exp)
                   movieid = i
                   df_rating_merge = data.frame(movieid , rating_avg)
                   df_new = rbind(df_new ,df_rating_merge)
                 })
               }

# Once you're done, clean up with:
stopCluster(cl)
# *********************************************************
library(dplyr)

data_set = data.frame(rating$movieId,rating$rating)

df_rating = arrange(data_set,data_set$rating.movieId)
colnames(df_rating) = c("movieid" , "rating")

df_rating_exp = filter(df_rating, movieid == 1)
rating_avg = sum(df_rating_exp$rating)/nrow(df_rating_exp)
movieid = 1
df_new = data.frame(movieid , rating_avg)

for (i in 2:500) 
{
  df_rating_exp = filter(df_rating, movieid == i)
  rating_avg = sum(df_rating_exp$rating)/nrow(df_rating_exp)
  movieid = i
  df_rating_merge = data.frame(movieid , rating_avg)
  df_new = rbind(df_new ,df_rating_merge)
}

