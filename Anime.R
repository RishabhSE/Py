#*****************************************************************
#*****************************************************************
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(dplyr))

dataset = data.frame(anime[c(-2)])

# sum( is.na( dataset)) [count no. of missing values]
# dataset = na.omit(dataset)
# glimpse(dataset)
# summary(dataset)

#*****************************************************************
#*****************************************************************

#*** GRAPHICAL ANALYSIS ***
# Visualize the relationship between the Episodes and Rating
gs <- ggplot( dataset ,aes(y = log(dataset$episodes) ,x = dataset$rating,color =rating)) # SETUP
gl <- gs + geom_count()+geom_smooth() # Adding Scatterplot
glb <- gl + labs(title ='ANIME ', y='LOGARITHM OF EPISODES', x='RATING');glb # Labeling



# Correlation
ggcorrplot(cor(dataset[c(-2,-3)]),method = 'circle',hc.order = T,
           type ='lower',colors = c('darkred','grey','cyan'),title = "CORRELATION MATRIX") #plotting


#*****************************************************************
#*****************************************************************

# *****ANIME TYPE  ANALYSIS*****

# Visualize the relationship between the Type and Episode

gs <- ggplot( dataset ,aes(x = dataset$type ,y = log(dataset$episodes),color =type)) # SETUP
gl <- gs + geom_bar(stat = "identity") # Adding Barplot
glb <- gl + labs(title ='ANIME ', x='TYPE', y='LOGARITHM OF EPISODES');glb # Labeling

#*****************************************************************
# Ratings vary by Anime type

# To see the distribution of the variable
dataset %>% filter(!is.na(rating)) %>% 
  ggplot(aes(rating, group = type)) +geom_density(aes(fill = type), alpha = .4) +xlim(0, 10)

#*****************************************************************
# Anime Distribution by TYPE(count)

# ONE HOT ENCODING ***
# Creating a new dataset for types
dataset_type = (dataset[1])

# METHOD-1 [ BASE ]
if(FALSE)
{
  # HOT-Encoding
  for(unique_value in unique(dataset$type))
  {
    dataset_type[paste("type", unique_value, sep = ".")] <- 
      ifelse(dataset$type == unique_value, 1, 0)
  }
  # Counting
  Type_of_anime=data.frame(c(sum(dataset_type$type.Movie == 1),
                             sum(dataset_type$type.TV == 1),
                             sum(dataset_type$type.OVA == 1),
                             sum(dataset_type$type.Special == 1),
                             sum(dataset_type$type.Music == 1),
                             sum(dataset_type$type.ONA == 1)))
  colnames(Type_of_anime) = "Count"
  
  names = data.frame(c("Movie","TV","OVA","Special","Music","ONA"))
  colnames(names) = "Type"
  Type_of_anime = cbind(names,Type_of_anime)
}
# METHOD-2 [ USING DPLYR ]
Type_of_anime_1= data.frame(dataset %>%group_by(type)%>%summarise(Count = n()))

# METHOD-3 [ USING table() ]
Type_of_anime_2 = as.data.frame(table(dataset$type))
colnames(Type_of_anime_2) =c("type",'Count')

# PLOTTING
plot_ly(data = Type_of_anime, labels = ~Type_of_anime$type,
        values = ~Type_of_anime$Count,type = 'pie')%>%
  layout(title = 'Anime Distribution by TYPE')
#*****************************************************************
# Anime(Type) Distribution by MEMBERS

# METHOD[ DPLYR ]
Type_of_anime_2 = dataset %>% group_by(type) %>% summarise( count_mem = sum(members))

Type_of_anime  = cbind(Type_of_anime,Type_of_anime_2$count_mem)
colnames(Type_of_anime) = c("Type","Count_type","Count_members")

# PLOTTING
plot_ly(data = Type_of_anime, labels = ~Type_of_anime$Type,
        values = ~Type_of_anime$Count_members,type = 'pie')%>%
  layout(title = 'Anime Distribution by MEMBERS')

#*****************************************************************
#*****************************************************************

# *****GENRE OF ANIME ANALYSIS*****

# Similar Anime By Genre
genre = as.data.frame(dataset$genre ,stringsAsFactors = F)

# Counting Unique genres[ 83 Unique genre in the data set ] 
unique_genre = (unique(unlist((as.data.frame
                    (tstrsplit(x = genre[,1],",",type.convert = T))))))
# Counting Total No. of Genres
genre_count = as.matrix(unlist(strsplit(x = genre[,1],","))
                                   ,colnames = "sep")
# Counting Table
gc_table = table(genre_count)
# Counting Datafame
gc_df = as.data.frame(gc_table)

# PLOTTING
plot_ly(data = gc_df,x= gc_df$genre_count ,y=gc_df$Freq,
        name = "GENRE CHART",type = "bar")%>%
      layout(title = "GENRE CHART",yaxis = list(title = 'FREQUENCY'), 
             xaxis = list(title = "GENRE"))

#*****************************************************************

# One Hot-ENCODING
genre_2 = as.data.frame(tstrsplit(genre[,1], '[,]',type.convert=TRUE),
                        stringsAsFactors=FALSE)

if(FALSE)
{
  genre_matrix <- matrix(0,11830,83)#empty matrix
  # genre_matrix[1,] <- unique_genre #set first row to genre list
  colnames(genre_matrix) <- unique_genre #set column names to genre list
  genre_3 = as.data.frame(genre_2,stringAsFactor = FALSE)
  
  # ITERATION
  for (i in 1:nrow(genre_2)) 
  {
    for (j in 1:ncol(genre_2)) 
    {
      for( k in 1:ncol(genre_matrix))
      {
        genre_matrix[i,k]=ifelse(genre_2[i,j] 
                                 == colnames(genre_matrix)[k],1, 0)
      }
    }
  }
}

genre_exp = as.data.frame(cbind(dataset$anime_id,genre_2))

genre_exp %>% 









#*****************************************************************
#*****************************************************************

# *** Movies Having sequence ***
movie_with_seq <- dataset %>% filter(episodes > 1 ,type == "Movie") %>% summarise( count = n())

# *** Movies Having No Sequence ***
movie_with_seq <- dataset %>% filter(episodes == 1 ,type == "Movie") %>% summarise( count = n())















