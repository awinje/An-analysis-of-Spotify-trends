library('glmnet')
library('tidyverse')
library('skimr') 
library('dplyr')
#visualization 
library('ggplot2')
library('corrplot')

library('treemap')
library('viridisLite')
library('ggiraphExtra') 
library('factoextra') 
library('gridExtra')
# machine learning
library('rpart')
library('rpart.plot')
library('caret')
library('foreign')
library('funModeling')
library('GGally')
library('rsample')
library('randomForest') 
library('e1071')
library('targets')

# first we import our dataset
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# summary statistics
view(spotify_songs)
colnames(spotify_songs)
summary(spotify_songs)


###### WRANGLING ##### 
# check for missing values 
colSums(is.na(spotify_songs)) # show that there are 5 missing values in the dataset 

# we want to remove missing values 
spotify_songs_wrangled <- spotify_songs %>%
  filter(!is.na(track_name) & !is.na(track_artist)) ## removes na from dataset 


## checks and removes any duplicate 
spotify_songs_wrangled[duplicated(spotify_songs_wrangled$track_name) | duplicated(spotify_songs_wrangled$track_name, fromLast=TRUE),]
### which code shows that all songs have a unique value. 

# this code removes variables that we will not analyze in our project 
spotify_songs_wrangled <- spotify_songs_wrangled %>% dplyr::select(-track_id,
                                                                   -track_album_id,
                                                                   -track_album_name,
                                                                   -playlist_id, 
                                                                   -playlist_name, 
                                                                   -track_name, 
                                                                   -track_artist)
view(spotify_songs_wrangled)

colnames(spotify_songs_wrangled) # to check what variables are included 
dim(spotify_songs_wrangled) # check final number of rows and columns, here we have 
#32828 unique observations and 19 variables. 


#dummy variable for >45 = not popular , <45 = popular # 45 = mean track_popularity. 
summary(spotify_songs_wrangled$track_popularity)
spotify_songs_wrangled$track_popularity_dummy= ifelse(spotify_songs_wrangled$track_popularity > 45, 1, 0)

names(spotify_songs_wrangled)
table(spotify_songs_wrangled$track_popularity_dummy)

colnames(spotify_songs_wrangled)

#we made this dummy variable such that the observations was split into two using the median of 
#the variable track_popularity which is 45. We consider the top half of our data to be popular, 
#and the other half to be not popular. This could also be split differently, 
#for example by using more than two categories. For our analysis, it made sense to 
#split the data in two. 


######## VIZUALIZATON OF THE DATASET ####### 
# bar chart of how many songs there are in the dataset from each genre. 
spotify_songs_wrangled %>% ggplot(aes(x = fct_infreq(playlist_genre) , fill = playlist_genre))+
  geom_bar(width=0.3)+
  labs(title = 'Number of songs from each genre', x= 'Genres', y = 'Count' )+
  theme_minimal()

#pie-chart of percentage how many songs there are in the dataset from each genre. 
spotify_pie_chart <- spotify_songs_wrangled %>% 
  group_by(playlist_genre) %>% 
  summarise(Total_number_of_tracks = length(playlist_genre))

ggplot(spotify_pie_chart, aes(x="", y=Total_number_of_tracks, fill=playlist_genre)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 0.5))
#the pie chart shows the variation of genres in the dataset. The most observed genres 
#are edm and rap, where edm songs have the highest proportion of genres = 18.4 and 
#rap = 17.5. The pie-chart is more pleasing to look at, therefor only this is 
#included in our analysis. 

# variables that describes a genre
spotify_songs_wrangled_genre_description <- spotify_songs_wrangled %>% 
  group_by(Genre = playlist_genre) %>%
  summarise(Danceability = mean(danceability),
            Energy = mean(energy),
            Key = mean(key),
            Loudness = mean(loudness),
            Mode = mean(mode),
            Speechiness = mean(speechiness),
            Acousticness = mean(acousticness),
            Instrumentalness = mean(instrumentalness),
            Liveness = mean(liveness),
            Valence = mean(valence),
            Tempo = mean(tempo),
            Duration = mean(duration_ms),
            Popularity=mean(track_popularity))

view(spotify_songs_wrangled_genre_description)
# this table shows the average mean of the variables within each genre. 
# more on the results from this in the paper. 

### correlation between variables 
correlation <- select(spotify_songs_wrangled,track_popularity,
                      danceability, energy, key, loudness, mode,speechiness, 
                      acousticness, instrumentalness, liveness, valence, tempo, duration_ms, mode,
                      track_popularity_dummy)

corrplot(cor(correlation), method = "number", number.cex=0.8) #better for analysis 

corrplot(cor(correlation), type = "lower") #better looking  


#finding overall popular artists, not using our wrangled data. 
popular_artists <- spotify_songs %>% 
  group_by(Artist = track_artist) %>%
  summarise(No_of_tracks = n(),Popularity = mean(track_popularity))  %>% 
  filter(No_of_tracks > 2) %>%
  arrange(desc(Popularity)) %>%
  top_n(15, wt = Popularity) %>% 
  ggplot(aes(x = Artist, y = Popularity)) +
  geom_bar(stat = "identity") +
  coord_flip() + labs(title = "popular artists overall", x = "Artists", y = "Popularity")

ggplotlyr(popular_artists) 

##this chart shows overall most popular artis. We did not include this in our analysis,
#because we did not find it relevant. 

# popular artist. 
top_genre <- spotify_songs %>% 
  select(playlist_genre, track_artist, track_popularity) %>% 
  group_by(playlist_genre,track_artist) %>% 
  summarise(n = n()) %>% top_n(15, n)

tm <- treemap(top_genre, index = c("playlist_genre", "track_artist"), 
              vSize = "n", vColor = 'playlist_genre', palette =  viridis(6),
              title="Top 15 Track Artists within each Playlist Genre")
#this treemap was included in our analysis. The information from the treemap 
#tells us which artist are the most popular within each genre.
#Not too much information for our analysis, 

# the development of each genre within the century
### first we sort the data yearly
spotify_songs_wrangled$year <- as.numeric(substring(spotify_songs_wrangled$track_album_release_date, 1,4))

view(spotify_songs_wrangled)

#creating a coulmn for year 
spotify_songs_wrangled$track_album_release_date <- as.character(spotify_songs_wrangled$track_album_release_date, "%m/%d/%Y")
spotify_songs_wrangled$year <- substr(spotify_songs_wrangled$track_album_release_date,1,4)

#poplarity movement across years
pop_across_years <- spotify_songs_wrangled %>% 
  group_by(playlist_genre, year) %>% 
  summarise(avg = mean(track_popularity) )%>% 
  subset(year >=2000)

year_graph <- pop_across_years %>%
  ggplot(aes(x = year, y = avg, 
             group = playlist_genre, color = playlist_genre)) +
  geom_line() +   labs(title = "21st Century", x = "Year of release",
                       y = "Average popularity") + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))

year_graph


######## DECISION TREE ######## 
#supervised learning 
tree_model <- rpart(track_popularity ~ loudness+valence+energy+key+danceability+acousticness+playlist_genre,
                    data = spotify_songs_wrangled)

rpart.plot(tree_model, box.palette = "GnBu")

varImp(tree_model) # shows which variables that are important 
#this shows that the variables energy and playlist_genre is the most important variables 
#to predicting popularity. 

#first: Linear model 
spotify_lm <- lm (track_popularity_dummy ~ danceability + energy + key + loudness + mode
                   + speechiness+ acousticness + instrumentalness + liveness 
                   + valence + tempo + duration_ms, data = spotify_songs_wrangled)


summary(spotify_lm) # all variables are significantly different from zero at a 5%- level,
#except key and danceability.

fit <- lm(track_popularity_dummy ~ valence + tempo, 
          data = spotify_songs_wrangled)
ggPredict(fit) + theme_minimal()

#looks like there is a positive correlation between valence and our popularity dummy. 

## To use machine learning tools, we must first split the data into train and test  
sample_index <- sample(nrow(spotify_songs_wrangled),nrow(spotify_songs_wrangled)*0.80 )

names <- c("track_popularity_dummy","playlist_genre","danceability","energy","key","loudness","mode","speechiness",
           "acousticness","instrumentalness","liveness","valence","tempo","duration_ms")

#we will use 80% of the data for training
spotify_train <- spotify_songs_wrangled[sample_index,names]

#and use 20% of the fata for testing
spotify_test <- spotify_songs_wrangled[-sample_index,names]

#converting into factors 
spotify_train$playlist_genre <-  as.factor(spotify_train$playlist_genre)
spotify_test$playlist_genre <-  as.factor(spotify_test$playlist_genre)
spotify_train$track_popularity_dummy <-  as.factor(spotify_train$track_popularity_dummy)
spotify_test$track_popularity_dummy <-  as.factor(spotify_test$track_popularity_dummy)


#Creating a random forest model
spotify_rf <- randomForest(track_popularity_dummy~.,
                           data = spotify_train, 
                           ntree=500 ,importance=T)
spotify_rf


#Prediction  in test
spotify_rf_prediction_test <- predict(spotify_rf, newdata=spotify_test, type = "class")
table(spotify_test$track_popularity_dummy, spotify_rf_prediction_test, dnn = c("True", "Pred"))

### missclassification rate = twice as much in traning and test 
varImp(spotify_rf)

## most important variables: duration, tempo, genre, instrumentalness, loudness. 

# most important variables visualization
varImpPlot(spotify_rf)

attributes(spotify_rf)
print(spotify_rf)


## CLUSTERING
# select required song features for clustering
cluster.input <- spotify_songs_wrangled[, c('energy', 'liveness','tempo', 'speechiness', 'acousticness',
                                 'instrumentalness', 'danceability', 'duration_ms' ,'loudness','valence')]

# scale features for input to clustering
cluster.input.scaled <- scale(cluster.input[, c('energy', 'liveness', 'tempo', 'speechiness'
                                                , 'acousticness', 'instrumentalness', 'danceability'
                                                , 'duration_ms' ,'loudness', 'valence')])

# kmeans with different k values
k2 <- kmeans(cluster.input.scaled, centers = 2, nstart = 25)
k3 <- kmeans(cluster.input.scaled, centers = 3, nstart = 25)
k4 <- kmeans(cluster.input.scaled, centers = 4, nstart = 25)
k5 <- kmeans(cluster.input.scaled, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = cluster.input.scaled) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

#optimal number of clusters 
set.seed(100)
fviz_nbclust(cluster.input[1:1000,], kmeans, method = "wss")
