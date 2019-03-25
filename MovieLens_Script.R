#R script that generates predicted movie ratings and calculates RMSE

# Create edx set, validation set, and submission file
#Load the necessary packages
library(tidyverse)
library(caret)
library(stringr)
libarary(dplyr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
     semi_join(edx, by = "movieId") %>%
     semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Data Exploration

#What are the dimensions of the edx dataset?
dim(edx)

#How many zeros and threes are there in the edx dataset?
edx %>% filter(rating == 3) %>% tally()

#How many different movies are in the edx dataset?
edx %>% summarize(n_movies=n_distinct(movieId))

#How many different users are in the edx dataset?
edx %>% summarize(n_users=n_distinct(userId))

#How many movie ratings are there for drama, comedy, thriller, and romance genres?
edx %>% separate_rows(genres, sep = "\\|") %>%
	group_by(genres) %>%
	summarize(count = n()) %>%
	arrange(desc(count))

#Which movie group has the highest number of ratings?
edx %>% group_by(title) %>% summarize(number = n()) %>% arrange(desc(number))

#What are the five most given ratings?
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>% arrange(desc(count)) 

#Are half-star ratings less common than whole star ratings?
table(edx$rating)
edx %>%
	group_by(rating) %>%
	summarize(count = n()) %>%
	ggplot(aes(x = rating, y = count)) +
	geom_line()

#Exploring Predictions

RMSE <- function(true_ratings, predicted_ratings){
        sqrt(mean((true_ratings - predicted_ratings)^2))}

lambda <- seq(0,5,0.5)

rmse <- sapply(lambda, function(x){

#Let's start by building a simple recommendation system that will predict the same rating for all movies, regardless of user.
#In this case, the estimate that minimizes the RSME is the average of all ratings (least squares estimate)
mu_hat <- mean(edx$rating)

#Now we will implement a Movie Effect model that takes movie variability into account
movie_avgs <- edx %>% group_by(movieId) %>% summarize(movie_avgs = mean(rating - mu_hat))

#Now we will also take account for the user effects due to variability
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% 
group_by(userId) %>% summarize(user_avgs = mean(rating - mu_hat - movie_avgs))

predicted_ratings <- edx %>% left_join(movie_avgs, by='movieId') %>%
	left_join(user_avgs, by='userId') %>%
	mutate(pred = mu_hat + movie_avgs + user_avgs) %>%
	pull(pred)

return(RMSE(predicted_ratings, edx$rating))})

lambda
plot(lambda,rmse)
#Due to the insiginificant effect of lambda on rmse, predictions will be made with lambda=0.5 for simplicity

min(rmse)

#A minimum rmse of 0.857 was achieved using the Movie & User Variability Method

#Predictions

lambda <- 0.5
      
prediction <- sapply(lambda,function(l){
  
#First, determine the mean of the training set
mu <- mean(edx$rating)
  
#Predict the movie effect with lambda
pred_movie_avgs <- edx %>% group_by(movieId) %>% summarize(pred_movie_avgs = mean(rating - mu))
  
#Predict the user effect with optimal lambda
pred_user_avgs <- edx %>% left_join(pred_movie_avgs, by='movieId') %>% 
group_by(userId) %>% summarize(pred_user_avgs = mean(rating - mu - pred_movie_avgs))

#Predict ratings on validation set
predicted_ratings <- validation %>% 
	left_join(pred_movie_avgs, by = "movieId") %>%
	left_join(pred_user_avgs, by = "userId") %>%
	mutate(pred = mu + pred_movie_avgs + pred_user_avgs) %>%
	pull(pred) 
  
  return(predicted_ratings)})

#These predicted ratings are documented on the following .csv file

write.csv(validation %>% select(userId, movieId) %>% mutate(rating = prediction),
          "movielens_prediction_submission.csv", na = "", row.names=FALSE)


















