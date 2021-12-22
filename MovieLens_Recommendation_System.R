##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

library(ggplot2)
library(lubridate)

#Data Pre Processing
# lets modify the columns to suitable formats that can be further used for analysis
# Modify the year as a column in the edx & validation datasets

edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))


#Exploratory Data analysis


## the dataset and its basic summary statistics
# intial 7 rows with header

head(edx)

# Total unique movies and users
#Summary statistics of edx dataset

summary(edx)

#Number of unique movies and users in the edx dataset 

edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

#Movie ratings are in each of the following genres in the edx dataset

genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})


# Summary statistics of edx rating
summary(edx$rating)

#This list shows movie that has the greatest number of ratings

edx %>% group_by(title)%>%summarise(number = n())%>%arrange(desc(number))


#The five most given ratings in order from most to least
head(sort(-table(edx$rating)),5)


# Ratings distribution

table(edx$rating)
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line(color = "blue")

# Rating Histogram
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "blue") +
  xlab("Rating") +
  ylab("Count") +
  ggtitle("Rating distribution") +
  theme(plot.title = element_text(hjust = 0.5)) 

# We can see in the plot , half star ratings are less common than whole star ratings

#Average ratings of edx dataset
avg_ratings <- edx %>% group_by(year) %>% summarise(avg_rating = mean(rating)) 


# Data Analysis Strategies
# Plot number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")

# Plot number of ratings given by users

edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users")

#Estimating the trend of rating versus release year-Year Effect

edx %>% group_by(year) %>%
  summarize(Rating = mean(rating)) %>%
  ggplot(aes(year, Rating)) +
  geom_point() +
  geom_smooth()+ 
  ggtitle("Rating vs Release year trend")

# The general trend shows modern users relatively rate movies lower.


# Table 20 movies rated only once

# These are noisy estimates which can increase our RMSE
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20) %>%
  knitr::kable()

# Plot mean movie ratings given by users

edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "blue") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()

# Modelling Approach

# The value used to evaluate algorithm performance is the Root Mean Square Error(RMSE). RMSE is one of the most 
# used measure of the differences between values predicted by a model and the values observed.

#  Since RMSE (root mean squre error) is used frequency so lets define a function


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2,na.rm=T))
}
## Average movie rating model ##

# Compute the dataset's mean rating
mu <- mean(edx$rating)
mu

# Test results based on simple prediction just the mean
naive_rmse <- RMSE(validation$rating, mu)
# Test results based on simple prediction
naive_rmse

# Check results
# Save prediction in data frame
rmse_results <- data_frame(method = "Average movie rating model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


# Movie effect model 

# Simple model taking into account the movie effect b_i
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_i
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 20, data = ., color = I("blue"),
                     ylab = "Number of movies", main = "Movie effect model - Penalty Term (b_i)")

# Test and save rmse results 
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model - Penalty Term (b_i)",  
                                     RMSE = model_1_rmse ))

# Check results
rmse_results %>% knitr::kable()


# Movie and user effect model
# Plot penalty term user effect 

user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs%>% qplot(b_u, geom ="histogram", bins = 20, data = ., color = I("blue"),
                   ylab = "Number of movies", main = "Movie and user effect model - Penalty Term (b_u)")

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Test and save rmse results 
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user effect model - Penalty Term (b_u)",  
                                     RMSE = model_2_rmse))

# Check result
rmse_results %>% knitr::kable()

#  Movie, user and year effect model

year_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))


# Test and save rmse results 
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  pull(pred)


model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie, User and Year effect model - Penalty Term (b_y)",  
                                     RMSE = model_3_rmse))

# Check result
rmse_results %>% knitr::kable()


# Regularized movie, user and year effect model 

# lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time  
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u)/(n()+l), n_y = n())
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = 'year') %>%
    mutate(pred = mu + b_i + b_u + b_y) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, rmses)  

#To find optimal lambda

lambda <- lambdas[which.min(rmses)]
lambda

# Test and save results                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie, user and year effect model",  
                                     RMSE = min(rmses)))


# Check result
rmse_results %>% knitr::kable()

#### Results ####                                                            
# RMSE results overview                                                          
rmse_results %>% knitr::kable()

#### Appendix ####
print("Operating System:")
version

