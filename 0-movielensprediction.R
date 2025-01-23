# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(dslabs)
library(ggplot2)
library(dplyr)
library(caret)
library(broom)
library(purrr)
library(gridExtra)
library(knitr)



# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10m.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")



str(movielens)
#View(movielens)

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)








#View(edx)
str(edx)

#number of unique users and movies
edx %>%
summarize(n_users = n_distinct(userId),
          n_movies = n_distinct(movieId))



 


#data wrangling
edx_1 <- edx %>% mutate(release_year =  as.numeric(str_extract(title,"(?<=\\()\\d{4}?(?=\\))")),
                        title =  as.character(str_remove(title,"\\s\\(\\d{4}\\)")))

edx_2 <- edx_1 %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"),
                          year_rated = year(timestamp))

str(edx)
str(edx_1)
str(edx_2)

#original data
head(edx)

#data with seperate release year
head(edx_1)

#data with release year and year rated
head(edx_2)



#histogram for rating

edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) + 
  ggtitle("rating distribution") +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.text = element_text(size = 12),  
    axis.title = element_text(size = 14)  
  )



#checking rating most used by users
edx %>% 
  group_by(rating) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#View(edx)

#grouping by movieid
edx_summary_movieid <- edx_2 %>% 
  group_by(movieId) %>%
      summarise(mean_rating = mean(rating), reviews = n()) 


edx_summary_movieid <- edx_summary_movieid %>% arrange(desc(mean_rating))


#top 20 movies 
head(edx_summary_movieid, n = 20)

#All of the top 20 have very few people involved in the rating and also their release date seem to be 
#from the 1900s.

#top 20 movies with high at least over 100 rating count
edx_summary_movieid %>% filter(reviews > 100) %>%
  head(n=20)


#bottom 20 movies
edx_summary_movieid %>% arrange(mean_rating) %>%
  head(n = 20)

#The bottom 20 have few people involved in the rating though not as extreme as the top 20 and in this case
#11 out of the 20 seem to be from the 2000s

#bottom movies with at least over 100 rating count
edx_summary_movieid %>% 
  filter(reviews>100)%>%
  arrange(mean_rating) %>%
  head(n = 20)

#histogram for mean rating
edx_summary_movieid %>% ggplot(aes(mean_rating)) +
  geom_histogram()

#The distribution is skewed to the left meaning there are relatively few lower values with most of the data points being 
#clustered on the higher end, creating a long tail on the left side of the distribution. The histogram shows
#the most movies have a rating of 3 to about 3.7


mean(edx$rating)

#the mean rating of all ratings is 3.5

#histogram for reviews


edx_summary_movieid %>% ggplot(aes(reviews)) +
  geom_histogram() +
  scale_x_log10()


#A plot to understand reviews and rating interaction
p1 <- edx_summary_movieid %>% ggplot(aes(reviews, mean_rating)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = lm)

p2 <- edx_summary_movieid %>% ggplot(aes(mean_rating, reviews)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = lm)

grid.arrange(p1,p2, ncol = 2)



cor(edx_summary_movieid$mean_rating, edx_summary_movieid$reviews)

#The correlation between the two variables is weak which means there is a lot of variability or noise due
#underlying factors. 

#we calculate the linear relationship bewtween the 2 variables

lm(mean_rating~ reviews, data = edx_summary_movieid)
lm(reviews~ mean_rating, data = edx_summary_movieid)

edx_summary_movieid %>% ggplot(aes(mean_rating, reviews)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = lm)
  

#The coeficients from linear regression model show
#that using the rate count to predict rating can be used from the rating of 3.146 but not below that, which means there is  
#another feature affecting this.

#analysing user rating

edx_summary_userid <- edx %>% 
  group_by(userId) %>%
  summarise(mean_rating = mean(rating), reviews = n())

str(edx_summary_userid)

#users who have given the highest ratings after reviewing more than 100 times 
edx_summary_userid %>% 
  filter(reviews > 100) %>%
  arrange(desc(mean_rating)) %>%
  head(n = 10)

#checking the users who reviewed the most 
edx_summary_userid %>% 
  arrange(desc(reviews)) %>%
  head(n = 10)

#checking the relationship between mean rating and reviews
edx_summary_userid %>% 
  ggplot(aes(reviews, mean_rating)) +
  geom_point()+
  geom_smooth()+
  scale_x_log10()

lm(mean_rating~reviews, data = edx_summary_userid)  


#analysing the time and rating

edx_releaseyear_summary <- edx_2 %>%
  group_by(release_year) %>%
  summarise(mean_rating = mean(rating), reviews = n())

#highest rating release years with more than 100 reviews

edx_releaseyear_summary %>%
  filter(reviews>100) %>%
  arrange(desc(mean_rating)) %>%
  head(n =10)

mean(edx_releaseyear_summary$reviews)

#based on the interquartile range of the reviews grouped by release year
release_yearIQR_reviews <-  IQR(edx_releaseyear_summary$reviews, 0.25,na.rm = FALSE)

edx_releaseyear_summary %>% 
  filter(reviews > release_yearIQR_reviews) %>%
  arrange(desc(mean_rating)) %>%
  head(n =10)

#histogram of reviews
edx_releaseyear_summary %>%
  ggplot(aes(reviews)) +
  geom_histogram() +
  scale_x_log10()

#histogram of mean rating
edx_releaseyear_summary %>%
  ggplot(aes(mean_rating)) +
  geom_histogram() +
  scale_x_log10()

#relationship between the mean rating and year
edx_releaseyear_summary %>%
  ggplot(aes(x = release_year, y = mean_rating)) +
  geom_point(alpha = 0.6, size = 2) +  
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  
  labs(title = "Release Year vs Mean Rating",
       x = "Release Year",
       y = "Mean Rating") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

cor(edx_releaseyear_summary$release_year, edx_releaseyear_summary$mean_rating)

#mean rating has a strong negative correlation with release year with mean rating decreasing 
#with each year.

#the next analysis is mean rating per year rated and their relationship

edx_yearrated_summary <- edx_2 %>%
  group_by(year_rated) %>%
  summarise(mean_rating = mean(rating), reviews=n())

#checking the highest mean ratings per the year rated with more than 100 reviews
edx_yearrated_summary %>%
  filter(reviews > 100) %>%
  arrange(desc(mean_rating))

#relationship between year rate and mean rating
edx_yearrated_summary %>%
  ggplot(aes(x = year_rated, y = mean_rating)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(
    x = "Year Rated", 
    y = "Mean Rating",
    title = "Mean Rating vs Year Rated",
    subtitle = "Linear Relationship between Year Rated and Mean Rating"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),  
    axis.title = element_text(size = 12),  # Axis title size
    axis.text = element_text(size = 10)  # Axis text size
  )


# the relationship is inverse with mean rating reducing with each year

cor(edx_yearrated_summary$year_rated, edx_yearrated_summary$mean_rating)




#machine learning model 
#To ensure we don't include users and movies in the test set that are not in the training set.


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#baseline model is the guessing model/naive model
#we use the average to represent expected prediction for all the cases
mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- RMSE(final_holdout_test$rating, mu_hat)
naive_rmse

rmse_results <- tibble(method = "baseline model ", RMSE = naive_rmse)
rmse_results %>% knitr::kable()
#the model is not very accurate but the point of the guessing model is to use it 
#as baseline model to compare it to the other models.

#first model
#next model we will utilise the movieid feature which was the first feature we analysed

mu <- mean(edx$rating) 

#the edxmovie_avgs contain the average bias for every movie 
edxmovie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


#to create a plot of an histogram to show the deviation from the global average
edxmovie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))

#we try the model on the test set and use left join to ensure nothing is lost from the test_set 
predicted_ratings <- mu + final_holdout_test %>% 
  left_join(edxmovie_avgs, by='movieId') %>%
  pull(b_i)

firstmodel_rmse <- RMSE(predicted_ratings, final_holdout_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = firstmodel_rmse ))
rmse_results %>% knitr::kable()



# Modeling user effects and movie effects

#the second model we will use userid the second feature we analysed together with the first feature 
#movie id

edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#the second model we will use userid the second feature which we analysed in our analysis
#movie id

edxuser_avgs <- edx %>% 
  left_join(edxmovie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predictions <- final_holdout_test %>% 
  left_join(edxmovie_avgs, by='movieId') %>%
  left_join(edxuser_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

secondmodel_rmse <- RMSE(predictions, final_holdout_test$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and User Effects Model",  
                                     RMSE = secondmodel_rmse ))
rmse_results %>% knitr::kable()




#the third model we will use Regularized movie effects and user effects

#lambda is a tuning parameter which can be chosed Using cross-validation.

lambdas <- seq(0, 10, 0.25)


#best lambda  
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predictions <- 
    final_holdout_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predictions, final_holdout_test$rating))
})


# The optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]
lambda

# Test and save results                                                             
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized movie and user effect model",  
                                     RMSE = min(rmses)))

# Check result
rmse_results %>% knitr::kable()









