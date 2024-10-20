# Create edx and final_holdout_test sets

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
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

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")  # if using R 3.6 or later
# set.seed(1)  # if using R 3.5 or earlier
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

# Clean up environment
rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Explore the distribution of ratings
ratings_distribution <- edx %>%
  group_by(rating) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot the ratings distribution
ggplot(ratings_distribution, aes(x = rating, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Movie Ratings", x = "Rating", y = "Count") +
  theme_minimal()

# Explore the number of ratings per movie
movie_ratings_count <- edx %>%
  group_by(movieId, title) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Top 10 movies with the most ratings
top_10_movies <- movie_ratings_count %>%
  top_n(10, count)

# Plot the top 10 most rated movies
ggplot(top_10_movies, aes(x = reorder(title, count), y = count)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Top 10 Most Rated Movies", x = "Movie Title", y = "Number of Ratings") +
  theme_minimal()

# Explore the number of ratings per user
user_ratings_count <- edx %>%
  group_by(userId) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot the distribution of the number of ratings per user
ggplot(user_ratings_count, aes(x = count)) +
  geom_histogram(bins = 50, fill = "lightgreen") +
  labs(title = "Distribution of Number of Ratings per User", x = "Number of Ratings", y = "Count") +
  theme_minimal()

# Explore the genres
genre_count <- edx %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot the distribution of genres
ggplot(genre_count, aes(x = reorder(genres, count), y = count)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Distribution of Movie Genres", x = "Genre", y = "Count") +
  theme_minimal()
