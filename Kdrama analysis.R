library(readr)
library(dplyr)

df <- read_csv("kdrama_dataset.csv")

# View structure
str(df)

# Summary statistics
summary(df)

# Number of unique genres, tags, etc.
df %>% summarise(
  num_dramas = n(),
  unique_genres = n_distinct(Genre),
  unique_tags = n_distinct(Tags),
  avg_rating = mean(Rating, na.rm = TRUE),
  avg_episodes = mean(`Number of Episodes`, na.rm = TRUE)
)






#Average K-Drama Rating by Year
library(ggplot2)
library(dplyr)

# Ensure the data is clean
df$`Year of release` <- as.numeric(df$`Year of release`)
df$Rating <- as.numeric(df$Rating)

# Group by year and calculate the average rating
avg_rating_per_year <- df %>%
  filter(!is.na(Rating), !is.na(`Year of release`)) %>%
  group_by(`Year of release`) %>%
  summarise(avg_rating = mean(Rating, na.rm = TRUE)) %>%
  arrange(`Year of release`)

# Line plot
ggplot(avg_rating_per_year, aes(x = `Year of release`, y = avg_rating)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "black") +
  scale_x_continuous(breaks = seq(min(avg_rating_per_year$`Year of release`),
                                  max(avg_rating_per_year$`Year of release`), 1)) +
  labs(
    title = "Average K-Drama Rating by Year",
    x = "Year of Release",
    y = "Average Rating"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






# Number of dramas per year
# Convert the 'Year of release' column to numeric in case it's not already
df$`Year of release` <- as.numeric(df$`Year of release`)

ggplot(df, aes(x = `Year of release`)) +
  # Create a bar chart of number of dramas released each year
  geom_bar(fill = "coral") +
  
  # Explicitly tell ggplot to show every year on the x-axis using a sequence of 1-year intervals
  scale_x_continuous(
    breaks = seq(
      min(df$`Year of release`, na.rm = TRUE),  # start at the earliest year
      max(df$`Year of release`, na.rm = TRUE),  # end at the latest year
      1                                          # show every year (step = 1)
    )
  ) +
  
  # Show consistent steps on the y-axis (e.g., 0, 5, 10, 15, ...)
  scale_y_continuous(
    breaks = seq(0, max(table(df$`Year of release`)), 2)  # Adjust 5 to whatever spacing looks best
  ) +
  
  
  # Add titles and axis labels
  labs(
    title = "Number of K-Dramas Released Per Year",  # chart title
    x = "Year of Release",                           # x-axis label
    y = "Number of Dramas"                           # y-axis label
  ) +
  
  # Use a clean, minimal theme
  theme_minimal() +
  
  # Rotate x-axis labels to avoid overlap
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#View top 10 dramas with the highest sentiment score
# Load necessary libraries
library(tidytext)
library(textdata)
library(tidyr)
library(dplyr) # Just in case it's not loaded for the pipe (%>%) and arrange

# Check available sentiment lexicon
get_sentiments("bing")

df <- read_csv("kdrama_dataset.csv")


# Prepare text data for sentiment analysis
df_sentiment <- df %>%
  # Break the 'Description' text into individual words (tokens)
  unnest_tokens(word, Description) %>%
  
  # Join with the Bing sentiment lexicon to label each word as positive/negative
  inner_join(get_sentiments("bing"), by = "word") %>%
  
  # Count number of positive and negative words per drama
  count(Title, sentiment) %>%
  
  # Pivot wider so that each row has a 'positive' and 'negative' column
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  
  # Calculate sentiment score by subtracting negative count from positive
  mutate(sentiment_score = positive - negative)

# View top 10 dramas with the highest sentiment score
df_sentiment %>% arrange(desc(sentiment_score)) %>% head(10)

# View top 10 dramas with the lowest sentiment score
df_sentiment %>% arrange((sentiment_score)) %>% head(10)






#Average rating by Genre
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Calculate average rating by genre
top_genres <- df %>%
  separate_rows(Genre, sep = ",") %>%
  mutate(Genre = trimws(Genre)) %>%
  group_by(Genre) %>%
  summarise(average_rating = mean(Rating, na.rm = TRUE)) %>%
  filter(average_rating >= 8) %>%
  arrange(desc(average_rating)) %>%
  head(10)  # Select top 10 genres

# Create a line plot focusing on the top 10 genres and ratings from 8.0 to 9.0
ggplot(top_genres, aes(x = reorder(Genre, average_rating), y = average_rating, group = 1)) +
  geom_line(color = "purple", size = 1) +  # Line plot
  geom_point(color = "red", size = 2) +   # Add points to the line plot
  scale_y_continuous(limits = c(8, 9), breaks = seq(8, 9, by = 0.2)) +  # Set y-axis range from 8 to 9
  labs(title = "Top 10 Genres by Average Rating ", x = "Genre", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability




#Top 10 actors with their average ratings


library(dplyr)
library(tidyr)

# Split the actors column into individual rows and calculate their average ratings
top_actors_avg_rating <- df %>%
  filter(Rating >= 5.0) %>%
  separate_rows(Actors, sep = ",") %>%
  mutate(Actors = trimws(Actors)) %>%
  group_by(Actors) %>%
  summarise(
    Count = n(),
    Avg_Rating = round(mean(Rating, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Count), desc(Avg_Rating))

# View top 10 actors with their average ratings
head(top_actors_avg_rating, 10)

