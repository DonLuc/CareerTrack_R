install.packages("rtweet")
library(rtweet)

# plotting and pipes - tidyverse!
install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")

#Text-mining library
install.packages("tidytext")
library("tidytext")

# Plotting packages
install.packages("igraph")
library("igraph")
install.packages("ggraph")
library("ggraph")
climate_tweets <- search_tweets(q = "@Iam_lucTheDon", n = 1000, lang = "en", include_rts = FALSE)


# w_tweets
# Check data to see if there are emojis
head(climate_tweets$text)


# remove http elements manually ==========================================================
# error - can't subset columns that doesn't exit
climate_tweets %>% mutate_at(c("stripped_text"), gsub("http.*","",.))


climate_tweets$stripped_text <- gsub("http.*","",  climate_tweets$text)
climate_tweets$stripped_text <- gsub("https.*","", climate_tweets$stripped_text)
head(climate_tweets$stripped_text)
# ========================================================================================


# remove punctuation, convert to lowercase, add id for each tweet! =======================
climate_tweets_clean <- climate_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)
climate_tweets_clean
# ========================================================================================

# plot the top 15 words -- notice any issues?
climate_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")


# load list of stop words - from the tidytext package
data("stop_words")
head(stop_words)

nrow(climate_tweets_clean)

# remove stop words from your list of words
cleaned_tweet_words <- climate_tweets_clean %>%
  anti_join(stop_words)

# plot the top 15 words -- notice any issues?
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")



