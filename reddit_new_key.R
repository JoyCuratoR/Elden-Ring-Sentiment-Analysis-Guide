library(RedditExtractoR)
library(readtext)
library(qdap)
library(tidyverse)
library(tidytext)
library(ggthemes)


# Step 1: collecting raw data
reddit_new <- find_thread_urls(
  subreddit = "eldenring",
  sort_by = "new",
  period = "all",
  keywords = "boss, boss fight"
)


# Step 2: Exporting and saving the data 
df <- as.data.frame(reddit_new)
write.csv(df, 'reddit_new.csv')

# Step 3: Load CSV in & Clean it

# 3.1 loading data into environment
key_elden_folder <- "D:/R_Studio/Project_Vault/OP_EldenRing_Sentiment/reddit_new_key.csv"

key_reddit_new_data <- read_csv(key_elden_folder)

glimpse(key_reddit_new_data)

key_reddit_new_data <- na.omit(key_reddit_new_data) # getting rid of all the 'NA's

# 3.2 cleaning data

# tokenize data using tidyverse
key_reddit_new_data %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)


# creating a custom stopwords lexicon for unique words
custom <- add_row(stop_words, word = "amp", lexicon = "custom") 
custom <- add_row(custom, word = "x200b", lexicon = "custom") 
custom <- add_row(custom, word = "gt", lexicon = "custom")
custom <- add_row(custom, word = "https", lexicon = "custom")
custom <- add_row(custom, word = "It", lexicon = "custom")
custom <- add_row(custom, word = "lt", lexicon = "custom")

# re-writing cleaning code to include custom variable
key_elden_cleaned <- key_reddit_new_data %>%
  unnest_tokens(word, text) %>%
  anti_join(custom) %>%
  count(word, sort = TRUE)

# cleaning  numbers
key_elden_new <- key_elden_cleaned %>%
  filter(! str_detect(key_elden_cleaned$word, "^[0-9]+$")) 
  
# checking the data has been accurately cleaned
head(key_elden_new, n = 15)

# renaming word column to words
colnames(key_elden_new)[1] <- "words"


# Step 4: EDA

# 4.1 finding most common words
key_elden_new %>%
  filter(n > 70) %>% # plotting data
  ggplot(aes(x = reorder(words, + n), y = n)) + # orders from highest to lowest
  geom_col() +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count", title = "Frequent Words \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title =  element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold", colour = "darkgreen"),
        axis.title.y = element_text(face = "bold", colour = "darkgreen"))
# https://dk81.github.io/dkmathstats_site/rtext-freq-words.html

# 4.2 summarising number of comments
key_reddit_new_data %>%
  summarize(avg_comments = mean(comments), 
            median_comments = median(comments))

# 4.3 NRC lexicon (categorizes words by specific emotion)
key_elden_new %>%
  unnest_tokens(output = word, input = words) %>%
  inner_join(get_sentiments('nrc')) %>%
  group_by(sentiment) %>%
  count() %>%
  # visualizing results
  ggplot(aes(x = reorder(sentiment, X = n), y = n, fill = sentiment)) +
  geom_col() +
  guides(fill = F) +
  coord_flip() +
  theme_wsj() +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") 
 

# 4.4 finding the most common pos and neg words and how much each word contributed
#to the sentiment

key_elden_tidy <- key_reddit_new_data |> # used the og data
  group_by(text) |>
  ungroup() |>
  unnest_tokens(word, text)

key_elden_word_counts <- key_elden_tidy |>
  inner_join(get_sentiments("bing")) |>
  count(word, sentiment, sort = T) |>
  anti_join(custom) |> # applied the custom dict to clean data
  group_by(sentiment) |>
  ungroup()

# plotting
key_elden_word_counts |>
  group_by(sentiment) |>
  top_n(10) |>
  ungroup() |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to Sentiment",
       y = NULL)


# Step 5: calculating sentiment

# 5.1 using BING lexicon to compare amount of neg and pos words in all reviews
key_elden_pos_neg <- key_elden_new %>%
  select(words) %>%
  unnest_tokens(output = word, input = words) %>%
  inner_join(get_sentiments(lexicon = 'bing'))

head(key_elden_pos_neg)

# plotting the results of how many pos and neg words in all of the reviews
ggplot(key_elden_pos_neg, aes(x = sentiment, fill = sentiment)) +
  geom_bar() +
  guides(fill = F) +
  theme_economist() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1)
# -- Negative words are a couple hundred above positive words

# calculating percentages 
total <- 317 + 199
total

neg <- (317/total) * 100
neg

pos <- (199/total) * 100
pos
# -- neg = 61.43% pos = 38.57%
# -- as of 18th July 2022, we can conclude that Elden Ring's bosses are percieved as more negative than positive by Redditors

