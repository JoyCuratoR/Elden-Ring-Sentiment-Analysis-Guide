
library(RedditExtractoR)
library(readtext)
library(qdap)
library(tidyverse)
library(tidytext)
library(ggthemes)


# Step 1: collecting raw data
reddit_new_all <- find_thread_urls(
  subreddit = "eldenring",
  sort_by = "new",
  period = "all"
)

# Step 2: Exporting and saving the data 
df <- as.data.frame(reddit_new_all)
write.csv(df, 'reddit_new_all.csv')

# Step 3: Load CSV in & Clean it

# 3.1 loading data into environment
folder <- "D:/R_Studio/Project_Vault/OP_EldenRing_Sentiment/reddit_new_all.csv"
reddit_new_data <- read_csv(folder)

glimpse(reddit_new_data)

reddit_new_data <- na.omit(reddit_new_data) # getting rid of all the 'NA's

# 3.2 cleaning data

# tokenize data using tidyverse
reddit_new_data %>%
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
cleaned <- reddit_new_data %>%
  unnest_tokens(word, text) %>%
  anti_join(custom) %>%
  count(word, sort = TRUE)

# cleaning numbers
reddit_elden_new <- cleaned %>%
  filter(! str_detect(cleaned$word, "^[0-9]+$")) 

# checking the data has been accurately cleaned
head(reddit_elden_new, n = 15)

# renaming word column to words
colnames(reddit_elden_new)[1] <- "words"


# Step 4: EDA
# 4.1 finding most common words
reddit_elden_new %>%
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

# 4.2 summarising comments
reddit_new_data %>%
  summarize(avg_comments = mean(comments), 
            median_comments = median(comments))

# 4.3 NRC lexicon (categorizes words by specific emotion)
reddit_elden_new %>%
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

tidy <- reddit_new_data |> # used the og data
  group_by(text) |>
  ungroup() |>
  unnest_tokens(word, text)

bing_word_counts <- tidy |>
  inner_join(get_sentiments("bing")) |>
  count(word, sentiment, sort = T) |>
  anti_join(custom) |> # applied the custom dict to clean data
  group_by(sentiment) |>
  ungroup()

# plotting
bing_word_counts |>
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
pos_neg <- reddit_elden_new %>%
  select(words) %>%
  unnest_tokens(output = word, input = words) %>%
  inner_join(get_sentiments(lexicon = 'bing'))

head(pos_neg)

# plotting the results of how many pos and neg words in all of the reviews
ggplot(pos_neg, aes(x = sentiment, fill = sentiment)) +
  geom_bar() +
  guides(fill = F) +
  theme_economist() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1)
# -- Negative words are a couple hundred above positive words
# -- neg = 64.61% pos = 35.39%

