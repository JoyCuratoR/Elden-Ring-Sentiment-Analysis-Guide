## Background
Elden Ring is an action role-playing game (RPG) released in 2022. It's gained notoriety for it's system of gameplay and the various challenging, and complex bosses. 

## Purpose
I wanted to know if there was an equal love and hate for Elden Ring's bosses and if that affects the sentiment of the game. The focus of this project is to understand the ratio of positive and negative sentiment scores for Elden Ring's bosses and how much the sentiment scores of the bosses contribute to the sentiment scores of the game.

## Methodology
The sentiment of both datasets were calculated using a dictionary-based approach using the ``BING`` lexicon which categorizes words into negative and positive categories. After the data has been categorized, it then got grouped by sentiment and counted to determine the negative and positive ratio. Additionally, the data was explored with the ``NRC`` lexicon which categorizes words into specific emotion categories.

## Data
The data was collected from Reddit using an R package called
RedditExtractoR. One dataset contains keywords of 'bosses' and 'boss fight' and the second dataset does not contain any keywords. Otherwise, both datasets had been gathered with the same parameters. Moreover, the data was last updated on 18th of July 2022 and is only intended to demonstrate a static, snapshot report.

## Phase 1: Loading Packages & Collecting Data
To begin, we'll load in the required packages.

``` r
library(RedditExtractoR) # a wrapper that collects data from Reddit
library(readtext) # loads in CSVs
library(qdap) # sentiment dictionaries
library(tidyverse) # helps manipulate and clean data
library(tidytext) # helps with text mining
library(ggthemes) # themes for ggplots
```

Next, let's collect data from Reddit and export it as a CSV. The function ```find_thread_urls()``` from ``` RedditExtractoR ``` returns posts from a specific subreddit and the set parameters. Since we're trying to find what the ratio of negative and positive sentiment scores are for Elden Ring's bosses, we'll set the parameters as:

``` r
reddit_new <- find_thread_urls(
  subreddit = "eldenring",
  sort_by = "new",
  period = "all",
  keywords = "boss, boss fight" # to return Reddit posts that contain the words "boss" and "boss fight"
)
```

Let's save the data as a data frame and export it as a CSV file.

``` r
df <- as.data.frame(reddit_new)
write.csv(df, 'reddit_new.csv')
```

## Phase 2: Processing & Cleaning Data
Now that we have our static data, we'll load in the CSV we just saved and begin cleaning it. 

``` r
key_elden_folder <- "D:/R_Studio/Project_Vault/OP_EldenRing_Sentiment/reddit_new.csv"

key_reddit_new_data <- read_csv(key_elden_folder)
```

We'll  use the function ```glimpse()``` to check that our columns have the correct data type assign to them. Then we'll clean out all the ``NA`` strings using the function ```na.omit()``` 

``` r
glimpse(key_reddit_new_data)

key_reddit_new_data <- na.omit(key_reddit_new_data) # omits all 'NA' inputs of the dataset
``` 

Next, we'll tokenize our data using ``unnest_tokens``. 

(Tokenizing is the processing method of separating words into individual tokens and makes them easier to analyze than if they were grouped in sentences.) 

The ``unnest_tokens`` function has two main parameters: 

``output`` = the name of the output column created

``input`` = the name of the column that gets split

 We'll ``anti_join()``  the lexicon ``stop_words`` to clean for stopwords (words like: it, the, I, to, a, etc.). The ``count()`` function adds up how many times each word appears in the dataset.

``` r
# tokenize data using tidyverse
key_reddit_new_data %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
```

If we look through our data now using the function ``head()`` 

``` r
head(key_reddit_new_data, n = 15) # returns the top 15
```

We'll see that there are some particular strings that the ``stop_words`` lexicon didn't clean out. So to clean them from the tibble, we'll manually create a custom stopwords lexicon. All we're doing is adding new rows to the ``stop_words`` lexicon. To do so, create a variable called "custom" and use the function ``add_row()``. 

``` r
# creating a custom stopwords lexicon for unique strings
custom <- add_row(stop_words, word = "amp", lexicon = "custom") 
custom <- add_row(custom, word = "x200b", lexicon = "custom") 
custom <- add_row(custom, word = "gt", lexicon = "custom")
custom <- add_row(custom, word = "https", lexicon = "custom")
custom <- add_row(custom, word = "It", lexicon = "custom")
custom <- add_row(custom, word = "lt", lexicon = "custom")
```

Then, we'll write in our newly created custom lexicon to replace ``anti_join(stop_words)`` with ``anti_join(custom)``. 

``` r
# re-writing code to include custom variable
key_elden_cleaned <- key_reddit_new_data %>%
  unnest_tokens(word, text) %>%
  anti_join(custom) %>%
  count(word, sort = TRUE)
```

Now, we'll clean the data for numbers since we only want to analyze textual data, double-check that the data thus far has been accurately cleaned, and rename the column ``word`` in the tibble to ``words`` (this will be explained in Phase 4).

``` r
# cleaning numbers
key_elden_new <- key_elden_cleaned %>%
  filter(! str_detect(key_elden_cleaned$word, "^[0-9]+$")) 
  
# checking the data has been accurately cleaned
head(key_elden_new, n = 15)

# renaming word column to words
colnames(key_elden_new)[1] <- "words"
```

## Phase 3: Performing EDA
Now that the data is cleaned, let's start exploring it. We can start by finding the most common words and visualizing it with a bar chart. 

``` r
# finding most common words and plotting a bar chart
key_elden_new %>%
  filter(n > 70) %>% # return only words that appear greater than 70
  ggplot(aes(x = reorder(words, + n), y = n)) + # + n orders from highest to lowest
  geom_col() +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count", title = "Frequent Words \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") + # labels each bar using the count of n
  theme(plot.title =  element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold", colour = "darkgreen"),
        axis.title.y = element_text(face = "bold", colour = "darkgreen"))
```

This chart tells us that the word 'game' is the most frequent word in the dataset, with 'boss' and 'bosses' following. Given how we specified for keywords of 'boss' and 'boss fight' it's no surprise that these words show up near the top.

![1](https://github.com/JoyCuratoR/Elden-Ring-Sentiment-Analysis-Guide/blob/main/Pasted%20image%2020220808184113.png)

Next, we'll find out the average and median number of comments. 

``` r
# summarising number of comments
key_reddit_new_data %>%
  summarize(avg_comments = mean(comments), 
            median_comments = median(comments))
```


Beyond the percentage ratios, we can analyze more on what emotions contribute to the negative and positive scores. 

Using the ``NRC`` lexicon which categorizes words into different emotions, we can glean what emotions are most prominent overall. (A/N: positive and negative are emotion categories in this lexicon.)

Like with the ``BING`` lexicon, we'll use the function ``inner_join()`` to bind the lexicon to the data and group by sentiment.

``` r
# 5.2 NRC lexicon (categorizes words by specific emotion)
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
  theme_wsj()
```

At first glance, the 'positive' emotion category is the highest, while 'surprise' is the lowest. Adding up all the "happy" emotions and the "unhappy" emotions, respectively results in: 

Happy = 805

Unhappy = 874 

Or

Happy = 47.94 %

Unhappy = 52.05 %

Thus, this still results in a majority of Redditors being unhappy with the Elden Ring's bosses.

![2](https://github.com/JoyCuratoR/Elden-Ring-Sentiment-Analysis-Guide/blob/main/Pasted%20image%2020220808220151.png)


Moreover, if we wanted to know exactly which individual words contributed the most to the negative and positive scoring, we can use the ``BING`` sentiment lexicon. 

For this we're using the original data that was only cleaned by the normal ``stop_words`` lexicon and not the one we customized (this will be explained why, after). 

First, let's tokenize the original dataset without cleaning for stopwords. We'll create a ``word`` column using the ``unnest_tokens`` function.

``` r
# finding the most common pos and neg words and how much each word contributed to the sentiment

key_elden_tidy <- key_reddit_new_data |> # used the og data
  group_by(text) |>
  ungroup() |>
  unnest_tokens(word, text)
```

Again, we'll ``inner_join`` the ``BING`` sentiment lexicon, this time using the function ``anti_join`` to clean for stopwords using the customized stopwords lexicon made earlier.

```
key_elden_word_counts <- key_elden_tidy |>
  inner_join(get_sentiments("bing")) |>
  count(word, sentiment, sort = T) |>
  anti_join(custom) |> # applied the custom dict to clean data
  group_by(sentiment) |>
  ungroup()
```

Now let's plot our results.

``` r
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
```

As we can determine, the word 'damage' contributed the most to the negative score and the word 'fun' contributed the most to the positive score.

![3](https://github.com/JoyCuratoR/Elden-Ring-Sentiment-Analysis-Guide/blob/main/Pasted%20image%2020220811193950.png)

So why didn't we used the already fully cleaned dataset instead of having to tokenize the original, uncleaned dataset, and then re-apply the ``stop_words`` lexicon to it?

 If we used the variable containing our customized stopwords lexicon, using the same code to calculate and plot the contribution of individual words to the sentiment, then our results will be this: 
``` r
# A tibble: 516 x 3
   word         sentiment     n
   <chr>        <chr>     <int>
 1 abuse        negative      1
 2 abused       negative      1
 3 abyss        negative      1
 4 accessible   positive      1
 5 accomplished positive      1
 6 accurate     positive      1
 7 achievement  positive      1
 8 achievements positive      1
 9 advanced     positive      1
10 advantage    positive      1
# ... with 506 more rows
```

And the plot will come out to look like this: 

![4](https://github.com/JoyCuratoR/Elden-Ring-Sentiment-Analysis-Guide/blob/main/Pasted%20image%2020220811191354.png)



## Phase 4: Calculating Sentiment Ratios for the Bosses and the Game
### 4.1 Finding the Sentiment Ratio of the Bosses
Earlier in Phase 2, we re-named the column ``word`` to ``words``, this is because when calculating sentiment, we need to unnest the tokens using the function ``unnest_tokens`` which has parameters of ``output`` and ``input``.

``output`` = the name of the output column created

``input`` = the name of the column that gets split as string

It is almost always wise to use the label 'word' as the ``output`` parameter, which is why earlier we changed the column ``word`` in the tibble to ``words``. 

To calculate the sentiment using the ``BING`` sentiment lexicon, use the function ``inner_join`` to bind the lexicon to the data.

``` r
# 5.1 using BING lexicon to compare amount of neg and pos words in all reviews
key_elden_pos_neg <- key_elden_new %>%
  select(words) %>%
  unnest_tokens(output = word, input = words) %>%
  inner_join(get_sentiments(lexicon = 'bing'))

head(key_elden_pos_neg)
```

Plot the results using a ``ggplot bar chart`` so we can see the difference clearly. The sentiment will be the ``x-axis value`` and the ``count`` will be the ``y-axis value``. 

``` r 
# plotting the results of how many pos and neg words 
ggplot(key_elden_pos_neg, aes(x = sentiment, fill = sentiment)) +
  geom_bar() +
  guides(fill = F) +
  theme_economist() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1)
```

When we plot out the results, we can clearly see a big difference between the amount of negative versus positive words used.

![5](https://github.com/JoyCuratoR/Elden-Ring-Sentiment-Analysis-Guide/blob/main/Pasted%20image%2020220808184004.png)

Now let's find the percentage ratio between negative and positive words. 

``` r
# calculating percentages
total <- 317 + 199
total

neg <- (317/total) * 100
neg

pos <- (199/total) * 100
pos
```

As we can see, 61.43% of the sentiment is negative while only 38.57% is positive. Thus, we can conclude that Elden Ring's bosses are perceived as more negative than positive by Redditors as of 18th July 2022 (when the data was collected).

### 4.2 Finding the Sentiment Ratio of the Elden Ring Game
It's important to note that the data gathered for both sets was gathered on the same day for consistency. 

Finding the sentiment ratio of the game is much like what we did with the bosses. We begin by collecting the data from Reddit but this time we don't use any keywords. 

``` r
# Step 1: collecting raw data
reddit_new_all <- find_thread_urls(
  subreddit = "eldenring",
  sort_by = "new",
  period = "all"
)
```

Then, we save the data as a dataframe and export it as a CSV file.

``` r
# Step 2: Exporting and saving the data 
df <- as.data.frame(reddit_new_all)
write.csv(df, 'reddit_new_all.csv')
```

Load in the CSV file, and omit all the ``NA`` inputs. 

``` r
# loading data into environment
folder <- "D:/R_Studio/Project_Vault/OP_EldenRing_Sentiment/reddit_new_all.csv"
reddit_new_data <- read_csv(folder)

glimpse(reddit_new_data)

reddit_new_data <- na.omit(reddit_new_data) # getting rid of all the 'NA's
```

Tokenize the data, find the unique string values, create a custom lexicon by adding rows to the ``stop_words`` lexicon, and re-tokenize the data with the customized lexicon. 

``` r
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
```

Next, we clean for numbers, double-check data has been properly cleaned, and change the column ``word`` to ``words``. 

``` r
# cleaning numbers
reddit_elden_new <- cleaned %>%
  filter(! str_detect(cleaned$word, "^[0-9]+$")) 

# checking the data has been accurately cleaned
head(reddit_elden_new, n = 15)

# renaming word column to words
colnames(reddit_elden_new)[1] <- "words"
```

### 4.2 EDA (optional)
Then do some Exploratory Data Analysis by finding the most common words, and summarizing comments.

``` r
# finding most common words
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

# summarising comments
reddit_new_data %>%
  summarize(avg_comments = mean(comments), 
            median_comments = median(comments))
```

Now we can do further analysis by finding out which specific emotion categories are the highest/lowest and which words had the most contribution to the negative and positive outcomes.

``` r
# NRC lexicon (categorizes words by specific emotion)
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
  theme_wsj()
```

![6](https://github.com/JoyCuratoR/Elden-Ring-Sentiment-Analysis-Guide/blob/main/Pasted%20image%2020220812120716.png)


``` r
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
```

![7](https://github.com/JoyCuratoR/Elden-Ring-Sentiment-Analysis-Guide/blob/main/Pasted%20image%2020220812121302.png)

### 4.2 Continued
Next we get into finding the positive and negative sentiment ratio by plotting a bar chart and then calculating the percentages. 

``` r
# using BING lexicon to compare amount of neg and pos words in all reviews
pos_neg <- reddit_elden_new %>%
  select(words) %>%
  unnest_tokens(output = word, input = words) %>%
  inner_join(get_sentiments(lexicon = 'bing'))

head(pos_neg)
```

``` r
> head(pos_neg)
# A tibble: 6 x 2
  word    sentiment
  <chr>   <chr>    
1 damage  negative 
2 fun     positive 
3 attack  negative 
4 attacks negative 
5 pretty  positive 
6 rot     negative 
```

``` r
# plotting the results of how many pos and neg words in all of the reviews
ggplot(pos_neg, aes(x = sentiment, fill = sentiment)) +
  geom_bar() +
  guides(fill = F) +
  theme_economist() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1)
```

The amount of negative words are almost a couple hundred above positive words.

![8](https://github.com/JoyCuratoR/Elden-Ring-Sentiment-Analysis-Guide/blob/main/Pasted%20image%2020220811234633.png)

Let's calculate the percentage ratio.

``` r
total <- 729

negative <- (471/total) * 100

positive <- (258/total) * 100
```

Results: 

Negative = 64.61% 

Positive = 35.39%

## Phase 5: Finding How Much the Bosses Contribute to the Game's Sentiment 
Finally, we can tie everything together by calculating how much the bosses' sentiment scores contribute to the game's sentiment scores.

``` r
game_neg <- 64.61/100
game_pos <- 35.39/100

boss_neg <- 61.43/100
boss_pos <- 38.57/100

neg_contribution <- (boss_neg * game_neg) * 100
pos_contribution <- (boss_pos * game_pos) * 100
```

So it turns out that the bosses contribute 39.69% to the game's negative sentiment score and they also contribute 13.65% to the game's positive sentiment score.

## Additional Analysis Options
- N-gram analysis
- Key-word-in-context analysis 
- Aspect-Based Sentiment Analysis

## Appendix
https://dk81.github.io/dkmathstats_site/rtext-freq-words.html

https://www.tidytextmining.com/sentiment.html

https://jtr13.github.io/cc21/sentiment-analysis-and-wordcloud.html
