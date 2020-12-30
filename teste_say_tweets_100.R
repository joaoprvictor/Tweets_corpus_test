
# Code based on "https://picoral.github.io/corpus_of_tweets_workshop/#inspect-and-clean-tweets"
# and on "https://github.com/rajkumarjagdale/Extracting-Tweets-from-Twitter-Using-R"


rm(list=ls())  # removes all objects from the current workspace (R memory)

# carregar pacotes
install.packages("Rtools")
library(Rtools)
library(twitteR)
library(ROAuth)
install.packages("RCurl")
library(RCurl)
install.packages("httr")
library(httr)                

# importando os APIs
consumerKey = "XXXXXXXXXX"
consumerSecret = "XXXXXXXXXX"
accessToken = "XXXXXXXXXX"
accessSecret = "XXXXXXXXXX"

#setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
setup_twitter_oauth(consumerKey, consumerSecret, 
                    accessToken, accessSecret)

sayTweets = searchTwitter("say",lang = "en", n = 100)
length(sayTweets)

tweets_df = twListToDF(sayTweets)

write.csv(tweets_df, file = "C:\\Users\\joaop\\OneDrive\\Documents\\tweets\\sayTweets.csv", 
          row.names=F)


# inspect and clean
##count tweets by user
tweets_df %>%
  count(screenName)

## get min and max of dates tweets were created by users
tweets_df %>%
  group_by(screenName) %>%
  summarise(begin = min(created),
            end = max(created))

## filter out (!) anything that is a retweet
original_tweets <- tweets_df %>%
  filter(!isRetweet)

## count tweets by user for original tweets
original_tweets %>%
  count(screenName)

# Tokenize text
## reduce the number of variables in our data,
tweets_smaller <- original_tweets %>%
  select(text, screenName,
         favoriteCount, retweetCount, id)
## tokenize words
tweets_tokenized <- tweets_smaller %>%
  unnest_tokens(word, text)

## inspect data
tweets_tokenized %>%
  head()

## tokenize bigrams
tweets_bigrams <- tweets_smaller %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

## inspect data
tweets_bigrams %>%
  head()

subcorpora_size <- tweets_tokenized %>%
  count(screenName)

## inspect data
subcorpora_size

## check stop_words data frame
stop_words %>%
  count(word) %>%
  arrange(-n)

## the smallest lexicon is snowball
stop_words %>%
  count(lexicon)

## filter the stop words to keep only words from the snowball lexicon.
my_stop_words <- stop_words %>%
  filter(lexicon == "snowball")

## remove stop words from pencil reviews tokenized
tweets_tokenized_clean <- tweets_tokenized %>%
  anti_join(my_stop_words)

# arrange count so we see most frequent words first
tweets_tokenized_clean %>%
  count(word, screenName) %>%
  arrange(-n) %>%
  head()

# remove not word tokens
tokens_to_remove <- c("https", "t.co", "amp", "et")

## remove stop words from tweets tokenized
tweets_tokenized_clean <- tweets_tokenized_clean %>%
  filter(!(word %in% tokens_to_remove))

## arrange count so we see most frequent words first
tweets_tokenized_clean %>%
  count(word, screenName) %>%
  arrange(-n) %>%
  head()

# looks good, create word_frequency_per_user data frame
word_frequency_per_user <- tweets_tokenized_clean %>%
  count(word, screenName)

## Plotting the data makes it easier to compare frequent tokens 
## across different users.

word_frequency_per_user %>%
  group_by(screenName) %>%
  top_n(2) %>%
  ggplot(aes(x = n, 
             y = reorder_within(word, n, screenName))) +
  geom_col() +
  facet_wrap(~screenName, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "")

## change the n in the column name to total
subcorpora_size <- subcorpora_size %>%
  rename(total = n)

## inspect subcopora_size again
subcorpora_size

##We can now join subcorpora_size with our word_frequency_per_user 
##data frame by the column they have in common, which is screenName.

word_frequency_per_user <- left_join(word_frequency_per_user,
                                     subcorpora_size)


##create a new column with normalized frequency using mutate().
word_frequency_per_user <- word_frequency_per_user  %>%
  mutate(n_norm = (n/total)*10000)

## inspect data
word_frequency_per_user %>%
  head()

## Plot the data again, but by normalized frequency.

word_frequency_per_user %>%
  group_by(screenName) %>%
  top_n(20) %>%
  ggplot(aes(x = n_norm, 
             y = reorder_within(word, n_norm, screenName))) +
  geom_col() +
  facet_wrap(~screenName, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "")

#We can calculate term frequency inverse document 
# frequency (tf-idf) instead of normalized frequency. 
# The goal in using tf-idf is to decrease the weight for
# commonly used words (i.e., words used across all documents) 
# and increase the weight for words that are less frequent in 
#other documents in that collection.

# calculate tf-idf based on n, providing the word column and the category col
word_tf_idf <- word_frequency_per_user %>%
  bind_tf_idf(word, screenName, n)

# inspect data
word_tf_idf %>%
  head()

#We can also add range, to decide what words to keep 
#and understand tf-idf a little better.

## calculate range per word (status_id indicates individual tweets)
word_range <- tweets_tokenized_clean %>%
  distinct(word, id) %>%
  count(word) %>%
  rename(range = n)

## add range to data frame with left_join
word_tf_idf <- left_join(word_tf_idf, word_range)
                         
## inspect data
word_tf_idf %>%
  head()
              
# what's the mean range?
mean(word_tf_idf$range)

## Plotting it again, by tf-idf filtering by range.

word_tf_idf %>%
  filter(range > 5) %>%
  group_by(screenName) %>%
  top_n(n = 10, wt = tf_idf) %>%
  ggplot(aes(x = tf_idf, 
             y = reorder_within(word, tf_idf, screenName))) +
  geom_col() +
  facet_wrap(~screenName, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "")


# Corpus searching
searchExpression = "say"

original_tweets %>%  
  filter(grepl(searchExpression, text)) %>%
  head() %>%
  pull(text)

## use the tokenized data frame to retrieve KWIC.
tweets_tokenized %>%  
  mutate(kwic = ifelse(word == searchExpression,
                       TRUE, FALSE)) %>%
  mutate(before = paste(lag(word, 3), lag(word, 2), lag(word)),
         after = paste(lead(word), lead(word, 2), lead(word, 3))
  ) %>%
  filter(kwic) %>%
  select(screenName, before, word, after) %>%
  top_n(n = 10, wt = after) %>%
  knitr::kable(align = c('l', 'r', 'c', 'l'))

           
                         
                         
                         
                         
                         
                         
                        
                         
