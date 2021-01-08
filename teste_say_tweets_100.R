
# Code based on "https://picoral.github.io/corpus_of_tweets_workshop/#inspect-and-clean-tweets"
# and on "https://github.com/rajkumarjagdale/Extracting-Tweets-from-Twitter-Using-R"


# carregar pacotes
install.packages("Rtools")
library(Rtools)
install.packages("twitteR")
library(twitteR)
install.packages("ROAuth")
library(ROAuth)
install.packages("RCurl")
library(RCurl)
install.packages("httr")
library(httr)
library(tidyverse)
library(tidytext)

# importando os APIs
consumerKey = "Gyj1h5odB016PKZd3xw7YTDLU"
consumerSecret = "ZlmjrQSlWWw00kgMENh2Zkj47dGnGqTRUavQTgnN5INgI6kiHr"
accessToken = "724276541560070145-gb1nsV2PGmSOPK2V4lt0bD49CO7ydXN"
accessSecret = "IgsY2wmyRvgwKQJVevTnuuOgMikzuI1UpIFrIb3pwWK6Q"

#setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
setup_twitter_oauth(consumerKey, consumerSecret, 
                    accessToken, accessSecret)

#Collecting the data
sayTweets = searchTwitter("say", lang = "en", n = 400)
length(sayTweets)

#putting into a DF
sayTweets_df = twListToDF(sayTweets)

#exporting a df
write.csv(sayTweets_df, file = "sayTweets.csv", 
          row.names=F)

# inspect and clean
##count tweets by user
sayTweets_df %>%
  count(screenName)

## get min and max of dates tweets were created by users
sayTweets_df %>%
  group_by(screenName) %>%
  summarise(begin = min(created),
            end = max(created))

## filter out (!) anything that is a retweet
original_sayTweets <- sayTweets_df %>%
  filter(!isRetweet)

## count tweets by user for original tweets
original_sayTweets  %>%
  count(screenName)

# Tokenize text
## reduce the number of variables in our data,
sayTweets_smaller <- original_sayTweets %>%
  select(text, screenName,
         favoriteCount, retweetCount, id)

## tokenize words
sayTweets_tokenized <- sayTweets_smaller %>%
  unnest_tokens(word, text)

## inspect data
sayTweets_tokenized %>%
  head()

## tokenize bigrams
tweets_bigrams <- sayTweets_smaller %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

## inspect data
tweets_bigrams %>%
  head()

## how much does each user contribute in terms of tokens?
saySubcorpora_size <- sayTweets_tokenized %>%
  count(screenName)

## inspect data
saySubcorpora_size

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

## remove stop words from tweets - tokenized
sayTweets_tokenized_clean <- sayTweets_tokenized %>%
  anti_join(my_stop_words)

# arrange count so we see most frequent words first
sayTweets_tokenized_clean %>%
  count(word, screenName) %>%
  arrange(-n) %>%
  head()

# remove not word tokens
tokens_to_remove <- 

## remove stop words from tweets tokenized
sayTweets_tokenized_clean <- sayTweets_tokenized_clean %>%
  filter(!(word %in% tokens_to_remove))

## arrange count so we see most frequent words first
sayTweets_tokenized_clean %>%
  count(word, screenName) %>%
  arrange(-n) %>%
  head()

# looks good, create word_frequency_per_user data frame
sayWord_frequency_per_user <- sayTweets_tokenized_clean %>%
  count(word, screenName)

## Plotting the data makes it easier to compare frequent tokens 
## across different users.

sayWord_frequency_per_user %>%
  group_by(screenName) %>%
  top_n(2) %>%
  ggplot(aes(x = n, 
             y = reorder_within(word, n, screenName))) +
  geom_col() +
  facet_wrap(~screenName, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "")

## change the n in the column name to total
saySubcorpora_size <- saySubcorpora_size %>%
  rename(total = n)

## inspect subcopora_size again
saySubcorpora_size

##We can now join subcorpora_size with our word_frequency_per_user 
##data frame by the column they have in common, which is screenName.

sayWord_frequency_per_user <- left_join(sayWord_frequency_per_user,
                                     saySubcorpora_size)

##create a new column with normalized frequency using mutate().
sayWord_frequency_per_user <- sayWord_frequency_per_user  %>%
  mutate(n_norm = (n/total)*10000)

## inspect data
sayWord_frequency_per_user %>%
  head()

## Plot the data again, but by normalized frequency.

sayWord_frequency_per_user %>%
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
sayWord_tf_idf <- sayWord_frequency_per_user %>%
  bind_tf_idf(word, screenName, n)

# inspect data
sayWord_tf_idf %>%
  head()

#We can also add range, to decide what words to keep 
#and understand tf-idf a little better.

## calculate range per word (status_id indicates individual tweets)
sayWord_range <- sayTweets_tokenized_clean %>%
  distinct(word, id) %>%
  count(word) %>%
  rename(range = n)

## add range to data frame with left_join
sayWord_tf_idf <- left_join(word_tf_idf, word_range)

## inspect data
sayWord_tf_idf %>%
  head()

# what's the mean range?
mean(word_tf_idf$range)

## Plotting it again, by tf-idf filtering by range.

sayWord_tf_idf %>%
  filter(range > 4) %>%
  group_by(screenName) %>%
  top_n(n = 10, wt = tf_idf) %>%
  ggplot(aes(x = tf_idf, 
             y = reorder_within(word, tf_idf, screenName))) +
  geom_col() +
  facet_wrap(~screenName, scales = "free_y") +
  scale_y_reordered() +
  labs(y = "")

# Corpus searching
say = "say"

original_sayTweets %>%  
  filter(grepl(say, text)) %>%
  head() %>%
  pull(text)

## use the tokenized data frame to retrieve KWIC.
sayTweets_tokenized %>%  
  mutate(kwic = ifelse(word == say,
                       TRUE, FALSE)) %>%
  mutate(before = paste(lag(word, 3), lag(word, 2), lag(word)),
         after = paste(lead(word), lead(word, 2), lead(word, 3))
  ) %>%
  filter(kwic) %>%
  select(screenName, before, word, after) %>%
  top_n(n = 10, wt = after) %>%
  knitr::kable(align = c('l', 'r', 'c', 'l'))


                         
                         
                         
                         
                         
                        
                         
