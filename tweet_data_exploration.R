
# Tweet data exploration and selection -----------------------------------------

tweet_data <- readRDS("tweet_data.rds") # from script: getting_user_tweets.R

# Getting rid of English tweets -------------------------------------------

devtools::install_version("cldr",version="1.1.0")

library(cldr)

detect1 <- detectLanguage(tweet_data$text, isPlainText = TRUE, 
                          pickSummaryLanguage = TRUE, 
                          removeWeakMatches = FALSE)

lg <- detect1 %>% select(candidateLanguage1)
colnames(lg) <- "lg"

# vectorize data frame from "lg" so tweet data does not end ub with a nested data frame

lg_vector <- lg[ , "lg"]

tweet_data$lg <- lg_vector # add vector as column to tweet data

tweet_data$lg <- factor(tweet_data$lg, levels = c("GERMAN", "ENGLISH"), labels = c("de", "en"))

# filter out German tweets

tweet_data_ger <- tweet_data %>% filter(lg == "de") # this reduced the overall number of 18649 tweets to

tweet_data_ger$screenName <- tolower(tweet_data_ger$screenName) # as in the diss_sub data frame


# 22004 tweets

# This resulted in the loss of 5 more participants!!! Small number of tweets - mostly in English

saveRDS(tweet_data_ger, file = "tweet_data_ger.rds")

tweet_data_ger <- readRDS("tweet_data_ger.rds")

# Explore German tweets ---------------------------------------------------

# all tweets

sum_tweets <- as.data.frame(tweet_data %>% group_by(screenName) %>% summarise(tweet_num = n()))

# German tweets

sum_tweets_ger <- as.data.frame(tweet_data_ger %>% group_by(screenName) %>% summarize(tweet_num = n()))

#sum_tweets_ger <- sum_tweets_ger %>% mutate(part_id = as.numeric(interaction(screenName, drop = FALSE)))

# combine counts with tweet data

tweet_data_ger <- left_join(tweet_data_ger, sum_tweets_ger, by = "screenName")

saveRDS(tweet_data_ger, file = "tweet_data_ger.rds")


range(tweet_data_ger$tweet_num)

mean(sum_tweets_ger$tweet_num) # = 314.34

sd(sum_tweets_ger$tweet_num) # = 526.79

##tweet_sub <- sum_tweets %>% filter(tweet_num >= 25 & tweet_num <= 1366)

## Determine my own cutoff part with less than 50 words...use clean tweets for that...

source("text_processing.R")

tweet_data_ger$clean <- lapply(tweet_data_ger$text, text_processing)

tweet_data_ger$clean <- unlist(tweet_data_ger$clean)

### Find unique words per tweet

word_list <- strsplit(tweet_data_ger$clean, " ")

uniq_words_per_tweet = sapply(word_list, function(x) length(unique(x)))

tweet_data_ger <- cbind(tweet_data_ger, uniq_words_per_tweet)

uniq_sum_words <- tweet_data_ger %>% group_by(screenName) %>% summarise(uniq_words_overall = sum(uniq_words_per_tweet))

tweet_data_ger <- left_join(tweet_data_ger, uniq_sum_words, by = "screenName")

## Filter out participants with fewer than 50 unique words overall

tweet_data_ger <- tweet_data_ger %>% filter(uniq_words_overall >= 50)

new_sum_tweet_ger <- as.data.frame(tweet_data_ger %>% group_by(screenName) %>% summarize(tweet_num = n()))

## Combine twitter data (counts) with diss_sub (Qualtrics data)

diss_data <- semi_join(diss_sub, new_sum_tweet_ger, by = "screenName", copy = TRUE)# filtering join
#copy = TRUE needed bc data frames from different sources.

diss_data <- left_join(diss_data, new_sum_tweet_ger, by = "screenName") # adds num of tweets


# Last thing - create part_ids

tweet_data_ger <- tweet_data_ger %>% mutate(part_id = as.numeric(interaction(screenName, drop = TRUE))) # also works

tweet_data_ger <- tweet_data_ger %>% select(part_id, everything())

diss_data <- diss_data %>% mutate(part_id = as.numeric(interaction(screenName, drop = TRUE)))

diss_data <- diss_data %>% select(part_id, everything())

saveRDS(tweet_data_ger, file = "tweet_data_ger.rds")
saveRDS(diss_data, file = "diss_data.rds")
