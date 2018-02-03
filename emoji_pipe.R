

# Emoji handling ----------------------------------------------------------

# Start with German tweets only == tweet_ger


## Change emoji encoding for emoji stuff

tweet_data_ger$emoji <- unlist(lapply(tweet_ger$text, function(row) iconv(row, "latin1", "ASCII", "byte")))

data_comb$text <- unlist(lapply(data_comb$text, function(row) iconv(row, "latin1", "ASCII", "byte")))

# Problem!! gets rid of Umlaut as well... so only for emoji encoding!!

## Read in emoji dicts

emoji_dict <- read.csv("./emoji_dict.csv", sep = ";", header = TRUE) # from https://github.com/today-is-a-good-day/emojis/blob/master/emDict.csv
# and http://opiateforthemass.es/articles/emoticons-in-R/

# has 842 entries

emoji_dict <- rename(emoji_dict, description = Description, r.encoding = R.encoding)
emoji_dict <- emoji_dict %>% mutate(description = tolower(description))

## ---- utility functions ----
# this function outputs the emojis found in a string as well as their occurences

library(stringr)
source("emoji_functions.R")

# create matching patterns

matchto <- as.character(emoji_dict$r.encoding)
description <- emoji_dict$description

## ---- most used emoji ----
# rank emojis by occurence in data
data_comb[data_comb$part_id == 2,]$text

rank <- emojis_matching(data_comb$text, matchto, description) %>% # you get text, description, count, and sentiment (here: NA)
        group_by(description) %>% 
        summarize(n = sum(count)) %>% 
        arrange(-n)

head(rank, 10)

## ---- tweets with most emojis ----

tweets_count <- emojis_matching(data_comb$text, matchto, description) %>% 
        group_by(text) %>% 
        summarize(n = sum(count)) %>%
        # I add the time created because it makes usermedia_merged 
        #%>% mutate(date = as.Date(created)) %>% group_by(date) %>% 
        #summarise(sent = mean(sentiment_score, na.rm = TRUE)) %>% 
        #ggplot + aes(x = date, y = sent) + geom_point() + geom_line()
        #it easiert to look up certain tweets
        left_join(data_comb, by = "text") %>% 
        select(text, n, created, screenName, part_id) %>%
        order_by(part_id) %>% 
        na.omit() %>% 
        arrange(-n)

mean(tweets_count$n, na.rm = TRUE)

test_data <- data.frame(part_id = c(1:2), gender = c("female", "male"))

test <- left_join(tweets_count, test_data, by = "part_id")

test %>% group_by(part_id, gender) %>% summarize(avg = mean(n))


# Sentiment scores --------------------------------------------------------

library(rvest)
library(Unicode)

# reference website
url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"

## get emojis from website with sentiment scores

emojis_raw <- url %>%
        read_html() %>%
        html_table() %>%
        data.frame %>%
        select(-Image.twemoji., -Sentiment.bar.c.i..95..)
names(emojis_raw) <- c("char", "unicode", "occurrences", "position", "negative", "neutral", 
                       "positive", "sentiment_score", "description", "block")

# change numeric unicode to character unicode to be able to match with emoji_dict - I do not have the 
# unicode codepoints

emojis_sent <- emojis_raw %>%
        mutate(unicode = as.u_char(unicode)) %>%
        mutate(description = tolower(description)) 

str(emojis_sent)

emojis_sent <- emojis_sent %>% left_join(emoji_dict, by = "description")

new_matchto <- as.character(emojis_sent$r.encoding)
new_description <- emojis_sent$description
sentiment <- emojis_sent$sentiment_score

sentiments <- emojis_matching(data_comb$text, new_matchto, new_description, sentiment) %>%
        group_by(text) %>% 
        na.omit() %>% 
        summarize(sent_score = mean(sentiment)) #this solves the problem of losing the -1, 0, +1 scale
# because now all the sentiment scores within a single tweet are averaged instead of multiplied
        

data_comb_sent <- data_comb %>% select(text, created, part_id) %>%  # some tweets don't have sentiment scores
        left_join(sentiments, by = "text")

# rank with sentiment scores

rank_sent <- left_join(rank, emojis_sent, by = "description")

#test <- left_join(data_comb_sent, test, by = "part_id")

# this is how it looks like over time:

library(lubridate)

tweet_ger_sent %>% 
        mutate(month = month(created, label = TRUE)) %>% 
        group_by(month) %>% 
        summarise(sent = mean(sent_score, na.rm = TRUE)) %>% 
        ggplot(aes(x = month, y = sent)) + 
        geom_point() + 
        geom_line()




