

# Matching sent score with BAWL_R ------------------------------------------

bawl <- read.csv("bawl_r.csv", header = TRUE, sep = ",")

names(bawl) <- tolower(names(bawl))

bawl_sub <- bawl %>% select(word_lower, emo_mean, arousal_mean, image_mean)

str(bawl_sub)

bawl_sub$word_lower <- as.character(bawl_sub$word_lower)

tweet_sub <- tweet_data_ger %>% select(part_id, created, clean_w_punct, sent_score) %>% na.omit() %>% droplevels()

str(tweet_sub)



# Matching ----------------------------------------------------------------

library(stringr)
library(purrr)
library(tidytext)
library(dplyr)

df <- bawl_sub$word_lower %>% split(bawl_sub$word_lower) %>% 
        map( ~ str_detect(tweet_sub$clean_w_punct, .x)) %>% # .x comes from the purrr package
        as_data_frame()

text <- data.frame(part_id = tweet_sub$part_id, text = tweet_sub$clean_w_punct, sent_score = tweet_sub$sent_score) #somehow also include part_id for matching later...

text$text <- as.character(text$text)

matches <- str_extract(text$text, paste0("\\s", bawl_sub$word_lower, "\\s", sep = "", collapse = "|")) # with white-space exact matches only

matches <- gsub(pattern = "[[:space:]]", "", matches) #delete white spaces

matches <- as.data.frame(matches)
matches$matches <- unlist(matches$matches)


combined <- cbind(text, matches)

combined <- combined %>% na.omit()

str(combined)

bawl_sub$word_lower <- as.factor(bawl_sub$word_lower)

combined <- left_join(combined, bawl_sub[, c("word_lower", "emo_mean")], by = c("matches" = "word_lower"))

combined


## Normalize sent_score and emo mean

combined <- combined %>% mutate(sent_z = scale(sent_score, center = TRUE, scale = TRUE),
                                emo_z = scale(emo_mean, center = TRUE, scale = TRUE))


combined_sum <- combined %>% group_by(part_id) %>% summarize(sent = mean(sent_z), emo = mean(emo_z)) %>% 
        ungroup()

combined_sum <- left_join(combined_sum, diss_data[, c("part_id", "gender")], by = "part_id")

# Differences

combined_sum %>% mutate(diff = abs(sent - emo)) %>% group_by(gender) %>% summarise(mean_diff = mean(diff))

library(ggplot2)

ggplot(combined_sum, aes(gender, sent, fill = gender)) + 
        geom_boxplot(notch = TRUE, outlier.colour = "black", outlier.shape = 21, outlier.fill = "red") +
        coord_flip() +
        scale_fill_manual(values = c("steelblue", "tomato"), guide = FALSE)


combined_sum_l <- combined_sum %>% gather(`sent`, `emo`, key = var, value = score)

combined_sum_l$gender <- relevel(combined_sum_l$gender, ref = 2)

t <- ggplot(combined_sum_l, aes(var, score, fill = gender)) + geom_boxplot()        

library(plotly)

saveRDS(combined_sum_l, file = "combined_sum_l.rds")

ggplotly(t) %>% layout(boxmode = "group")

plot_ly(combined_sum_l, x = ~var, y = ~score, color = ~gender, 
        colors = c("tomato", "steelblue"), type = "box") %>% 
        layout(boxmode = "group", xaxis = list(title = "Variable", type = "category", text = c("t1", "t2")), yaxis = list(title = "Z_Scores"))
