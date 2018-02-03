

#' Emoji density function
#'
#' Emoji density function
#' @param dataframe A data frame.
#' @keywords Emoji test function
#' @export
#' @examples
#' emoji_density(dataframe)

emoji_density <- function(dataframe){
        require(stringr)
        require(dplyr)
        dataframe$emoji <- gsub("<e2><80><a6>", '', dataframe$emoji) # replaces (...) with nothing
        dataframe$emo <- str_detect(string = dataframe$emoji, pattern =  "(<\\w{2}>){3,8}") 
        overall <- dataframe %>% group_by(part_id) %>% summarize(overall = n())
        emo_num <- dataframe %>% filter(emo == TRUE) %>% group_by(part_id) %>% summarize(emo_num = n())
        both <- left_join(overall, emo_num, by = "part_id")
        both <- both %>% mutate(emoji_dens = round((both$emo_num/both$overall)*100, 2))
        
        return(both)
}

#' Test function
#'
#' Emoji test function
#' @param dataframe A data frame.
#' @keywords Emoji test function
#' @export
#' @examples
#' emoji_density_test(dataframe)
#' 
emoji_density_test <- function(dataframe){
        library(stringr)
        dataframe$emoji <- gsub("<e2><80><a6>", '', dataframe$emoji) # replaces (...) with nothing
        tweet_ger$emo <- str_detect(string = dataframe$emoji, pattern =  "(<\\w{2}>){3,8}")   
        t_emo <- table(dataframe$emo)
        
        overall <- length(dataframe$emo)
        tweets_wo_emo_num <- t_emo[[1]]
        tweets_w_emo_num <- t_emo[[2]]
        tweets_wo_emo <- t_emo[[1]]/length(dataframe$emo)
        tweets_w_emo <- t_emo[[2]]/length(dataframe$emo)
        
        print(paste0("German tweets overall (#): ", overall))
        print(paste0("Tweets with emojis (#): ", tweets_w_emo_num))
        print(paste0("Tweets with emojis (%): ", round(tweets_w_emo*100, 2)))
        print(paste0("Tweets without emojis (#): ", tweets_wo_emo_num ))
        print(paste0("Tweets without emojis (%): ", round(tweets_wo_emo*100, 2)))
        
        
}

# emo is a true/false vector created by string_detect() indicating if there is an emoji present in a tweet


#<c3><b6> = ö <e2><80><a6> = ... <c3><bc> = ü <c3><a4> = ä

# t <- str_extract_all(string = tweet_ger$emoji, pattern = "(<\\w{2}>){3,6}")

# t <- t[-grep("<e2><80><a6>", t)] # removes ...