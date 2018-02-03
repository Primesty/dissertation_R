
hashtag_density <- function(dataframe){
        require(stringr)
        require(dplyr)
        dataframe$hash <- str_detect(string = dataframe$text, pattern = "#\\w+")
        overall <- dataframe %>% group_by(part_id) %>% summarize(overall = n())
        hash_num <- dataframe %>% filter(hash == TRUE) %>% group_by(part_id) %>% summarize(hash_num = n())
        both <- left_join(overall, hash_num, by = "part_id")
        both <- both %>% mutate(hash_dens = round((both$hash_num/both$overall)*100, 2))
        
        return(both)
}


hash_extract <- function(dataframe){
        require(stringr)
        require(dplyr)
        dataframe$hash <- str_detect(string = dataframe$text, pattern = "#\\w+")
        hash_corp <- dataframe %>% filter(hash == TRUE) %>% 
                select(part_id, text, created, statusSource) %>% group_by(part_id)
        
        return(hash_corp)
}

hashtag_density_test <- function(dataframe){
        require(stringr)
        dataframe$hash <- str_detect(string = dataframe$text, pattern = "#\\w+")
        t <- table(dataframe$hash)
        
        overall <- length(dataframe$hash)
        tweets_wo_num <- t[[1]]
        tweets_w_num <- t[[2]]
        tweets_wo <- t[[1]]/length(dataframe$hash)
        tweets_w <- t[[2]]/length(dataframe$hash)
        
        print(paste0("German tweets overall (#): ", overall))
        print(paste0("Tweets with hashtags (#): ", tweets_w_num))
        print(paste0("Tweets with hastags (%): ", round(tweets_w*100, 2)))
        print(paste0("Tweets without hashtags (#): ", tweets_wo_num))
        print(paste0("Tweets without hashtags (%): ", round(tweets_wo*100, 2)))
        
        
}
