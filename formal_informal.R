
# Percentages for weil/denn/da --------------------------------------------


# Denn/da formal vs. weil informal

# da excluded because can also mean 'dort' - location

formal_informal <- function(dataframe){
        require(stringr)
        require(plyr)
        require(dplyr)

        dataframe$denn <- str_detect(string = dataframe$clean, pattern = 'denn')
        dataframe$weil <- str_detect(string = dataframe$clean, pattern = 'weil')
        
        overall <- dataframe %>% group_by(part_id) %>% summarize(overall = n())
        denn <- dataframe %>% filter(denn == TRUE) %>% group_by(part_id) %>% summarize(denn = n())
        weil <- dataframe %>% filter(weil == TRUE) %>% group_by(part_id) %>% summarize(weil = n())
        
        all <- join_all(dfs = list(overall, denn, weil), by = "part_id", type = "full")
        
        denn_dens <- round((all$denn/all$overall)*100, 2)
        weil_dens <- round((all$weil/all$overall)*100, 2)
        
        formal_informal <- data.frame(part_id = levels(dataframe$part_id), denn_dens = denn_dens,  weil_dens = weil_dens)
        
        return(formal_informal)
}