
#' Time to 100 tweets
#'
#' This function allows you to calculate the amount of time it takes a part to write 100 tweets (in days).
#' It is based on the data returned from a twitteR object and individual numerical participant ids genderated
#' for each screenName. 
#' @param dataframe A data frame object
#' @keywords Time to 100
#' @export
#' @examples
#' time_to_100_part(dataframe)

time_to_100_part <- function(dataframe){
        N = 100 # set number of observations you want to 'check'
        output <- vector("double", length(levels(dataframe$part_id))) # output vector based on number of indiv. part ids
        for(part in dataframe$part_id){
            
                output[[part]] <- as.numeric(diff(sort(as.Date(dataframe[dataframe$part_id == part,]$created))[c(1,N)]),
                                       units = "days")
        }
        
        return(output)
}

#' Time to 50 tweets
#'
#' This function allows you to calculate the amount of time it takes a part to write 50 tweets (in days).
#' It is based on the data returned from a twitteR object and individual numerical participant ids genderated
#' for each screenName. 
#' @param dataframe A data frame object
#' @keywords Time to 50
#' @export
#' @examples
#' time_to_50_part(dataframe)

time_to_50_part <- function(dataframe){
        N = 50 # set number of observations you want to 'check'
        output <- vector("double", length(levels(dataframe$part_id))) # output vector based on number of indiv. part ids
        for(part in dataframe$part_id){
                
                output[[part]] <- as.numeric(diff(sort(as.Date(dataframe[dataframe$part_id == part,]$created))[c(1,N)]),
                                             units = "days")
        }
        
        return(output)
}

#' Time to 25 tweets
#'
#' This function allows you to calculate the amount of time it takes a part to write 25 tweets (in days).
#' It is based on the data returned from a twitteR object and individual numerical participant ids genderated
#' for each screenName. 
#' @param dataframe A data frame object
#' @keywords Time to 25
#' @export
#' @examples
#' time_to_25_part(dataframe)

time_to_25_part <- function(dataframe){
        N = 25 # set number of observations you want to 'check'
        output <- vector("double", length(levels(dataframe$part_id))) # output vector based on number of indiv. part ids
        for(part in dataframe$part_id){
                
                output[[part]] <- as.numeric(diff(sort(as.Date(dataframe[dataframe$part_id == part,]$created))[c(1,N)]),
                                             units = "days")
        }
        
        return(output)
}

# data.frame(keyName=names(span), value=span, row.names=NULL) # creates data frame with screenNames (key)

#N = 100 # set N to be find difference between 1st and Nth tweet
#diff(sort(as.Date(tweet_data$created))[c(1,N)])
# Time difference of 114 days
# Breaking this down: 1) sort(as.Date(dates$date)) converts character vector to date type, 
# and arranges them in ascending order. 
# 2) [c(1,N)] subsets to find the earliest (1st) date and the Nth one following that. 
# 3) diff() calculates the difference between the two dates.
