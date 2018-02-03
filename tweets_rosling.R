
# Rosling chart - tweets --------------------------------------------------

library(googleVis)
library(lubridate)

tweet_ger_date <- tweet_ger_date %>% mutate(year = year(created))

time <- tweet_ger_date %>% group_by(part_id, gender, year, age) %>% summarise(n = n())

gender_tweet_motion <- gvisMotionChart(time, idvar="part_id", 
                        timevar="year", 
                        xvar = "age",
                        yvar = "n",
                        colorvar = "gender",
                        chartid = "GenderTweetsAge1")


plot(gender_tweet_motion)

print(gender_tweet_motion, tag = "chart", file="GenderTweetsAge1.html")
