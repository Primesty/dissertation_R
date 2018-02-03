
# Language recognition ----------------------------------------------------

## Google's cldr

library(devtools)

devtools::install_version("cldr",version="1.1.0") 

library(cldr)

demo(cldr)

ger <- "Ich habe keine Zeit"

# You can input the data from a dataframe from the twitteR package - tweet_data$text

detect1 <- detectLanguage(tweet_data$text, isPlainText = TRUE, 
               pickSummaryLanguage = TRUE, 
               removeWeakMatches = FALSE)

detect2 <- detectLanguage(tweet_data2$text, isPlainText = TRUE, 
               pickSummaryLanguage = TRUE, 
               removeWeakMatches = FALSE)

## add a column to tweet_data with language

library(dplyr)

lg <- detect1 %>% select(candidateLanguage1)
lg2 <- detect2 %>% select(candidateLanguage1)

tweet_data$lg <- lg
tweet_data2$lg <- lg2

documents <- c("Rechtdoor gaan, dan naar rechts.",
               "Kemal Kılıçdaroğlu Doğan TV Center'da",
               "I live in the countryside",
               "Questa frase non è scritta in Napoletano.",
               "Das ist ein deutscher satz.",
               "La vie est magnifique",
               "El jugador está predispuesto a que será un partido complicado.",
               "Καιρό έχουμε να τα πούμε!",
               "Jar kan ikke snakke Norsk")

detectLanguage(documents = documents, isPlainText = TRUE, 
               pickSummaryLanguage = TRUE, 
               removeWeakMatches = FALSE)

# Textcat -----------------------------------------------------------------

install.packages("textcat")
library(textcat)

textcat(x)
textcat("test.txt", p = )


## Okay textcat sucks!
