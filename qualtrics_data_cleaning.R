
# Cleaning data from Qualtrics --------------------------------------------

library(tidyverse)
library(forcats)

# Data collection on Qualtrics ceased 5/19 at 9am EST


# diss_numbers <- read.csv("diss_numbers.csv", header = TRUE)

diss_text <- read.csv("diss_text.csv", header = TRUE) # could stringsAsFactors = FALSE to prevent age
#from becoming a factor variable

# Delete first two rows - useless

diss_text <- diss_text[-c(1:2),]

diss_text <- diss_text %>% filter(!Q6 == "Nein")


# Select variables for analysis -------------------------------------------

diss_sub <- diss_text %>% select(Finished, Q5_1:Q9_5, Q25, Q26, Q27, Q28, Q30, Q11, 
                                 Q12, Q14, Q15, Q16, Q34, Q19, Q33, Q29, Q32, Q35) %>% 
        rename(years_twitter = Q26, check_twitter = Q27,
               function_hash = Q30, gender = Q11,
               same_city = Q15, relationship = Q34,
               citizenship = Q19, native_lang = Q33,
               edu = Q29, edu2 = Q32, employ = Q35,
               item1 = Q5_1, item2 = Q5_2,
               item3 = Q5_3, item4 = Q5_4,
               item5 = Q5_5, item6 = Q9_1,
               item7 = Q9_2, item8 = Q9_3,
               item9 = Q9_4, item10 = Q9_5,
               screenName = Q25, time_twitter_min = Q28,
               age = Q12, zip = Q14, alt_zip = Q16)


# Data cleaning -----------------------------------------------------------


# 1 Extract participants who completed the survey

diss_sub <- diss_sub %>% filter(Finished == "True")

diss_sub <- diss_sub %>% droplevels() # drop factors with 0 entries

str(diss_sub)

# 2 Clean up screenNames

diss_sub$screenName <- gsub('@', '', diss_sub$screenName)

diss_sub$screenName <- gsub("^[[:space:]]*","", diss_sub$screenName) ## Remove leading whitespaces

diss_sub$screenName <-  gsub("[[:space:]]*$","", diss_sub$screenName) ## Remove trailing whitespaces

diss_sub$screenName <- tolower(diss_sub$screenName)

# 3 Combine with hand-checked Twitter accounds

good_tweets <- read.csv("screen_name_validity.csv", header = TRUE)

head(good_tweets)
str(good_tweets)

table(good_tweets$valid.0...no..1...yes.) # = 75 valid accounts, 161 total out of 202 submitted

diss_sub <- diss_sub %>% mutate(screenName = fct_recode(screenName, "larsihasi_" = "larsi",
                                                        "darling110" = "ilonadarling110"))

good_tweets$screenName <- tolower(good_tweets$screenName)

diss_sub <- left_join(diss_sub, good_tweets, by = "screenName")


# Select verified accounts ------------------------------------------------

diss_sub <- diss_sub %>% na.omit %>% filter(valid.0...no..1...yes. == 1)

# Delete unnecessary columns

diss_sub$changes <- NULL
diss_sub$Finished <- NULL
diss_sub$valid.0...no..1...yes. <- NULL

## Ultimately 73 verified accounts out of 161 accounts submitted


# Variable recoding (factor>numeric) etc ----------------------------------

str(diss_sub)

# 1 recode personality stuff to factor from text

# Positive Items

diss_sub <- diss_sub %>% mutate(item1 = fct_recode(item1, "1" = "triff überhaupt nicht zu",
                                                   "2" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "4" = "trifft eher zu",
                                                   "5" = "trifft voll und ganz zu"))

diss_sub <- diss_sub %>% mutate(item2 = fct_recode(item2, "1" = "triff überhaupt nicht zu",
                                                   "2" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "4" = "trifft eher zu",
                                                   "5" = "trifft voll und ganz zu"))

diss_sub <- diss_sub %>% mutate(item3 = fct_recode(item3, "1" = "triff überhaupt nicht zu",
                                                   "2" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "4" = "trifft eher zu",
                                                   "5" = "trifft voll und ganz zu"))

diss_sub <- diss_sub %>% mutate(item4 = fct_recode(item4, "1" = "triff überhaupt nicht zu",
                                                   "2" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "4" = "trifft eher zu",
                                                   "5" = "trifft voll und ganz zu"))

diss_sub <- diss_sub %>% mutate(item5 = fct_recode(item5, "1" = "triff überhaupt nicht zu",
                                                   "2" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "4" = "trifft eher zu",
                                                   "5" = "trifft voll und ganz zu"))

# Negative items are recoded in reverse order

diss_sub <- diss_sub %>% mutate(item6 = fct_recode(item6, "5" = "triff überhaupt nicht zu",
                                                   "4" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "2" = "trifft eher zu",
                                                   "1" = "trifft voll und ganz zu"))

diss_sub <- diss_sub %>% mutate(item7 = fct_recode(item7, "5" = "triff überhaupt nicht zu",
                                                   "4" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "2" = "trifft eher zu",
                                                   "1" = "trifft voll und ganz zu"))

diss_sub <- diss_sub %>% mutate(item8 = fct_recode(item8, "5" = "triff überhaupt nicht zu",
                                                   "4" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "2" = "trifft eher zu",
                                                   "1" = "trifft voll und ganz zu"))

diss_sub <- diss_sub %>% mutate(item9 = fct_recode(item9, "5" = "triff überhaupt nicht zu",
                                                   "4" = "trifft eher nicht zu",
                                                   "3" = "teils/teils",
                                                   "2" = "trifft eher zu",
                                                   "1" = "trifft voll und ganz zu"))

diss_sub <- diss_sub %>% mutate(item10 = fct_recode(item10, "5" = "triff überhaupt nicht zu",
                                                    "4" = "trifft eher nicht zu",
                                                    "3" = "teils/teils",
                                                    "2" = "trifft eher zu",
                                                    "1" = "trifft voll und ganz zu"))


# 2 Recode other variables

diss_sub <- diss_sub %>% mutate(item1 = as.character(item1),item2 = as.character(item2),
                                item3 = as.character(item3), item4 = as.character(item4),
                                item5 = as.character(item5), item6 = as.character(item6),
                                item7 = as.character(item7), item8 = as.character(item8),
                                item9 = as.character(item9), item10 = as.character(item10),
                                time_twitter_min = as.numeric(time_twitter_min),
                                age = as.character(age), zip = as.character(zip),
                                alt_zip = as.character(alt_zip)) %>% 
        mutate(age = as.numeric(age),
               item1 = as.numeric(item1),item2 = as.numeric(item2),
               item3 = as.numeric(item3), item4 = as.numeric(item4),
               item5 = as.numeric(item5), item6 = as.numeric(item6),
               item7 = as.numeric(item7), item8 = as.numeric(item8),
               item9 = as.numeric(item9), item10 = as.numeric(item10))

# Since I did not set stringsAsFactors = FALSE when importing the data, the age and item variable was converted
# to a factor. The values were mapped to integers and when converting to numeric, R converts the integer
# and not the value, which is why we have to convert to character first.

str(diss_sub)

# 3 Create personality scores from individual item scores -----------------------------------------------

# http://zis.gesis.org/skala/Rammstedt-Kemper-Klein-Beierlein-Kovaleva-Big-Five-Inventory-(BFI-10)

# The items were coded according to Rammstedt, 2007 (see also link above)

# Items 1(+) and 10(-) = agreeableness
# Items 2(+) and 6(-) = extraversion
# Items 3(+) and 7(-) = conscientiousness
# Items 4(+) and 8(-) = neuroticism
# Items 5(+) and 9(-) = openness

diss_sub <- diss_sub %>% mutate(a = (item1 + item10)/2,
                                e = (item2 + item6)/2,
                                c = (item3 + item7)/2,
                                n = (item4 + item8)/2,
                                o = (item5 + item9)/2)

range(diss_sub$a)

# 4 Recode German factors to English

diss_sub <- diss_sub %>% mutate(years_twitter = fct_recode(years_twitter, "1-2 years" = "1-2 Jahre",
                                                "2-3 years" = "2 -3 Jahre",
                                                "More than 3 years" = "Mehr als 3 Jahre",
                                                "Less than a year" = "Weniger als ein Jahr"),
                   check_twitter = fct_recode(check_twitter, "Once per day" = "Einmal am Tag",
                                              "Several times per day" = "Mehrmals taeglich",
                                              "Very frequently" = "Sehr oft am Tag",
                                              "Very frequently" = "Staendig",
                                              "Less than once per day" = "Weniger als einmal pro Tag"),
                   function_hash = fct_recode(function_hash, "tag" = "Tag (um Tweets suchbar zu machen und zu organisieren)",
                                              "comment" = "Kommentar (auch als ganzer Satz)",
                                              "both" = "Beides"),
                   gender = fct_recode(gender, "male" = "Männlich", "female" = "Weiblich"),
                   same_city = fct_recode(same_city, "yes" = "Ja", "no" = "Nein"),
                   relationship = fct_recode(relationship, "Married" = "Verheiratet",
                                             "Divorced" = "Geschieden",
                                             "In a relationship" = "In einer Beziehung",
                                             "Single" = "Single", "Widowed" = "Verwitwet"),
                   citizenship = fct_recode(citizenship, "German" = "Deutsch",
                                            "Not German" = "Nicht deutsch"),
                   native_lang = fct_recode(native_lang, "German" = "Deutsch",
                                            "Croatian" = "Kroatisch", "Spanish" = "Spanisch",
                                            "Dutch" = "Niederlaendisch", "Russian" = "Russisch",
                                            "Other" = "Andere"),
                   edu = fct_recode(edu, "High school (Abitur)" = "Allgemeine Hochschulreife (Abitur)",
                                    "High school (FOS/BOS)" = "FOS/BOS",
                                    "Lowest tier (Hauptschule)" = "Hauptschulabschluss",
                                    "Mid tier - secondary (Realschule)" = "Realschulabschluss"),
                   edu2 = fct_recode(edu2, "Apprenticeship (vocational training)" = "Abgeschlossene Lehre/Berufsschule",
                                     "Univeristy of applied sciences" = "Fachhochschulabschluß",
                                     "University" = "Hochschulabschluß",
                                     "No degree" = "Kein Abschluss"),
                   employ = fct_recode(employ, "Unemployed" = "Arbeitslos, Kurzarbeit",
                                       "Part time work (~20 hrs/week)" = "Erwerbstätig, Teilzeit ~20 Std./Woche",
                                       "Full time work (~40 hrs/week)" = "Erwerbstätig, Vollzeit ~40 Std./Woche",
                                       "Student" = "Schüler(in)",
                                       "Student" = "Student(in)",
                                       "Retired" = "Rentner(in)/Pensionär(in)"))

saveRDS(diss_sub, file = "diss_sub.rds")

str(diss_sub)


# 5 Get rid of too young too old --------------------------------------------

range(diss_sub$age)

diss_sub <- diss_sub %>% filter(age >= 18 & age <= 45) # defines age bracket

# 6 Get rid of individual items

diss_sub <- diss_sub %>% select(-(item1:item10))

# 7 Bring personality scores to front

diss_sub <- diss_sub %>% select(e, a, c, n, o, everything())

# 8 Filter out non-Germans --------------------------------------------------

diss_sub <- diss_sub %>% filter(citizenship == "German")

# Replace zip-codes with origin zip codes ------------------------

# Use primary zip codes to show from where participants are tweeting - maybe second map with part
# movement...




saveRDS(diss_sub, file = "diss_sub.rds")

# Summary stats -----------------------------------------------------------

summary(diss_sub)

str(diss_sub)

table(diss_sub$gender, diss_sub$function_hash)

# Run chi-sq to see if siginificant difference between male/female numbers...

diss_sub %>% group_by(gender, function_hash) %>% summarize(n = n())


# 9 Create participant IDs not until combined with twitter data --------------------------------------------------

diss_sub <- diss_sub %>% mutate(part_id = as.numeric(interaction(screenName, drop = TRUE)))

# alternative

diss_sub <- transform(diss_sub, part_id = as.numeric(interaction(screenName, drop = TRUE)))

# 10 order by participant id

diss_sub <- diss_sub %>% arrange(part_id)

# 11 Bring part_id and screenName to front

diss_sub <- diss_sub %>% select(part_id, screenName, everything())