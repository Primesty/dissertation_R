

### Regex cleaning double vowels and consonants

gsub('([[:alpha:]])\\1+', '\\1', text) #Problem: also removes double l - reduces every letter to one

# b, d, f, g, l, m, n, p, r, s, and t

text <- 'Halllooo süüüüüßße bäääääöööö'

text <- gsub('[ß]{2,4}','ß',text) # Eszett

text <- gsub('l{3,}','ll', text) # makes sure that double ll at the end remains

text <-gsub('(.)o{2,5}( )', '\\1o\\2', text) # check those again

text <- gsub('u{2,8}', "u", text)

text <- gsub('i{2,8}', "i", text)

text <- gsub('ü{2,8}', "ü", text)

text <- gsub('ö{2,8}', "ö", text)

text <- gsub('ä{2,8}', "ä", text)

text




gsub('(.)o{2,5}( )', '\\1o\\2', "Du bist soo doof") # at the end


# getting rid of utf-16 crap

#gsub('(/[a-z0-9].{2}/?[a-z0-9]*)+', '', "/xed/ede/e90") # that works but with slashes the other way it does not see below


#gsub('(\\\\[a-z0-9].{2}\\\\?[a-z0-9]*)+', '', "\xbd")

#str <- gsub("\\", "", "a\b\c\d\e\f", fixed=TRUE)

gsub("[^A-Za-z0-9 ]", "", "I mean totally \xed\xa0\xbd\xed\xb8\x8a") # this is the oneeeee!!!!

text2 <- "Zutaten für ein glüüüüückliches Leben\" \xed\xa0\xbc\xed\xbc\x9f\n https://www.dictum.de jaaaaa schööööön"

source("text_processing.R")

lapply(text2$text2, text_processing)

text3 <- "Postkarten fürs #coachingcafe #regensburg \xed\xa0\xbd\xed\xb8\x8aDann"

# url has to be deleted first = do other cleaning first!!!

text2 <- gsub('http\\S+\\s*', '', text2) # gets rid of https...

text2 <- gsub("[^A-Za-z0-9 üäö]", "", text2) ### this is the one for German with umlaut!!!

gsub("^\\s+|\\s+$", "", text2)

# write individual regexes for the words with letters before and after!!!

gsub('([a-z]|[A-Z])e{3,8}([a-z]{0,2})', "\\1ee\\2", "Schneeeeee") # gsub for double e

gsub('([a-z]|[A-Z])o{3,8}([a-z]{0,2})', "\\1oo\\2", "Du bist sooooo dooooof") # gsub for double o - check wortende!!!

gsub('([a-z]|[A-Z])a{3,8}([a-z]{0,2})', "\\1aa\\2", "Haaaaare") # gsub for double a

gsub('e{3,5}h{2,8}', 'eh', "seeeehhhhhhr") 


# You may remove any 1+ non-ASCII symbols with a [^ -~]+ regex: but does not work for umlaut!

gsub("[^ -~]+", "", text3)

#The pattern means:

#[^ - start of a negated character class
# -~ - a range of chars in the ASCII table between a space (decimal code 32) and a tilde (decimal code 126)
#] - end of the character class
# + - a quantifier, matching the subpattern to the left of it one or more times.



