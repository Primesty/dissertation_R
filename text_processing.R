text_processing <- (function(x) {
        
        x = gsub('http\\S+\\s*', '', x) ## Remove URLs
        
        x = gsub('\\b+RT', '', x) ## Remove RT
        
        x = gsub('\\n', " ", x) # remove \n
        
        x = gsub('( #[^# ]+?)+$', '', x) ## Remove hashtags only at the end!!!
        
        x = gsub('…', '', x) # remove …
        
        x = gsub(' ?#', " ", x) # Remove hashtag-metacharacter inside syntax for tagging...
        
        x = gsub('@\\S+', '', x) ## Remove Mentions
        
        x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
        
        x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
        
        x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
        
        x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
        
        x = gsub(' +',' ',x) ## Remove extra whitespaces
        
        x = gsub('\\b[a-zA-Z]\\b', "", x) # remove single characters that would otherwise show up as words 
        
        x = gsub("[^A-Za-z0-9 üäöéÄÜÖ]", "", x) ### remove \xed\xa0\xbd\xed\xb8\x8a patterns
        
        x = gsub('\\b amp\\b', " und", x) # Remove ampersand 'amp' replace with 'und'
        
        x = gsub('^amp\\s', "und ", x) # Remove ampersand at beginning of tweet and replace with 'und'
        
        x = gsub('ß{2,4}','ß', x) # Eszett
        
        x = gsub('l{3,}','ll', x) # makes sure that double ll at the end remains
        
        x = gsub('b{3,}','bb', x)
        
        x = gsub('d{3,}','dd', x)
        
        x = gsub('f{3,}','ff', x)
        
        x = gsub('g{3,}','gg', x)
        
        x = gsub('m{3,}','mm', x)
        
        x = gsub('n{3,}','nn', x)
        
        x = gsub('p{3,}','pp', x)
        
        x = gsub('r{3,}','rr', x)
        
        x = gsub('s{3,}','ss', x)
        
        x = gsub('t{3,}','tt', x)
        
        x = gsub('(.)o{2,}( )', '\\1o\\2', x) # o in sooooo
        
        x = gsub('(.)a{2,}( )', '\\1a\\2', x) # a in e.g. jaaaa
        
        x = gsub('([a-z]|[A-Z])e{3,8}([a-z]{0,2})', "\\1ee\\2", x) # gsub for double e
        
        x = gsub('([a-z]|[A-Z])o{3,8}([a-z]{0,2})', "\\1oo\\2", x) # gsub for double o
        
        x = gsub('([a-z]|[A-Z])a{3,8}([a-z]{0,2})', "\\1aa\\2", x) # gsub for double a
        
        x = gsub('e{3,5}h{2,8}', 'eh', x) # seeeehhr
        
        # Bavaria
        
        x = gsub('[Ww]eda', 'Wetter', x) # weather
        
        x = gsub('\\b[Aa]\\b', 'ein', x) # article
        
        x = gsub('\\b[Ee]am\\b', "ihm", x) # eam pronoun
        
        x = gsub('\\b[Ff]ei\\b', "wirklich", x) # Southern-German intensifier 'fei' = indeed
        
        x = gsub('\\b[Ee]tz\\b', "jetzt", x) # etz = Souther German abbr. for jetzt (now)
        
        x = gsub('\\<nix\\>', 'nichts', x)
        
        x = gsub('\\<hol\\>', 'hole', x)
        
        x = gsub('\\<ned\\>', 'nicht', x)
        
        x = gsub('\\<is\\>', 'ist es', x)
        
        x = gsub('\\<hab\\>', 'habe', x)
        
        x = gsub('u{2,8}', "u", x)
        
        x = gsub('i{2,8}', "i", x)
        
        x = gsub('ü{2,8}', "ü", x)
        
        x = gsub('ö{2,8}', "ö", x)
        
        x = gsub('ä{2,8}', "ä", x)
        
        x = gsub('\\b[Nn]ich\\b', 'nicht', x)
        
        x = gsub('\\b[Nn]e\\b', 'eine', x)
        
        x = gsub('\\b[Nn]em\\b', 'einem', x)
        
        x = gsub('\\b([stl])ach\\b', '\\1ag', x) # spirantization
        
        x = gsub('\\b[Ss]chee\\b', 'schön', x)
        
        x = gsub('\\b[Ii]cke?\\b', 'ich', x)
        
        x = gsub('\\bwos\\b', 'wo es', x)
        
        x = gsub('\\bhabens\\b', 'haben es', x)
        
        x = gsub('\\bwies\\b', 'wie es', x)
        
        x = gsub('\\b[Mm]a\\b', 'man', x)
        
        x = gsub('\\b[Ss]cho\\b', 'schon', x)
        
        x = gsub('\\b[Kk]enn\\b', 'kenne', x)
        
        x = gsub('\\b[Hh]alt\\b', 'halte', x)
        
        x = gsub('\\b[Ii]sch\\b', 'ich', x)
        
        x = gsub('\\b[Ww]ollt\\b', 'wollte', x)
        
        x = gsub('\\b(.*)sche\\b', '\\1che', x) # replaces words like mansche, solsche
        
        x = gsub('\\b[Ss]isch\\b', 'sich', x)
        
        x = gsub('\\b(.*)[ae]scht\\b', '\\1st', x) # s-palatalisierung
        
        x = gsub('\\bnischt\\b', 'nicht', x) # s-palatalisierung
        
        x = gsub('\\bge?(.*)\\>', 'ge\\1', x) # e-syncope e.g. gsehn, gschehn, gfahrn.
     
        x = gsub('(.*)([^eißouan])n\\>', '\\1\\2en', x)
        
        x = gsub('[Hh]abt', 'habet', x)
        
})

text_processing_with_punct <- (function(x) {
        
        x = gsub('http\\S+\\s*', '', x) ## Remove URLs
        
        x = gsub('\\b+RT', '', x) ## Remove RT
        
        x = gsub('\\n', " ", x) # remove \n
        
        x = gsub('( #[^# ]+?)+$', '', x) ## Remove hashtags only at the end!!!
        
        x = gsub('…', '', x) # remove …
        
        x = gsub(' ?#', " ", x) # Remove hashtag-metacharacter inside syntax for tagging...
        
        x = gsub('@\\S+', '', x) ## Remove Mentions
        
        x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
        
        x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
        
        x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
        
        x = gsub(' +',' ',x) ## Remove extra whitespaces
        
        x = gsub('\\b[a-zA-Z]\\b', "", x) # remove single characters that would otherwise show up as words 
        
        x = gsub("[^A-Za-z0-9 üäöéÄÜÖ!.,:;?]", "", x) ### remove \xed\xa0\xbd\xed\xb8\x8a patterns
        
        x = gsub('\\b amp\\b', " und", x) # Remove ampersand 'amp'

        x = gsub('^amp\\s', "und ", x)
        
        x = gsub('ß{2,4}','ß', x) # Eszett
        
        x = gsub('l{3,}','ll', x) # makes sure that double ll at the end remains
        
        x = gsub('b{3,}','bb', x)
        
        x = gsub('d{3,}','dd', x)
        
        x = gsub('f{3,}','ff', x)
        
        x = gsub('g{3,}','gg', x)
        
        x = gsub('m{3,}','mm', x)
        
        x = gsub('n{3,}','nn', x)
        
        x = gsub('p{3,}','pp', x)
        
        x = gsub('r{3,}','rr', x)
        
        x = gsub('s{3,}','ss', x)
        
        x = gsub('t{3,}','tt', x)
        
        x = gsub('(.)o{2,}( )', '\\1o\\2', x) # o in sooooo
        
        x = gsub('(.)a{2,}( )', '\\1a\\2', x) # a in e.g. jaaaa
        
        x = gsub('([a-z]|[A-Z])e{3,8}([a-z]{0,2})', "\\1ee\\2", x) # gsub for double e
        
        x = gsub('([a-z]|[A-Z])o{3,8}([a-z]{0,2})', "\\1oo\\2", x) # gsub for double o
        
        x = gsub('([a-z]|[A-Z])a{3,8}([a-z]{0,2})', "\\1aa\\2", x) # gsub for double a
        
        x = gsub('e{3,5}h{2,8}', 'eh', x) # seeeehhr
        
        # Bavaria
        
        x = gsub('[Ww]eda', 'Wetter', x) # weather
        
        x = gsub('\\b[Aa]\\b', 'ein', x) # article
        
        x = gsub('\\b[Ee]am\\b', "ihm", x) # eam pronoun
        
        x = gsub('\\b[Ff]ei\\b', "wirklich", x) # Southern-German intensifier 'fei' = indeed
        
        x = gsub('\\b[Ee]tz\\b', "jetzt", x) # etz = Souther German abbr. for jetzt (now)
        
        x = gsub('\\<nix\\>', 'nichts', x)
        
        x = gsub('\\<hol\\>', 'hole', x)
        
        x = gsub('\\<ned\\>', 'nicht', x)
        
        x = gsub('\\<is\\>', 'ist es', x)
        
        x = gsub('\\<hab\\>', 'habe', x)
        
        x = gsub('u{2,8}', "u", x)
        
        x = gsub('i{2,8}', "i", x)
        
        x = gsub('ü{2,8}', "ü", x)
        
        x = gsub('ö{2,8}', "ö", x)
        
        x = gsub('ä{2,8}', "ä", x)
        
        x = gsub('\\b[Nn]ich\\b', 'nicht', x)
        
        x = gsub('\\b[Nn]e\\b', 'eine', x)
        
        x = gsub('\\b[Nn]em\\b', 'einem', x)
        
        x = gsub('\\b([stl])ach\\b', '\\1ag', x) # spirantization
        
        x = gsub('\\b[Ss]chee\\b', 'schön', x)
        
        x = gsub('\\b[Ii]cke?\\b', 'ich', x)
        
        x = gsub('\\bwos\\b', 'wo es', x)
        
        x = gsub('\\bhabens\\b', 'haben es', x)
        
        x = gsub('\\bwies\\b', 'wie es', x)
        
        x = gsub('\\b[Mm]a\\b', 'man', x)
        
        x = gsub('\\b[Ss]cho\\b', 'schon', x)
        
        x = gsub('\\b[Kk]enn\\b', 'kenne', x)
        
        x = gsub('\\b[Hh]alt\\b', 'halte', x)
        
        x = gsub('\\b[Ii]sch\\b', 'ich', x)
        
        x = gsub('\\b[Ww]ollt\\b', 'wollte', x)
        
        x = gsub('\\b(.*)sche\\b', '\\1che', x) # replaces words like mansche, solsche
        
        x = gsub('\\b[Ss]isch\\b', 'sich', x)
        
        x = gsub('\\b([^d].*)rch\\b', '\\1rg', x) # replaces rch in cities like Marburch
        
        x = gsub('\\b(.*)[ae]scht\\b', '\\1st', x) # s-palatalisierung
        
        x = gsub('\\bnischt\\b', 'nicht', x) # s-palatalisierung
        
        x = gsub('\\bge?(.*)\\>', 'ge\\1', x) # e-syncope e.g. gsehn, gschehn, gfahrn.
        
        x = gsub('(.*)([^eißouan])n\\>', '\\1\\2en', x)
        
        x = gsub('[Hh]abt', 'habet', x)
        
})