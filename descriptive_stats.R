
# Descriptive stats -------------------------------------------------------

diss_data %>% group_by(gender) %>% summarise(mean = mean(age), sd = sd(age))

sd(diss_data$age)

# independent 2-group t-test

t.test(diss_data$age~diss_data$gender, paired = FALSE) # where y is numeric and x is a binary factor

library(effsize)

cohen.d(diss_data$age ~ diss_data$gender)

dif <- abs(diss_data[diss_data$gender == "female",]$age - diss_data[diss_data$gender == "male",]$age)

hist(dif) #difference is not normally distributed...
hist(diss_data$age)

# independent 2-group Mann-Whitney U Test 
wilcox.test(diss_data$age~diss_data$gender) 
# where y is numeric and A is A binary factor

# Relationship status

diss_data %>% group_by(gender, relationship) %>% summarise(n = n())

rel <- diss_data %>% filter(relationship == "In a relationship" | relationship == "Single") %>% droplevels()

rel$gender <- as.factor(rel$gender)
rel$relationship <- as.factor(rel$relationship)

rel <- table(rel$gender, rel$relationship)

chisq.test(rel, p = 0.05) # 2x2 - effect size:phi # weird result p = 1

library(vcd)

assocstats(rel)

t2 <- table(diss_data$gender, diss_data$relationship)

fisher.test(t2)




# We do not reject H0 that gender and relationship are independent, they are thus not related in some way...

e <- diss_data %>% select(gender, edu) %>% droplevels()
e <- table(e$gender, e$edu)

fisher.test(e)
assocstats(e)


e2 <- diss_data %>% select(gender, edu2) %>% droplevels()
e2 <- table(e2$gender, e2$edu2)

fisher.test(e2)
assocstats(e2)

e3 <- diss_data %>% select(gender, employ) %>% droplevels()
e3 <- table(e3$gender, e3$employ)

fisher.test(e3)
assocstats(e3)

## Twitter statistics

diss_data %>% group_by(gender, years_twitter) %>% summarise(n = n())

diss_data %>% group_by(gender, check_twitter) %>% summarise(n = n())

diss_data %>% group_by(gender, function_hash) %>% summarise(n = n())

k <- c(46, 1, 15)
round((k/62) * 100, 1)

tw <- diss_data %>% select(gender, years_twitter) %>% droplevels()
tw <- table(tw$gender, tw$years_twitter)

fisher.test(tw)
assocstats(tw)

tw2 <- diss_data %>% select(gender, check_twitter) %>% droplevels()
tw2 <- table(tw2$gender, tw2$check_twitter)

fisher.test(tw2)
assocstats(tw2)

tw3 <- diss_data %>% select(gender, function_hash) %>% droplevels()
tw3 <- table(tw3$gender, tw3$function_hash)

fisher.test(tw3)
assocstats(tw3)

## Time on Twitter t-test


diss_data %>% group_by(gender) %>% summarise(mean = mean(time_twitter_min), 
                                             sd = sd(time_twitter_min), med = median(time_twitter_min))

mean(diss_data$time_twitter_min)
sd(diss_data$time_twitter_min)

# independent 2-group t-test


t.test(diss_data$time_twitter_min~diss_data$gender) # where y is numeric and x is a binary factor

cohen.d(diss_data$time_twitter_min ~ diss_data$gender)

# Participants personality scores

mean(diss_data$o)
sd(diss_data$o)
median(diss_data$o)

diss_data %>% group_by(gender) %>% summarise(mean = mean(o), 
                                             sd = sd(o), med = median(o))


# Anova to compare groups

diss_data_gather <- diss_data %>% gather(`e`, `a`, `c`, `n`, `o`, key = "p_type", value = "bfi10_score") %>% arrange(part_id)

diss_data_gather$p_type <- as.factor(diss_data_gather$p_type)

diss_data_gather$p_type <- factor(diss_data_gather$p_type, levels = c("e", "a", "c", "n", "o"))

fit1 <- aov(bfi10_score ~ gender + p_type, data = diss_data_gather)
fit1
summary(fit1) # significant result for gender and p_type overall BUT low effect sizes (partial omegas) >
# higher risk of type II error = false negative (H0 accepted as true but shouldn't)

plot(fit1, 1)
plot(fit1, 2)

library(car)

leveneTest(bfi10_score ~ gender, data = diss_data_gather)
leveneTest(bfi10_score ~ p_type, data = diss_data_gather)
leveneTest(bfi10_score ~ gender*p_type, data = diss_data_gather)

# From the output above we can see that the p-value is not less than the significance level of 0.05. 
# This means that there is no evidence to suggest that the variance across groups is statistically significantly different. 
# Therefore, we can assume the homogeneity of variances in the different treatment groups.


TukeyHSD(fit1)

pairwise.t.test(diss_data_gather$bfi10_score, diss_data_gather$gender, p.adjust.method = "bonferroni")
cohen.d(diss_data_gather$bfi10_score ~ diss_data_gather$gender)
pairwise.t.test(diss_data_gather$bfi10_score, diss_data_gather$p_type, p.adjust.method = "bonferroni")


library(lsr) # eta squared
library(broom)

tidy(fit1)
etaSquared(fit1, anova = TRUE)

library(sjstats)

omega_sq(fit1)

eta_sq(fit1)

## Alternative with slightly different function - results virtually the same

partialOmegas <- function(mod){
        aovMod <- mod
        if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
        sumAov     <- summary(aovMod)[[1]]
        residRow   <- nrow(sumAov)
        dfError    <- sumAov[residRow,1]
        msError    <- sumAov[residRow,3]
        nTotal     <- nrow(model.frame(aovMod))
        dfEffects  <- sumAov[1:{residRow-1},1]
        ssEffects  <- sumAov[1:{residRow-1},2]
        msEffects  <- sumAov[1:{residRow-1},3]
        partOmegas <- abs((dfEffects*(msEffects-msError)) /
                                  (ssEffects + (nTotal -dfEffects)*msError))
        names(partOmegas) <- rownames(sumAov)[1:{residRow-1}]
        partOmegas
}

partialOmegas(fit1)

## Twitter corpus stats

tweet_data_ger <- readRDS("tweet_data_ger.rds")

## Make three age groups for diss_data...

library(Hmisc)

diss_data$age_group <- cut(diss_data$age, breaks = c(18,24,35,45), 
                           labels = c("20 - 24 years", "25 - 35 years", "36 - 45 years")) 
#intervals closed on the right by default = not inlcuded in the next interval

diss_data$age_group <- as.factor(diss_data$age_group)
levels(diss_data$age_group)

diss_data %>% group_by(gender) %>% summarise(tweets = sum(tweet_num))

k <- c(2057, 10202, 7513)
round((k/19772) * 100, 1)

diss_data %>%  summarise(mean = mean(tweet_num), 
                                             sd = sd(tweet_num), med = median(tweet_num))

## Sentiment scores

sent_data <- left_join(tweet_data_ger[,c("part_id", "sent_score", "statusSource")], diss_data[,c("part_id", "age", "age_group" ,"gender", "e", "a", "c", "n", "o", "emoji_dens")], by = "part_id")

sent_data <- sent_data %>% na.omit() # 1,789 observations, 45 participants

sent_data$part_id <- as.factor(sent_data$part_id)

sent_data %>% group_by(gender, age_group) %>% summarise(mean = round(mean(sent_score),2), 
                                                        sd = round(sd(sent_score),2))
round(sd(sent_data$sent_score),2)

# ANOVA for age groups and gender

fit2 <- aov(tweet_num ~ gender + age_group, data = diss_data)

summary(fit2)

source("partial_omegas_anova.R")
partialOmegas(fit2)

TukeyHSD(fit2)

## Eg denn weil dens - male mean higher => females more informal??

# Modeling tweet number

### Poisson model produces statistically significant results
# OR NEG BINOMIAL
library(MASS)

ggplot(diss_data) + geom_histogram(aes(tweet_num)) # > count data

po <- glm(tweet_num ~ gender + age + e + a + c + n + o, data = diss_data, family = "poisson")
summary(po)

logLik(po)

tweet_num.nb <- glm.nb(tweet_num ~ gender + age, data = diss_data) # tweet num does follow a negative binomial dist.
summary(tweet_num.nb)

tidy(tweet_num.nb)

est <- cbind(Estimate = coef(tweet_num.nb), confint(tweet_num.nb))

exp(est)

logLik(tweet_num.nb) ## Fitted log-lik of NB is a lot larger/better using just one additional parameter (6 coefs and 1 theta)

library(car)

vif(tweet_num.nb) # no high variance inflation values for multicolinearity!! Good!! 

# Perform likelihood ratio test: H0:theta = Inf (poisson model) against HA: theta < Inf (neg binomial model)

library(lmtest)

lrtest(po, tweet_num.nb)

# = very significant, we can reject H0 and assume the negative binomial model is the better fit.

# AIC for NB is also a lot lower

# Pseudo R2

library(pscl)

pR2(tweet_num.nb)


# Next neg.binom model with hour of day as predictor for tweet number

tweet_ger_date <- readRDS("tweet_ger_date.rds")

library(lubridate)

tweet_ger_date <- tweet_ger_date %>% mutate(hour = hour(created))

t <- as.data.frame(tweet_ger_date %>% group_by(hour) %>% summarise(n = n()))

cor.test(t$hour, t$n)

t2 <- as.data.frame(tweet_ger_date %>% group_by(hour, gender, age, age_group) %>% summarise(n = n()))

tweets_nour.po <- glm(n ~ hour + gender + age, data = t2, family = "poisson")

summary(tweets_nour.po)

tweets_nour.nb <- glm.nb(n ~ hour + gender + age, data = t2)

summary(tweets_nour.nb)

lrtest(tweets_nour.po, tweets_nour.nb)

# again NB model is better fit

library(pscl)

pR2(tweets_nour.nb)

### Participants' other tweet measures

diss_data %>% 
        summarise(meand = mean(weil_dens, na.rm = TRUE), sd = sd(weil_dens, na.rm = TRUE))

### Table for statusSource and participant gender/age

tweet_ger_date %>% group_by(gender, age_group, statusSource) %>% summarize(n = n()) %>% top_n(5)


## Model testing

summary(gam(tweet_num ~ gender + age, data = diss_data))


