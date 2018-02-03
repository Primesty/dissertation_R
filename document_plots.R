
# Extracting plots --------------------------------------------------------

library(ggplot2)
library(ggpubr)

source("theme_matt.R")

part_gender_age <- ggplot(diss_data) + geom_boxplot(aes(gender, age, fill = gender), notch = TRUE) + 
        coord_flip() + 
        scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
        theme_matt() + 
        scale_fill_manual(values = c(female = "tomato", male = "steelblue"), guide = FALSE) +
        xlab("Gender") +
        ylab("Age [years]")


ggsave(plot = part_gender_age, filename = "part_gender_age.png", device = "png", width = 12, height = 8, units = "cm")

part_gender_age2 <- ggplot(diss_data) + geom_boxplot(aes(gender, age, fill = gender), notch = TRUE) + 
        coord_flip() + 
        scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
        theme_matt() + 
        scale_fill_manual(values = c(female = "tomato", male = "steelblue"), guide = FALSE) +
        xlab("Gender") +
        ylab("Age [years]") +
        stat_summary(geom = "text", fun.y = mean,
                     aes(gender, age, label = sprintf("%1.2f", ..y..)), 
                     position = position_nudge(x = .06, y = -1.4), size = 2.5) +
        stat_summary(mapping = aes(gender, age, fill = gender), 
                     dfun.y = mean, colour="orange", geom="point", shape=16, size=2, 
                     position = position_dodge(width = 0.75))

ggsave(plot = part_gender_age2, filename = "part_gender_age2.png", device = "png", 
       width = 12, height = 8, units = "cm")

library(plotly)

ggplotly(part_gender_age2, tooltip = TRUE, dynamicTicks = TRUE, layerData = TRUE)


part_gender_age_gold <- ggplot(diss_data) + geom_boxplot(aes(gender, age, fill = gender), notch = TRUE) + 
        coord_flip() + 
        scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
        theme_matt() + 
        theme(legend.title = element_text(size = 9), axis.title = element_text(size = 9), 
              axis.text = element_text(size = 9, color = "black")) +
        scale_fill_manual(values = c(female = "rosybrown1", male = "steelblue"), guide = FALSE) +
        xlab("Gender") +
        ylab("Age [years]") +
        stat_summary(geom="text", fun.y = quantile,
                     aes(gender, age, label=sprintf("%1.0f", ..y..)), # the %1.1f is floating point notation for printing 
                     #(..y.. refers to value calculated by stat summary)
                     position = position_nudge(x = 0.5), size = 3) +
        stat_summary(geom = "text", fun.y = mean,
                     aes(gender, age, label = sprintf("%1.2f", ..y..)), 
                     position = position_nudge(x = .06, y = -1.4), size = 2.5) +
        stat_summary(mapping = aes(gender, age, fill = gender), 
                     dfun.y = mean, colour="orange", geom="point", shape=16, size=2, 
                     position = position_dodge(width = 0.75)) +
        labs(caption = expression(paste(italic(bar(x)), " = dot"))) +
        theme(plot.caption = element_text(hjust = 0.95, size = 8))

ggsave(plot = part_gender_age_gold, filename = "part_gender_age_gold.png", device = "png", 
       width = 12, height = 12*0.618, units = "cm") #golden ratio!!






age_dist <- ggplot(diss_data) + geom_histogram(aes(age, ..density..), col = "lightgrey", binwidth = 30, alpha = 0) +
        geom_density(aes(x = age, y = ..density..),col = "steelblue", 
                     lwd = 1, fill = "steelblue", alpha = 0.5) +
        stat_function(fun = dnorm, color = "goldenrod", lwd = 1, args = list(mean = mean(diss_data$age), sd = sd(diss_data$age))) +
        theme_matt() +
        xlab("Age") +
        ylab("Density")

library(gridExtra)

grid.arrange(part_gender_age, age_dist, ncol =2)

## Edu, edu2, and employ - sankey-diagram

diss_data_edu <- diss_data %>% select(gender, edu, edu2, employ)

library(forcats)

diss_data_edu <- diss_data_edu %>% mutate(edu = fct_recode(edu, "High school\n(Abitur/FOS/BOS)" = "High school (FOS/BOS)",
                                                           "High school\n(Abitur/FOS/BOS)" = "High school (Abitur)",
                                                           "Mid tier -\n secondary (Realschule)" = "Mid tier - secondary (Realschule)"
                                                           )) %>% 
        droplevels()

diss_data_edu <- diss_data_edu %>% mutate(edu2 = fct_recode(edu2, "Apprenticeship\n(vocational training)" = "Apprenticeship (vocational training)",
                                                           "University of\n applied sciences" = "University of applied sciences")) %>% 
        droplevels()

diss_data_edu <- diss_data_edu %>% mutate(employ = fct_recode(employ, "Full time" = "Full time work (~40 hrs/week)",
                                                           "Part time" = "Part time work (~20 hrs/week)")) %>% 
        droplevels()

diss_data_edu <-  diss_data_edu %>% mutate(edu = fct_relevel(edu,
                                                             "Lowest tier (Hauptschule)",
                                                             "Mid tier -\n secondary (Realschule)",
                                                             "High school\n(Abitur/FOS/BOS)"
))

diss_data_edu <- diss_data_edu %>% group_by(gender, edu, edu2, employ) %>% summarise(n = n())


diss_data_edu$gender <- ifelse(diss_data_edu$gender == "female", "Female", "Male")

library(alluvial)

png(filename = "edu_sankey.png", height = 16*0.618, width = 16, units = "cm", res = 450)
alluvial(diss_data_edu[,1:4], freq = diss_data_edu$n, col = ifelse(diss_data_edu$gender == "Male", "steelblue", "tomato"),
         alpha = 0.5, axis_labels = c("Gender", "Education1", "Education2", "Employment"), cex = 0.8, cex.axis = 0.9, 
         border = ifelse(diss_data_edu$gender == "Male", "steelblue", "rosybrown1"), blocks = TRUE, cw = 0.13)
dev.off()

## With ggalluvial

library(ggplot2)
library(ggalluvial)
library(ggrepel)

edu_ggalluvial <- ggplot(diss_data_edu,
       aes(weight = n, axis1 = gender, axis2 = edu, axis3 = edu2, axis4 = employ)) +
        geom_alluvium(aes(fill = gender, col = gender), width = 1/12) +
        geom_stratum(width = 1/12, fill = "white", color = "black") +
        scale_fill_manual(values = c("rosybrown1", "steelblue"), guide = FALSE) +
        scale_color_manual(values = c("rosybrown1", "steelblue")) +
        geom_label_repel(stat = "stratum", label.strata = TRUE, size = 3, alpha = 1, box.padding = .2) +
        scale_x_continuous(breaks = 1:4, labels = c("Gender", "Education 1", "Education 2", "Employment")) +
        theme(plot.background = element_blank(), panel.background = element_blank(), axis.text.y = element_blank(),
              axis.ticks.y = element_blank(), legend.position = "none")

ggsave(edu_ggalluvial, filename = "edu_ggalluvial.png", device = "png", height = 18*0.618, width = 18, units = "cm", dpi = 600)


## Participants twitter habits maybe with age groups??

part_gender_time_twitter <- ggplot(diss_data) + geom_boxplot(aes(gender, time_twitter_min, fill = gender), notch = TRUE) + 
        coord_flip() + 
        scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
        theme_matt() + 
        theme(
              axis.title = element_text(size = 9), 
              axis.text = element_text(size = 9, color = "black")) +
        scale_fill_manual(values = c(female = "rosybrown1", male = "steelblue"), guide = FALSE) +
        xlab("Gender") +
        ylab("Time on Twitter [min]") +
        stat_summary(geom="text", fun.y = quantile,
                     aes(gender, time_twitter_min, label=sprintf("%1.0f", ..y..)), # the %1.1f is floating point notation for printing 
                     #(..y.. refers to value calculated by stat summary)
                     position = position_nudge(x = 0.5), size = 3) +
        stat_summary(geom = "text", fun.y = mean,
                     aes(gender, time_twitter_min, label = sprintf("%1.2f", ..y..)), 
                     position = position_nudge(x = .06, y = -2.5), size = 2.5) +
        stat_summary(mapping = aes(gender, time_twitter_min, fill = gender), 
                     fun.y = mean, color="orange", geom="point", shape=16, size=2, 
                     position = position_dodge(width = 0.75)) +
        labs(caption = expression(paste(italic(bar(x)), " = dot"))) +
        theme(plot.caption = element_text(hjust = 0.95, size = 8))

ggsave(plot = part_gender_time_twitter, filename = "part_gender_time_twitter.png", device = "png", 
       width = 12, height = 12*0.618, units = "cm") #golden ratio!!

## Participants' personality scores...

library(tidyverse)
library(forcats)

diss_data_gather <- diss_data %>% gather(`e`, `a`, `c`, `n`, `o`, key = "p_type", value = "bfi10_score") %>% arrange(part_id)

diss_data_gather$p_type <- as.factor(diss_data_gather$p_type)

diss_data_gather$p_type <- factor(diss_data_gather$p_type, levels = c("e", "a", "c", "n", "o"))

part_gender_bfi10 <- ggplot(diss_data_gather) + 
        geom_boxplot(aes(p_type, bfi10_score, fill = gender), notch = FALSE, outlier.color = "salmon",
                     outlier.size = 1.5, outlier.shape = 18) + 
        scale_x_discrete(labels=c("a" = "A", "c" = "C", "e" = "E",
                                  "n" = "N", "o" = "O")) +
        theme_matt() + 
        scale_fill_manual(values = c(female = "rosybrown1", male = "steelblue"), name = "Gender", labels = c("Male", "Female")) +
        theme(legend.position = "right", legend.text = element_text(size = 8),
              legend.title = element_text(size = 9), axis.title = element_text(size = 9), 
              axis.text = element_text(size = 9, color = "black")) +
        xlab("Big Five domain") +
        ylab("BFI-10 Big Five score") +
        stat_summary(mapping = aes(p_type, bfi10_score, fill = gender), 
                     fun.y = mean, colour="orange", geom="point", shape=16, size=1.5, 
                     position = position_dodge(width = 0.75)) +
        scale_y_continuous(limits = c(1,5), breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
                           labels = c("1", "", "2", "", "3", "", "4", "", "5")) +
        labs(caption = expression(paste(italic(bar(x)), " = dot, outliers = diamond"))) +
        theme(plot.caption = element_text(hjust = 1.3, size = 7))


ggsave(plot = part_gender_bfi10, filename = "part_gender_bfi10.png", device = "png", 
       width = 12, height = 12*0.618, units = "cm") #golden ratio!!

## Participants' tweets


gender_agegroup_tweetnum <- ggplot(diss_data) + 
        geom_boxplot(aes(age_group, tweet_num, fill = gender), 
                        notch = FALSE,
                        outlier.color = "salmon",
                        outlier.size = 2, outlier.shape = 18) + 
        scale_x_discrete(labels=c("female" = "Female", "male" = "Male")) +
        theme_matt() + 
        theme(
              axis.title = element_text(size = 9), 
              axis.text = element_text(size = 9, color = "black")) +
        scale_fill_manual(values = c(female = "rosybrown1", male = "steelblue"), 
                        name = "Gender", labels = c("Male", "Female")) +
        scale_y_continuous(limits = c(0,2500), labels = scales::comma) +
        xlab("Age group") +
        ylab("Number of tweets") +
        stat_summary(mapping = aes(age_group, tweet_num, fill = gender), 
                     fun.y = mean, colour="orange", geom="point", shape=16, size=2, 
                     position = position_dodge(width = 0.75)) +
        labs(caption = expression(paste(italic(bar(x)), " = dot, outliers = diamond"))) +
        theme(plot.caption = element_text(hjust = 1.3, size = 7))

ggsave(plot = gender_agegroup_tweetnum, filename = "gender_agegroup_tweetnum.png", device = "png", 
       width = 12, height = 12*0.618, units = "cm") #golden ratio!!



ggplot(diss_data, aes(age, tweet_num)) + 
        geom_point() +
        geom_smooth(se = FALSE) +
        geom_encircle(data = subset(diss_data, tweet_num > 1500), expand = .03, s_shape = .5, spread = 1) +
        geom_point(data = subset(diss_data, tweet_num > 1500), col = "red")


## Dumbell plot with differences in tweet_num...male/female

library(ggalt)
library(hrbrthemes)
library(scales)


sub_gender <- diss_data %>% 
        select(gender, age_group, tweet_num) %>% 
        group_by(gender, age_group) %>% 
        summarise(tweets = sum(tweet_num))

sub_gender <- sub_gender %>% mutate(id = 1:n()) %>% spread(key = gender, value = tweets)

sub_gender$diff <- abs(sub_gender$male - sub_gender$female) #absolute values!
       

tweet_dumbbell <- ggplot(sub_gender, aes(x = male, xend = female, y = age_group, group = age_group)) +
        geom_dumbbell(color = "#e3e2e1", colour_x = "steelblue", colour_xend = "rosybrown1",
                      size = 2, dot_guide = TRUE, dot_guide_size = 0.15,
                      size_x = 3.5, size_xend = 3.5) + 
        geom_text(aes(x = male, y = age_group, label = format(male, big.mark = ",")), vjust = 2, size = 3.5) +
        geom_text(aes(x = female, y = age_group, label = format(female, big.mark = ",")), vjust = 2, size = 3.5) +
        theme(axis.title = element_blank(), axis.line = element_blank(), panel.background = element_blank(),
              panel.grid.major.y = element_line(color = "grey", linetype = "dotted"),
              axis.text = element_text(colour = "black")) +
        xlab("Number of tweets") + 
        scale_x_continuous(breaks = c(1000, 2000, 3000, 4000, 5000, 6000), 
                           labels = c("1,000", "2,000", "3,000", "4,000", "5,000", "6,000")) +
        ## difference column
        geom_rect(aes(xmin = 6500, xmax = 7000, ymin = -Inf, ymax = Inf),
                  fill = "#efefe3") +
        geom_text(aes(x = 6750, y = age_group, label = format(diff, big.mark = ",")), size = 3.5) +
        geom_text(data = filter(sub_gender, age_group == "36 - 45 years"), 
                  aes(x = 6750, y = age_group), label = "DIFF", vjust = -2, fontface = "bold") +
        ## labels female
        geom_text(data = filter(sub_gender, age_group == "36 - 45 years"), 
                  aes(x = 1658, y = age_group), label = "F", vjust = -1, size = 3) +
        geom_text(data = filter(sub_gender, age_group == "25 - 35 years"), 
                  aes(x = 5947, y = age_group), label = "F", vjust = -1, size = 3) +
        geom_text(data = filter(sub_gender, age_group == "20 - 24 years"), 
                  aes(x = 489, y = age_group), label = "F", vjust = -1, size = 3) +
        ## labels male
        geom_text(data = filter(sub_gender, age_group == "36 - 45 years"), 
                  aes(x = 5855, y = age_group), label = "M", vjust = -1, size = 3) +
        geom_text(data = filter(sub_gender, age_group == "25 - 35 years"), 
                  aes(x = 4255, y = age_group), label = "M", vjust = -1, size = 3) +
        geom_text(data = filter(sub_gender, age_group == "20 - 24 years"), 
                  aes(x = 1568, y = age_group), label = "M", vjust = -1, size = 3)

ggsave(tweet_dumbbell, filename = "tweet_dumbell.png", device = "png", dpi = 600, height = 2.3, width = 6)


## Polar bar chart with BigFive Scores

library(ggiraphExtra)

t <- diss_data %>% filter(part_id == 1) %>% select(gender, e, a, c, n, o) %>% mutate(E = e, A = a, C = c, N = n, O = o)


t_2 <- t %>% gather(`E`, `A`, `C`, `N`, `O`, key = "p_type", value = "bfi10_score") %>% mutate(p_type = as.factor(p_type))

t_2$part_id <- "1"



source("ggBar2.R") # From ggiraphExtra package - removed Color Brewer Restrictions and updated function to work with predefined palette
# Standard RColor Brewer palettes don't work no more - have to be custom made with ggBar2

library(RColorBrewer)

col_pal <- brewer.pal(11, "RdYlBu")[c(2,4,6,8,10)]

bfi_bar_round <- ggBar2(t_2, aes(x = p_type, fill = p_type, y = bfi10_score), stat = "identity", polar = TRUE, 
      palette = col_pal, width = 1, color = "black", size = 0.2, addlabel = FALSE) + 
        theme(legend.position = "none", 
              axis.title = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = "lightgrey"), 
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 12),
              axis.ticks.x = element_blank(),
              axis.line = element_line(color = "black")) + 
        scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("1", "2", "3", "4", "5")) +
        scale_x_discrete(limit = c("C", "N", "O", "E", "A")) +
        geom_text(data = t_2, aes(x = p_type, y = bfi10_score, label = p_type), size = 4.5, vjust = -.15)


ggsave(bfi_bar_round, filename = "bfi_bar_round.png", device = "png", units = "cm", height = 10, width = 10)

## GGRose better

source("ggRose2.R") # see ggBar2...also loads ggBar2

bfi_rose <- ggRose2(t_2, aes(x = p_type, fill = p_type, y = bfi10_score), palette = col_pal, alpha = .85) +
        theme(legend.position = "none", 
              axis.title = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = "lightgrey"), 
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12),
              axis.line = element_line(color = "black")) +
        scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("1", "2", "3", "4", "5")) +
        scale_x_discrete(limit = c("A", "C", "N", "O", "E"))

ggsave(bfi_rose, filename = "bfi_rose.png", device = "png", units = "cm", height = 10, width = 10)


## Getting mean personalities

t2 <- diss_data %>% filter(part_id == 1 | part_id == 4) %>% 
        select(e, a, c, n, o) %>% summarize_at(vars(e:o), mean)

t2 <- t2 %>% gather(`e`, `a`, `c`, `n`, `o`, key = "p_type", value = "bfi10_score") %>% mutate(p_type = as.factor(p_type))


ggRose2(t2, aes(x = p_type, fill = p_type, y = bfi10_score), palette = col_pal, alpha = .85) +
        theme(legend.position = "none", 
              axis.title = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_line(color = "lightgrey"), 
              axis.text.x = element_text(size = 12, face = "bold"),
              axis.text.y = element_text(size = 12),
              axis.line = element_line(color = "black")) +
        scale_y_continuous(breaks = c(0,1,2,3,4), labels = c("1", "2", "3", "4", "5")) +
        scale_x_discrete(limit = c("A", "C", "N", "O", "E"))

iris %>% summarise_at(vars(1:2), mean)
