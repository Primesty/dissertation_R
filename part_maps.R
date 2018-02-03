
# Plotting participants on map --------------------------------------------

# Prelimniary work in zip_code_handling.R

library(zipcode)
library(tidyverse)


zip_ger <- readRDS(file = "zip_ger_sub.rds")

### Match zip codes from one dataframe with lon/lat

diss_zip <- diss_data %>% select(zip, gender, age, edu, edu2, tweet_num)

diss_zip$zip <- clean.zipcodes(diss_zip$zip)

diss_part <- left_join(diss_zip, zip_ger, by = "zip") %>% na.omit()


# matches zip codes (zip) in both dataframes and adds lon/lat and city and omits parts with no zip


# Start plotting ----------------------------------------------------------

ger_map.df <- readRDS(file = "ger_map.df.rds")

statenames <- ger_map.df %>% select(long, lat, NAME_1) %>% 
        group_by(NAME_1) %>% summarise(long = mean(long), lat = mean(lat))

ggplot() +
        geom_polygon(data = ger_map.df, 
                     aes(x = long, y = lat, group = group), fill = NA, 
                     color = "black", size = 0.4) + 
        geom_point(data = diss_part, aes(lon, lat, col = gender), size = 2) +
        scale_color_manual(values = c(female = "red", male = "blue"), name = "Gender") +
        coord_map() +
        theme_nothing(legend = TRUE) +
        ggtitle("Study Participants") +
        geom_text(data = statenames, aes(long, lat, label = NAME_1), size = 3,
                  fontface = "bold", check_overlap = TRUE) +
        geom_density2d(data = diss_part, aes(lon, lat), size = 0.5, col = "black")

## If we would like to fill by tweet_num, we first have to add the values to the ger_map.df matching by state

## Getting list with zips and states into R

zip_state <- read.csv("./states_zips.csv", header = FALSE, col.names = c("state", "zip", "city"))

levels(zip_state$state)

zip_state <- zip_state %>% mutate(state = fct_recode(state, "Baden-Württemberg" = "Baden-W\x9frttemberg",
                                                     "Thüringen" = "Th\x9fringen"))

zip_state$zip <- clean.zipcodes(zip_state$zip)

zip_state <- zip_state[row.names(unique(zip_state[,c("zip", "state")])),] # only unique entries

diss_part <- left_join(diss_part, zip_state[,c("zip","state")], by = "zip")

state_num <- diss_part %>% group_by(state) %>% summarise(n = sum(tweet_num))

## Now we match back to ger_map.df

ger_map.df <- left_join(ger_map.df, state_num, by = c("NAME_1" = "state"))


ggplot() +
        geom_polygon(data = subset(ger_map.df, !NAME_1 %in% c("Berlin", "Bremen")), 
                     aes(x = long, y = lat, group = group, fill = n), 
                     color = "black", size = 0.4) + 
        geom_polygon(data = subset(ger_map.df, NAME_1 %in% c("Berlin", "Bremen")), 
                     aes(x = long, y = lat, group = group, fill = n), 
                     color = "black", size = 0.4) + 
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 1428, name = "Num tweets") +
        geom_point(data = diss_part, aes(lon, lat, shape = gender), size = 2) +
        coord_map() +
        theme_nothing(legend = TRUE) +
        ggtitle("Study Participants")


ggplot() +
        geom_polygon(data = subset(ger_map.df, !NAME_1 %in% c("Berlin", "Bremen")), 
                     aes(x = long, y = lat, group = group, fill = n), 
                     color = "black", size = 0.4) + 
        geom_polygon(data = subset(ger_map.df, NAME_1 %in% c("Berlin", "Bremen")), 
                     aes(x = long, y = lat, group = group, fill = n), 
                     color = "black", size = 0.4) + 
        scale_fill_gradient(low = "steelblue", high = "tomato", name = "Number of \n   tweets") +
        geom_point(data = diss_part, aes(lon, lat, shape = gender), size = 2) +
        scale_shape_discrete(name = "Gender") +
        coord_map() +
        theme_nothing(legend = TRUE) +
        ggtitle("Study Participants and tweet density") +
        theme(plot.title = element_text(hjust = 0.5))

## Plotting with ggmap

library(ggmap)

germany <- get_map(location = "germany", zoom = 6, source = "google")

part_map1 <- ggmap(germany, extent = "normal") +
        geom_point(data = diss_part, aes(lon, lat, col = gender, shape = gender), size = 2) + 
        theme_nothing() + 
        scale_color_manual(values = c(female = "red", male = "blue"))

ggsave(part_map1, height = 5, width = 5, filename = "part_map1.png")

part_map2 <- ggmap(germany, extent = "device") + 
        geom_density2d(data = diss_part, aes(lon, lat), size = 0.3, col = "black", alpha = 0.6) +
        geom_point(data = diss_part, aes(lon, lat, shape = gender, col = gender), size = 2) +
        scale_color_manual(values = c("female" = "tomato", "male" = "steelblue"), name = "Gender", labels = c("Male", "Female")) +
        scale_shape_discrete(name = "Gender", labels = c("Male", "Female")) +
        theme_matt() +
        theme(axis.text = element_blank(), legend.position = "bottom", legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))

ggsave(part_map2, height = 11, width = 10, filename = "part_map2.png", units = "cm", device = "png")

part_map3_jitter <- ggmap(germany, extent = "device") + 
        geom_density2d(data = diss_part, aes(lon, lat), size = 0.3, col = "black", alpha = 0.6) +
        geom_jitter(data = diss_part, aes(lon, lat, shape = gender, col = gender), size = 2) +
        scale_color_manual(values = c("female" = "red", "male" = "blue"), name = "Gender", labels = c("Male", "Female")) +
        scale_shape_discrete(name = "Gender", labels = c("Male", "Female")) +
        theme_matt() +
        theme(axis.text = element_blank(), legend.position = "bottom", legend.text = element_text(size = 8),
              legend.title = element_text(size = 8))

ggsave(part_map3_jitter, height = 11, width = 10, filename = "part_map3_jitter.png", units = "cm", device = "png")

## GGmap with arrows (movement)

# Create data to show movement

diss_zip2 <- diss_data %>% select(zip, alt_zip, gender, age, edu, tweet_num)

diss_zip2$zip <- clean.zipcodes(diss_zip2$zip)

diss_zip2$alt_zip <- clean.zipcodes(diss_zip2$alt_zip)

alt_zip <- as.data.frame(diss_zip2$alt_zip)
names(alt_zip) <- "alt_zip"

alt_zip <- left_join(alt_zip, zip_ger, by = c("alt_zip" = "zip"))

diss_part2 <- left_join(diss_zip2, zip_ger, by = "zip")

diss_part2 <- diss_part2 %>% select(zip, lon, lat, everything())

diss_part2 <- diss_part2 %>% na.omit()

alt_zip <- alt_zip %>% na.omit()

diss_part2 <- left_join(diss_part2, alt_zip, by = "alt_zip")

part_map4_move <- ggmap(germany, extent = "device") + 
        geom_point(data = diss_part, aes(lon, lat, shape = gender, col = gender), size = 2) +
        scale_color_manual(values = c("female" = "tomato", "male" = "steelblue"), name = "Gender", labels = c("Male", "Female")) +
        scale_shape_discrete(name = "Gender", labels = c("Male", "Female")) +
        geom_segment(data = diss_part2, aes(lon.y, lat.y, yend = lat.x, xend = lon.x), 
                     arrow = arrow(angle = 20, length = unit(0.2, "cm")), col = "black", alpha = .65) +
        theme(axis.text = element_blank(), legend.position = "bottom", legend.text = element_text(size = 8),
              legend.title = element_text(size = 8), legend.key = element_blank())

ggsave(part_map4_move, height = 11, width = 10, filename = "part_map4_move.png", units = "cm", device = "png")

# Plot heatmap

ggmap(germany) + 
        geom_density2d(data = diss_part, aes(x = lon, y = lat), size = 0.3, alpha = 0.5, color = "black") +
        stat_density2d(data = diss_part, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                       geom = 'polygon') +
        scale_fill_viridis(direction = -1, option = "plasma", guide = FALSE) +
        scale_alpha(range = c(0.05, 0.2), guide = FALSE) +
        geom_point(data = diss_part, aes(lon, lat, shape = gender, col = gender)) +
        scale_color_manual(values = c("female" = "red", "male" = "blue", guide = FALSE)) +
        scale_shape_discrete(name = "Gender") +
        ggtitle("Study participants and tweet density") +
        theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))



## Plotting with leaflet
library(leaflet)

## Make custom Twitter icons

# red = https://www.iconfinder.com/icons/738405/media_online_social_twitter_icon#size=128
# blue = https://www.iconfinder.com/icons/1233015/twitter_icon#size=128

twitterIconBlue <- makeIcon(
        iconUrl = "twitter_blue.png",
        iconWidth = 24,
        iconHeight = 24,
        iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

twitterIconRed <- makeIcon(
        iconUrl = "twitter_red.png",
        iconWidth = 24,
        iconHeight = 24,
        iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

## Adding popup-info to data frame

diss_part2 <- diss_part %>% mutate(gender = fct_recode(gender, "Female" = "female",
                                                       "Male" = "male")) %>% 
        mutate(popup_info = paste(sep = "<br/>", paste0("<b>","<i>",gender,"<i>", "      
                                                        </b>"), city, edu, edu2, tweet_num))

## Plotting the map

twitterIcons <- iconList(Male = twitterIconBlue, Female = twitterIconRed)



leaflet(diss_part2) %>%
        addTiles() %>% 
        addMarkers(lng = ~lon, 
                   lat = ~lat, icon = ~twitterIcons[diss_part2$gender],
                   popup = ~popup_info, clusterOptions = markerClusterOptions())

# Alternative

leaflet(diss_part2) %>%
        addProviderTiles(provider = providers$Esri.WorldStreetMap,
                         options = tileOptions(minZoom=2)) %>% 
        addMarkers(lng = ~lon, 
                   lat = ~lat, icon = ~twitterIcons[diss_part2$gender],
                   popup = ~popup_info, clusterOptions = markerClusterOptions())


leaflet() %>% 
        addProviderTiles("Stamen.Watercolor")