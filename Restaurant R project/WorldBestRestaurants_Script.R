#import packages 
library(readr)
 library(dplyr)
 library(tidyr)
 library(ggplot2)

#import datasheet
Data1 <- read_csv("C:/Users/cursa/Downloads/WorldBestRestaurants.csv")

#Clean data (Replacing "na" with "Germany" and remove the star column respectively)
 Data1$country[is.na(Data1$country)] <- "Germany"
 Data01 <- Data1 %>% select(-star)

 #renamed new datasheet
write.csv(Data01, "WorldBestRestaurantNew.csv")

#create heat map

ggplot(heatmap_data, aes(x = year, y = country, fill = avg_rank)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "darkgreen", high = "lightyellow", name = "Avg Rank") +
  theme_minimal() +
  labs(
    title = "Average Restaurant Ranking by Year and Country",
    x = "Year",
    y = "Country"
  )

#list top ten and create a bar chart
top10 <- Data01 %>% filter(rank <= 10)

top10_country <- top10 %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(top10_country, aes(x = reorder(country, count), y = count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Countries with the Most Top-10 Restaurants",
    x = "Country",
    y = "Number of Top-10 Restaurants"
  ) +
  theme_minimal()

#create a world map to look at the location of each restaurant
library(maps)
world_map <- map_data("world")

ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region),
           fill = "gray90", color = "gray60", size = 0.2) +
  geom_point(data = top10, aes(x = lng, y = lat, color = country),
             size = 2, alpha = 0.8) +
  labs(
    title = "Top 10 World's Best Restaurants (by Location)",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

