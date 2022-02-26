# Map showing percentage of Minority Populations in U.S. Prisons


library(stringr)
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
library(tidyverse)
library(tidyr)

blank_theme <- theme_bw() + 
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()
  )

df3 <- incarceration_trends %>%
  select(year, aapi_prison_pop, latinx_prison_pop, native_prison_pop, black_prison_pop, other_race_prison_pop, total_prison_pop, fips) %>%
  na.omit() %>%
  mutate(minority_prison_pop = aapi_prison_pop + black_prison_pop + native_prison_pop + latinx_prison_pop + other_race_prison_pop) %>%
  mutate(minority_prison_percent = round((minority_prison_pop/total_prison_pop)*100, 2)) %>%
  select(year, minority_prison_pop, total_prison_pop, minority_prison_percent, fips)

county_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")


map_data <- county_shape %>%
  left_join(df3, by = "fips")
  
prison_pop_map <- ggplot(map_data) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = minority_prison_percent),
               color = "gray", size = 0.3) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$minority_prison_percent)), na.value = "white", 
low = "blue", high = "green") +
  blank_theme + 
  ggtitle("Percentage of Minority Populations Incarcerated of Total Prison Population in the U.S")
  

