incaceration_file <- "http://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_trends <- read.csv(url(incaceration_file), stringsAsFactors = FALSE)
setwd("/Users/carolinewills/Desktop/info201_code/a3-cwills07")

library(stringr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(reshape)

# Trend chart comparing the total prison population in the U.S. from 1988 to 2016 by race

df1 <- incarceration_trends %>%
  select(year, aapi_prison_pop, black_prison_pop, native_prison_pop, white_prison_pop, latinx_prison_pop, total_prison_pop, other_race_prison_pop) %>%
  filter(aapi_prison_pop > 0, black_prison_pop > 0, native_prison_pop > 0, white_prison_pop > 0, latinx_prison_pop > 0, total_prison_pop > 0, other_race_prison_pop > 0) %>%
  group_by(year) %>%
  summarise(year = unique(year), aapi_prison_pop= sum(aapi_prison_pop), black_prison_pop = sum(black_prison_pop), native_prison_pop = sum(native_prison_pop),
            white_prison_pop = sum(white_prison_pop), latinx_prison_pop = sum(latinx_prison_pop), other_race_prison_pop = sum(other_race_prison_pop), total_prison_pop = sum(total_prison_pop))

chart1 <- ggplot(df1, aes(x = year, y = white_prison_pop)) +  geom_line(color = "red") + labs(x = "year", y = "White")
chart2 <- ggplot(df1, aes(year, aapi_prison_pop)) + geom_line(color = "purple") + labs(x = "year", y = "AAPI")
chart3 <- ggplot(df1, aes(year, black_prison_pop)) + geom_line(color = "blue") + labs(x = "year", y = "Black")
chart4 <- ggplot(df1, aes(year, latinx_prison_pop)) + geom_line(color = "pink") + labs(x = "year", y = "Latinx")
chart5 <- ggplot(df1, aes(year, native_prison_pop)) + geom_line(color = "orange") + labs(x = "year", y = "Native American")
chart6 <- ggplot(df1, aes(year, other_race_prison_pop)) + geom_line(color = "cyan") + labs(x = "year", y = "Other Races")
chart7 <- ggplot(df1, aes(year, total_prison_pop)) + geom_line(color = "green") + labs(x = "year", y = "Total Prison")

prison_pop_trend <- (chart1 + chart2 + chart3 + chart4 + chart5 + chart6 + chart7) + plot_annotation(title = "Racial Disparities in Prison Populations in the U.S.",
                                                                                                     subtitle = "Collected from 1988 to 2016", caption = "https://github.com/vera-institute/incarceration-trends#documentation")

# Map showing minority prison population to total prison population in the U.S. 

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

# Chart Comparing the number of Black People Incarcerated to the number of White People Incarcerated within different urbancities in 2009

df2 <- incarceration_trends %>%
  filter(white_prison_adm > 0, black_prison_adm > 0, year == "2009") %>%
  select(white_prison_adm, black_prison_adm, urbanicity)

plot2 <- ggplot(df2, mapping = aes(x = white_prison_adm, y = black_prison_adm, colour = urbanicity)) + geom_point() + labs(title = "Racial Disparities in Prison Admissions by Urbancities in 2009",
                                                                                                                           subtitle = "Number of White People Incarcerated vs. Number of Black People Incarcerated in the U.S.", x = "White Prison Incarcerations", y = "Black Prison Incarcerations")
### Summary: 

# Which year had the highest incarceration rate in the U.S.? 

year_highest <- df1 %>%
  filter(total_prison_pop == max(total_prison_pop)) %>% 
  pull(year)

# Which state has the highest incarceration rate of minority populations? 

df7 <- incarceration_trends %>%
  select(year, state, white_prison_pop, aapi_prison_pop, latinx_prison_pop, native_prison_pop, black_prison_pop, other_race_prison_pop, total_prison_pop) %>%
  na.omit() %>%
  mutate(minority_prison_pop = aapi_prison_pop + black_prison_pop + native_prison_pop + latinx_prison_pop + other_race_prison_pop) %>%
  mutate(minority_prison_percent = round((minority_prison_pop/total_prison_pop)*100, 2))

state_highest_2009 <- df8 %>%
  filter(year == "2009") %>%
  filter(minority_prison_pop == max(minority_prison_pop)) %>%
  pull(state)

# How many Black people were incarcerated in California in 2009? 

black_prison_pop_2009_CA <-df7 %>% 
  filter(year == "2009", state == "CA") %>% 
  summarise(black_prison_pop_CA = sum(black_prison_pop)) %>%
  pull(black_prison_pop_CA)


# How many White people were incarcerated in California in 2009?

white_prison_pop_2009_CA <- df7 %>%
  filter(year == "2009", state == "CA") %>%
  summarise(white_prison_pop_CA = sum(white_prison_pop)) %>%
  pull(white_prison_pop_CA)


# How many more Black people were incarcerated to White people in California in 2009?

difference_prison_pop_2009_CA <- (black_prison_pop_2009_CA - white_prison_pop_2009_CA)

