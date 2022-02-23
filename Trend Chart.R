incaceration_file <- "http://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_trends <- read.csv(url(incaceration_file), stringsAsFactors = FALSE)


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
                                                                                                     
# Map showing prison incarcerations in the U.S. by state from 

