# Chart Comparing the number of Black People Incarcerated to the number of White People Incarcerated within different urbancities in 2009

incaceration_file <- "http://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_trends <- read.csv(url(incaceration_file), stringsAsFactors = FALSE)

df2 <- incarceration_trends %>%
  filter(white_prison_adm > 0, black_prison_adm > 0, year == "2009") %>%
  select(white_prison_adm, black_prison_adm, urbanicity)
  
plot2 <- ggplot(df2, mapping = aes(x = white_prison_adm, y = black_prison_adm, colour = urbanicity)) + geom_point() + labs(title = "Racial Disparities in Prison Admissions by Urbancities in 2009",
subtitle = "Number of White People Incarcerated vs. Number of Black People Incarcerated in the U.S.", x = "White Prison Incarcerations", y = "Black Prison Incarcerations")
