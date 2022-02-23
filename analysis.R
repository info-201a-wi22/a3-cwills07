incarceration_trends_AL <- incarceration_trends %>%
  select(black_jail_pop, total_jail_pop, state, year, white_jail_pop) %>%
  filter(state == "AL", black_jail_pop > 0) %>%
  group_by(year)

incarceration_trends_AL$black_jail_pop <- as.numeric(incarceration_trends_AL$black_jail_pop)
incarceration_trends_AL$white_jail_pop <- as.numeric(incarceration_trends_AL$white_jail_pop)

ggplot(incarceration_trends_AL, aes(x = year, y = black_jail_pop)) + geom_point() + geom_smooth(method = lm, col = "cyan") + 
  ggtitle("Black Incarceration Rate in Alabama from 1985 to 2018") + labs(x = "year", y = "Black jail population")

ggplot(incarceration_trends_AL, aes(x = year, y = white_jail_pop)) + geom_point() + geom_smooth(method = lm, col = "green") + 
  ggtitle("White Incarceration Rate in Alabama from 1985 to 2018") + labs(x = "year", y = "White jail population")

chart2 <- ggplot(data = incarceration_trends_prison_pop, aes(x = year, y = white_prison_pop)) + geom_point() + geom_smooth(method = lm, col = "red") + 
  ggtitle("White Prison Population from 1970 to 2016") + labs(x = "year", y = "White Prison Population")    

chart3 <- ggplot(data = incarceration_trends_prison_pop, aes(x = year, y = native_prison_pop)) + geom_point() + geom_smooth(method = lm, col = "blue") + 
  ggtitle("Native American Prison Population from 1970 to 2016") + labs(x = "year", y = "Native American Prison Population")

chart4 <-  ggplot(data = incarceration_trends_prison_pop, aes(x = year, y = latinx_prison_pop)) + geom_point() + geom_smooth(method = lm, col = "purple") + 
  ggtitle("Latinx Prison Population from 1970 to 2016") + labs(x = "year", y = "Latinx Prison Population")

chart5 <- ggplot(data = incarceration_trends_prison_pop, aes(x = year, y = total_prison_pop)) + geom_point() + geom_smooth(method = lm, col = "orange") + 
  ggtitle("Total Prison Population from 1970 to 2016") + labs(x = "year", y = "Total Prison Population")
