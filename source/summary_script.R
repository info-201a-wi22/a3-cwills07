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
