library(tidyverse)
library(tidycensus)
library(lubridate)

url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

dat <- read_csv(url)

dat %>% filter(date == max(date)) %>% view

pop  <-  get_estimates(geography = "state",
                       year = 2019,
                       product = "population") %>%
  filter(variable == "POP") %>%
  select(NAME, value) %>%
  rename(state = NAME, population = value)


dat <- dat %>% left_join(pop, by = "state")

dat %>% mutate(deaths_per_person = deaths / population * 100000) %>%
  filter(date==max(date)) %>%
  view()

dat %>% 
  filter(wday(date) == 2) %>%
  group_by(state) %>%
  mutate(deaths = diff(c(0,deaths))) %>% 
  ungroup() %>%
  mutate(deaths_per_person = deaths / population * 100000) %>%
  #filter(state %in% c("New York", "New Jersey", "Mississippi", "Rhode Island", "Massachusetts")) %>%
  filter(state %in% c("Hawaii", "Vermont", "Oregon", "Maine", "Alaska")) %>%
  ggplot(aes(date, deaths_per_person, color = state)) +
  geom_line() 



  