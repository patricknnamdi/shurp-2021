library(tidyverse)
library(tidycensus)
library(lubridate)

pop  <-  get_estimates(geography = "state",
                       year = 2019,
                       product = "population") %>%
  filter(variable == "POP") %>%
  select(NAME, value) %>%
  rename(state = NAME, population = value)


url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
vac <- read_csv(url) %>% 
  select(date, location, people_fully_vaccinated_per_hundred) %>%
  mutate(location = recode(location, `New York State` = "New York")) %>%
  rename(percent_vaccinated  = people_fully_vaccinated_per_hundred,
         state = location)

R <- c("Mississippi", "West Virginia", "Florida", "Texas", "Alabama")
D <- c("Massachusetts", "Vermont", "New York", "California", "Connecticut")
#states <- c("Vermont", "West Virginia")

vac %>% 
  filter(state %in% c(R, D)) %>%
  mutate(dem = state %in% D) %>%
  ggplot(aes(date, percent_vaccinated, group = state, color = dem))+
  geom_line()

url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
dat <- read_csv(url) %>% 
  left_join(pop, by = "state") %>% 
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(cases = pmax(0,diff(c(0, cases)))) %>%
  ungroup() %>%
  group_by(state, date = floor_date(date, "week")) %>%
  summarize(rate = mean(cases, na.rm=TRUE) / population[1] * 100000, .groups = "drop") %>%
  ungroup() %>%
  left_join(vac, by = c("date", "state")) %>%
  mutate(abb = state.abb[match(state, state.name)])


dat %>%  filter(state %in% states & date >= make_date(2021, 2, 1)) %>%
  ggplot(aes(date, rate, color = state)) +
  geom_line()


election <- data.frame(state = state.name, outcome = c("R","D")[c(1,1,2,1,2,2,2,
                                                                  2,1,2,2,1,2,1,
                                                                  1,1,1,1,2,2,2,
                                                                  2,2,1,1,1,1,2,
                                                                  2,2,2,2,1,1,1,
                                                                  1,2,2,2,1,1,1,
                                                                  1,1,2,2,2,1,2,1)])

tmp <- dat %>% filter(date >= make_date(2021, 2, 1))  %>% left_join(election, by = "state") %>%
  mutate(outcome = factor(outcome, level = c("R", "D")))

tmp %>%
  ggplot(aes(percent_vaccinated, rate, label = abb, color = outcome)) +
  geom_text(cex = 1, show.legend = FALSE) +
  facet_wrap(~date) +
  scale_y_continuous(trans = "log2")

dates <- unique(tmp$date)
library(animation)
saveGIF({
  for(i in seq_along(dates)){
    d <- dates[i]
    p<- tmp %>% filter(date == d) %>%
      ggplot(aes(percent_vaccinated, rate, label = abb, color = outcome)) +
      geom_text(cex = 5, show.legend = FALSE) +
      ggtitle(d) +
      ylab("Cases per 100,000") +
      xlab("Percent vaccinated") +
      theme_bw()+
      scale_y_continuous(trans = "log2", limit = c(0.5,128)) + 
      scale_x_continuous(limits = c(0, 70)) + 
      theme(text = element_text(size = 20),
            plot.title = element_text(size = 15, hjust = 0.5, face = "bold"))
    print(p)
  }}, movie.name = "~/Desktop/ani.gif", 
  ani.height = 800, 
  ani.width = 1280, interval = 0.4)


