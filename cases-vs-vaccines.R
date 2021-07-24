library(tidyverse)
library(tidycensus)
library(lubridate)

ma7 <- function(d, y, k = 7)
  tibble(date = d, moving_avg = as.numeric(stats::filter(y, rep(1/k, k), side = 1)))

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
terr.name <- c("District of Columbia", "Puerto Rico", "American Samoa", "Guam", "Marshall Islands", "Northern Mariana Islands", "Virgin Islands")
terr.abb <- c("DC", "PR", "AS", "GU", "MH", "MP", "VI")

state.abb <- c(state.abb,terr.abb)
state.name <- c(state.name, terr.name)
dat <- read_csv(url) %>% 
  left_join(pop, by = "state") %>% 
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(cases = pmax(0,diff(c(0, cases)))) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(rate = ma7(date, cases, k = 14)$moving_avg / population[1] * 100000) %>%
  ungroup() %>%
  left_join(vac, by = c("date", "state")) %>%
  mutate(abb = state.abb[match(state, state.name)])


dat %>%  filter(state %in% c(R,D) & date >= make_date(2021, 2, 1)) %>%
  ggplot(aes(date, rate, color = state)) +
  geom_line()

election <- data.frame(state = state.name,
                       outcome = c("R","D","N")[c(1,1,2,1,2,2,2,
                                                  2,1,2,2,1,2,1,
                                                  1,1,1,1,2,2,2,
                                                  2,2,1,1,1,1,2,
                                                  2,2,2,2,1,1,1,
                                                  1,2,2,2,1,1,1,
                                                  1,1,2,2,2,1,2,1,2,
                                                  3,3,3,3,3,3)])

tmp <- dat %>% filter(date >= make_date(2021, 2, 1))  %>% left_join(election, by = "state") %>%
  mutate(outcome = factor(outcome, level = c("R", "D", "N")))


# tmp %>%
#   ggplot(aes(percent_vaccinated, rate, label = abb, color = outcome)) +
#   geom_text(cex = 1, show.legend = FALSE) +
#   facet_wrap(~date) +
#   scale_y_continuous(trans = "log2")

dates <- tmp %>%
  group_by(date) %>%
  summarize(na=sum(is.na(percent_vaccinated))) %>%
  filter(na==0) %>%
  pull(date)

dates <- c(dates, rep(last(dates),10))
library(animation)
saveGIF({
  for(i in seq_along(dates)){
    d <- dates[i]
    p<- tmp %>% filter(date == d) %>%
      ggplot(aes(percent_vaccinated, rate, label = abb, color = outcome)) +
      geom_text(cex = 10, show.legend = FALSE) +
      ggtitle(d) +
      ylab("Cases per 100,000") +
      xlab("Percent vaccinated") +
      theme_bw()+
      scale_y_continuous(trans = "log2", limit = c(0.5,82)) + 
      scale_x_continuous(limits = c(0, 70)) + 
      theme(text = element_text(size = 45),
            plot.title = element_text(size = 60, hjust = 0.5, 
                                      face = "bold")) +
      scale_color_manual(values = c("red", "blue", "black"))
    print(p)
  }
}, movie.name = "~/Desktop/ani.gif", 
  ani.height = 1089, 
  ani.width = 1280, interval = 0.1)


