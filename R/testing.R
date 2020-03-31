
# ---------------------------------------------------------------------------------------------
# Covid Data
# ---------------------------------------------------------------------------------------------

## Reinstall data every time
devtools::install_github("RamiKrispin/coronavirus")

options(tidyverse.quiet = TRUE)
library(tidyverse)
library(janitor)
library(coronavirus)
library(forecast)
library(plotly)

load("data/state_populations.rda")

us_coronavirus <- coronavirus %>% 
  clean_names() %>% 
  as_tibble() %>% 
  filter(country_region == "US",
         type == "confirmed") %>% 
  group_by(province_state) %>% 
  arrange(date, .by_group = TRUE) %>% 
  mutate(cs_cases = cumsum(cases),
         prop_change = (cs_cases / lag(cs_cases)) - 1) %>% 
  ungroup() 

## so we have Washington D.C. and District of Columbia...

us_coronavirus %>% 
  filter(province_state %in% c("Washington, D.C.", "District of Columbia")) %>% distinct(province_state)
  anti_join(state_populations, by = c("province_state" = "state"))
distinct(us_coronavirus)


# total number of cases
us_coronavirus %>% 
  group_by(province_state) %>% 
  summarise(n_cases = sum(cases)) %>% 
  ggplot(aes(y = reorder(province_state, n_cases), x = n_cases)) +
  geom_col() +
  labs(title = "Total number of cases",
       x = "cumulative confirmed cases",
       y = "State")
ggplotly()


# cases per million
us_coronavirus %>%
  group_by(province_state) %>% 
  summarise(n_cases = sum(cases)) %>% 
  inner_join(state_populations, by = c("province_state" = "state")) %>% 
  mutate(cases_per_million = n_cases / (population_estimate / 1e6)) %>% 
  ggplot(aes(y = reorder(province_state, cases_per_million), x = cases_per_million)) +
  geom_col() +
  labs(title = "Number of cases per 1 million persons",
       x = "Cumulative confirmed cases (per 1mil)",
       y = "State")
ggplotly()

us_coronavirus %>%
  group_by(province_state) %>% 
  summarise(n_cases = sum(cases)) %>% 
  inner_join(state_populations, by = c("province_state" = "state")) %>% 
  mutate(cases_per_million = n_cases / (population_estimate / 1e6)) %>% 
  ggplot(aes(x = n_cases, y = cases_per_million,
             label = province_state)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  geom_point(aes(size = population_estimate)) +
  stat_ellipse()
ggplotly()

penn <- us_coronavirus %>% 
  filter(province_state == "Pennsylvania") %>%
  filter(cs_cases > 0) %>% 
  select(cases = cs_cases,
         date)

penn_ts <- ts(cumsum(penn$cases), start = penn$date[1])

mod <- ets(log(penn_ts), model = "AAN", damped = FALSE, additive.only = TRUE)
res <- forecast(mod)
res$series
max(penn$date)
plot(res)
res


## Overall trend
us_coronavirus %>% 
  ggplot(aes(x = date, y = cs_cases, col = province_state, group = province_state)) +
  geom_line() +
  scale_y_log10()

ggplotly()


## Trend of days since 30
us_coronavirus %>% 
  filter(cs_cases >= 50) %>% 
  group_by(province_state) %>% 
  mutate(days_since_n = row_number(),
         label = if_else(date == max(date), province_state, NA_character_)) %>% 
  ungroup() %>% 
  ggplot(aes(x = days_since_n, y = cs_cases, col = province_state, group = province_state)) +
  geom_line() + geom_point() +
  geom_label(aes(label = label)) +
  scale_y_log10() +
  geom_smooth(method = "loess", aes(group = "model fit"))

ggplotly()


us_coronavirus %>% 
  filter(cs_cases >= 50) %>%
  group_by(province_state) %>% 
  mutate(days_since_n = row_number()) %>% 
  ungroup() %>% 
  ggplot(aes(x = days_since_n,
             y = prop_change,
             col = province_state,
             group = province_state)) +
  geom_line() + geom_point() +
  geom_smooth(method = "loess", aes(group = "model fit"))
ggplotly()
