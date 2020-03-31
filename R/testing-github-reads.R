
# ---------------------------------------------------------------------------------------------
# Read from github
# ---------------------------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(plotly)

link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

states <- c(state.name, "District of Columbia")

## this doesn't have US totals?
covid_temp <- read_csv(link) %>% 
  pivot_longer(cols = -c(1:4),
               names_to = "date",
               values_to = "cs_cases") %>% 
  mutate_at("date", as.Date, format = "%m/%d/%y") %>% 
  clean_names()

us_covid <- covid_temp %>% 
  filter(province_state %in% states,
         country_region == "US") %>% 
  group_by(country_region, date) %>% 
  summarise(cs_cases = sum(cs_cases))

covid <- bind_rows(covid_temp, us_covid)  

covid %>% 
  filter(country_region == "US",
         province_state %in% states,
         date == Sys.Date()) %>%
  ggplot(aes(x = cs_cases,
           y = reorder(province_state, cs_cases))) +
  geom_col()
ggplotly()

covid %>% 
  filter(country_region == "US",
         province_state %in% states,
         date == Sys.Date()) %>%
  mutate(prop = cs_cases / sum(cs_cases)) %>% 
  arrange(desc(prop))

# link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

covid %>% 
  filter(is.na(province_state)) %>% 
  filter(cs_cases >= 500) %>% 
  group_by(country_region) %>% 
  mutate(n_days = row_number()) %>% 
  ggplot(aes(x = n_days, y = cs_cases, color = country_region)) +
  scale_y_log10() +
  geom_line() +
  geom_smooth(method = "lm",
              formula = "y ~ x",
              aes(color = "fitting")) +
  geom_smooth(method = "loess",
              formula = "y ~ log10(x)",
              aes(color = "smoothed")) +
  geom_smooth(method = "lm",
              se = TRUE,
              formula = "y ~ log10(x)",
              aes(color = "modeling"))
ggplotly()


# Cases per population ------------------------------------------------------------------------

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
