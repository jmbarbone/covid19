
library(tidyverse)
library(plotly)
theme_set(theme_bw())

load('data/pa_county_confirmed.rda')
load('data/pa_county_population.rda')

# New confirmed cases -----------------------------------------------------

pa_county_confirmed %>% 
  pivot_longer(cols = -date,
               names_to = "county",
               values_to = "cases") %>% 
  group_by(date) %>% 
  summarise(cases = sum(cases)) %>% 
  mutate(new_cases = cases - lag(cases)) %>% 
  replace_na(list(new_cases = 0)) %>% 
  mutate(fill = fct_reorder(as.factor(date), new_cases)) %>% 
  ggplot(aes(x = date, y = new_cases, fill = fill)) +
  geom_col() +
  scale_fill_discrete() +
  theme(legend.position = "none") +
  labs(title = "Pennsylvania: New cases by date",
       x = "Date of report",
       y = "New cases (from previous day)")
ggplotly()

# By county pop -----------------------------------------------------------

pa_county_confirmed %>% 
  filter(date == max(date)) %>% 
  pivot_longer(-date, names_to = "county", values_to = "cases") %>% 
  inner_join(pa_county_population, by = "county") %>% 
  mutate(cases_prop = cases / estimate_population,
         cases_pmil = cases_prop * 1e6,
         county = fct_reorder(county, cases_prop)) %>% 
  ggplot(aes(x = cases_pmil, y = county, fill = county)) +
  geom_col() +
  theme(legend.position = "none") +
  # scale_x_continuous(label = scales::label_percent()) +
  labs(title = "County cases per million residents",
       x = "Cases per million",
       y = 'County')
ggplotly()


# Growth and cases --------------------------------------------------------

means <- pa_county_confirmed %>% 
  mutate_if(is.integer, function(x) (x - lag(x)) / lag(x)) %>% 
  tail(7) %>% 
  select(-date) %>% 
  summarise_all(function(x) 
    psych::harmonic.mean(x[abs(x) != Inf], na.rm = TRUE, zero = FALSE)
    # mean(x[abs(x) != Inf], na.rm = TRUE)
    ) %>% 
  pivot_longer(cols = colnames(.),
               names_to = "county",
               values_to = "percentage")

pa_county_confirmed %>% 
  tail(1) %>% 
  pivot_longer(-date,
               names_to = "county",
               values_to = "total_cases") %>%
  filter(total_cases >= 50) %>%
  inner_join(means, by = "county") %>% 
  ggplot(aes(x = total_cases, y = percentage, labels = county,
             size = log(total_cases),
             color = percentage)) +
  geom_smooth(formula = "y ~ x",
              method = loess,
              aes(group = "fit")) +
  # geom_smooth(formula = "y ~ x",
  #             method = glm,
  #             method.args = list(family = quasibinomial),
  #             aes(group = "qbinomfit")) +
  geom_point() +
  scale_x_log10() +
  scale_color_gradient2(low = "green", mid = "blue", high = "red") +
  scale_y_continuous(labels = scales::label_percent(1)) +
  theme(legend.position = "none") +
  labs(title = "Pennsylvania county cases growth",
       subtitle = "Of counties with 50 or more cases",
       x = "Total Cases",
       y = "Mean proportion of growth (past 7 days)")
ggplotly()

# Some graphs ---------------------------------------------------------------------------------

pa_counties <- pa_county_confirmed %>% 
  pivot_longer(col = -date,
               names_to = "county",
               values_to = "cs_cases")

# ggplot(pa_counties,
#        aes(x = date, y = cs_cases, col = county)) +
#   geom_line() +
#   geom_smooth(method = "loess",
#               aes(group = "model"))

pa_counties %>% 
  filter(cs_cases >= 100) %>% 
  group_by(county) %>% 
  mutate(n_days = row_number()) %>% 
  ggplot(aes(x = n_days, y = cs_cases, col = county)) +
  scale_y_log10() +
  geom_smooth(method = "lm",
              se = FALSE,
              formula = "y ~ x",
              linetype = 2,
              aes(group = "fit")) +
  geom_line(size = 1) +
  labs(x = "Number of days since 100th case",
       y = "Cumulative cases")
ggplotly()

penn <- pa_counties %>% 
  group_by(date) %>% 
  summarise(cases = sum(cs_cases))

penn_ts <- ts(penn$cases, start = penn$date[1])
n <- length(penn_ts)
n_start <- n - 14

library(forecast)

## prediction based on last two weeks
mod1 <- ets(log10(penn_ts[seq(n_start, n)]), model = "AAN", damped = FALSE, additive.only = FALSE)
mod2 <- ets(log10(penn_ts[seq(n_start, n)]), model = "AAN", damped = TRUE, additive.only = FALSE)

forecast_h <- 120
res1 <- forecast(mod1, h = forecast_h)
res2 <- forecast(mod2, h = forecast_h)
res_data1 <- cbind(date = c(max(penn$date) + seq(forecast_h)),
                   res1$lower %>% sapply(function(x) 10^x) %>% as_tibble %>% set_names(c("lower_80", "lower_90")),
                   mean = sapply(res1$mean, function(x) 10^x, simplify = FALSE) %>% unlist(),
                   res1$upper %>% sapply(function(x) 10^x) %>% as_tibble %>% set_names(c("upper_80", "upper_90"))) %>% 
  mutate_if(is.double, round) %>% as_tibble()

res_data2 <- cbind(date = c(max(penn$date) + seq(forecast_h)),
                   res2$lower %>% sapply(function(x) 10^x) %>% as_tibble %>% set_names(c("lower_80", "lower_90")),
                   mean = sapply(res2$mean, function(x) 10^x, simplify = FALSE) %>% unlist(),
                   res2$upper %>% sapply(function(x) 10^x) %>% as_tibble %>% set_names(c("upper_80", "upper_90"))) %>% 
  mutate_if(is.double, round) %>% as_tibble()
res_data1 <- res_data1[seq(14), ]
res_data2 <- res_data2[seq(14), ]

ggplot(penn, aes(x = date, y = cases)) +
  geom_line(size = 1.5) +
  # geom_ribbon(data = res_data1,
  #             fill = "red",
  #             alpha = .15,
  #             aes(x = date,
  #                 y = NULL,
  #                 ymin = lower_90,
  #                 ymax = upper_90)) +
  # geom_ribbon(data = res_data1,
  #             fill = "red",
  #             alpha = .3,
  #             aes(x = date,
  #                 y = NULL,
  #                 ymin = lower_80,
  #                 ymax = upper_80)) +
  # geom_point(data = res_data1,
  #            size = 2,
  #            color = "darkred",
  #            aes(x = date, y = mean)) +
  geom_ribbon(data = res_data2,
              fill = "blue",
              alpha = .15,
              aes(x = date,
                  y = NULL,
                  ymin = lower_90,
                  ymax = upper_90)) +
  geom_ribbon(data = res_data2,
              fill = "blue",
              alpha = .3,
              aes(x = date,
                  y = NULL,
                  ymin = lower_80,
                  ymax = upper_80)) +
  geom_point(data = res_data2,
             size = 2,
             color = "darkblue",
             aes(x = date, y = mean)) +
  scale_x_date(date_breaks = "14 days") +
  # scale_y_log10() +
  geom_hline(yintercept = 34000, linetype = 2) +
  theme_bw() +
  labs(title = "Forecast for confirmed COVID-19 cases on Pennsylvania",
       subtitle = paste0("Estimates with 90% and 80% confidence intervals\n",
                         "At 34k cases, assuming 10% cases need ICU,",
                         " all estimate 3400 ICU beds will be filled"),
       caption = paste0("Data are first transformed to log10 values for forecast\n",
                        "Only last 2 weeks of data are used"),
       x = "Date",
       y = "Number of confirmed cases")
ggplotly(dynamicTicks = FALSE, originalData = TRUE)
