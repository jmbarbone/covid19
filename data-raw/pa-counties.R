
# ---------------------------------------------------------------------------------------------
# County level data for Pennsylvania
# ---------------------------------------------------------------------------------------------


library(tidyverse)
library(janitor)

read_text_table <- function(text, sep = "") {
  text %>% 
    textConnection() %>% 
    read.table(sep = sep) %>% 
    mutate(V1 = str_trim(V1)) %>% 
    pivot_wider(names_from = V1,
                values_from = V2) %>% 
    mutate_all(as.double) ## will convert everything back to integers
}

# Total count as per: https://www.health.pa.gov/topics/disease/coronavirus/Pages/Cases.aspx
# Archived here: https://www.health.pa.gov/topics/disease/coronavirus/Pages/Archives.aspx

pa_county_confirmed <- list( 
  "2020-03-06" = tibble( 
    Delaware = 1,
    Wayne = 1,
  ),
  "2020-03-07" = tibble( 
    Delaware= 1,
    Montgomery = 2,
    Wayne = 1,
  ),
  "2020-03-08" = tibble( 
    Delaware = 1,
    Montgomery = 4,
    Wayne = 1,
  ),
  "2020-03-09" = tibble( 
    Delaware = 1,
    Monroe = 1,
    Montgomery = 7,
    Wayne = 1,
  ),
  "2020-03-10" = tibble( 
    Delaware = 1,
    Monroe = 1,
    Montgomery = 8,
    Philadelphia = 1,
    Wayne = 1,
  ),
  "2020-03-11" = tibble( 
    Bucks = 2,
    Delaware = 1,
    Monroe = 2,
    Montgomery = 9,
    Philadelphia = 1,
    Wayne = 1,
  ),
  "2020-03-12" = tibble( 
    Bucks = 2,
    Delaware = 1,
    Monroe = 2,
    Montgomery = 13,
    Northampton = 1,
    Philadelphia = 1,
    Pike = 1,
    Wayne = 1,
  ),
  "2020-03-13" = tibble( 
    Bucks = 3,
    Chester = 1,
    Cumberland = 3,
    Delaware = 6,
    Monroe = 3,
    Montgomery = 18,
    Northampton = 1,
    Philadelphia = 3,
    Pike = 1,
    Washington = 1,
    Wayne = 1,
  ),
  ## Archive lists as 15 March
  "2020-03-14" = tibble( 
    Allegheny = 2, 
    Bucks = 3,
    Chester = 2,
    Cumberland = 3,
    Delaware = 6,
    Monroe = 3,
    Montgomery = 20,
    Northampton = 1,
    Philadelphia = 4,
    Pike = 1,
    Washington = 1,
    Wayne = 1,
  ),
  "2020-03-15" = tibble( 
    Allegheny = 3, 
    Bucks = 4,
    Chester = 2,
    Cumberland = 5,
    Delaware = 7,
    Lehigh = 1,
    Luzerne = 1,
    Monroe = 6,
    Montgomery = 24,
    Northampton = 1,
    Philadelphia = 6,
    Pike = 1,
    Washington = 1,
    Wayne = 1,
  ),
  "2020-03-16" = read_text_table(
    "Allegheny 5
    Bucks 5
    Chester 2
    Cumberland 5
    Delaware 7
    Lehigh 1
    Luzerne 1
    Monroe 8
    Montgomery 30
    Northampton 1
    Philadelphia 8
    Pike 1
    Wayne 1
    Washington 1"
  ),
  "2020-03-17" = read_text_table(
    "Allegheny 7
    Beaver 1
    Bucks 8
    Chester 4
    Cumberland 10
    Delaware 9
    Lehigh 1
    Luzerne 1
    Monroe 8
    Montgomery 32
    Northampton 1
    Philadelphia 10
    Pike 1
    Washington 2
    Wayne 1"
  ),
  "2020-03-18" = read_text_table( ## deaths removes
    'Allegheny 11
    Beaver 2
    Berks 1
    Bucks 9
    Chester 9
    Cumberland 10
    Delaware 14
    Lackawanna 1
    Lehigh 1
    Luzerne 1
    Monroe 7
    Montgomery 42
    Northampton 1
    Philadelphia 17
    Pike 2
    Washington 2
    Wayne 1
    York 2'
  ),
  "2020-03-19" = read_text_table(
    "Adams  1
    Allegheny 16
    Beaver 2
    Berks 1
    Bucks 12
    Chester 10
    Cumberland 11
    Delaware 14
    Lackawanna 2
    Lancaster 2
    Lebanon 1
    Lehigh 1
    Luzerne 1
    Monroe 15
    Montgomery 47
    Northampton 5
    Philadelphia 33
    Pike 3
    Washington 3
    Wayne 1
    Westmoreland 2
    York 2"
  ),
  "2020-03-20" = read_text_table(
    "Adams 4
    Allegheny 28
    Beaver 3
    Berks 5
    Bucks	16
    Centre 1
    Chester	17
    Cumberland	11
    Delaware 23
    Erie 1
    Franklin 1
    Lackawanna 4
    Lancaster 2
    Lebanon 1
    Lehigh 2
    Luzerne 2
    Monroe 19
    Montgomery 59
    Northampton	10
    Philadelphia 42
    Pike 2
    Potter 1
    Washington 3
    Wayne 1
    Westmoreland 4
    York 6"
  ),
  "2020-03-21" = read_text_table(
    'Adams 4
    Allegheny 31
    Beaver 3
    Berks 7
    Bucks 24
    Butler 1
    Centre 1
    Chester 19
    Cumberland 11
    Delaware 33
    Erie 1
    Franklin 1
    Lackawanna 5
    Lancaster 4
    Lebanon 2
    Lehigh 13
    Luzerne 6
    Monroe 25
    Montgomery 71
    Montour 1
    Northampton 17
    Philadelphia 69
    Pike 2
    Potter 1
    Washington 5
    Wayne 1
    Westmoreland 4
    York	9'
  ),
  "2020-03-22" = read_text_table(
    'Adams 5
    Allegheny 40
    Beaver 3
    Berks 13
    Bucks 32
    Butler 1
    Centre 1
    Chester 23
    Columbia 1
    Cumberland 11
    Dauphin 1 
    Delaware	43
    Erie 2
    Fayette 1
    Franklin 1
    Lackawanna 6
    Lancaster 6
    Lebanon 3
    Lehigh 19
    Luzerne 7
    Mercer 1
    Monroe 31
    Montgomery 87
    Montour 1
    Northampton 21
    Philadelphia 91
    Pike 3
    Potter 1
    Schuylkill 1
    Washington 7
    Wayne 2
    Westmoreland 4
    York 10'
  ),
  "2020-03-23" = read_text_table(
    'Adams 6
    Allegheny 48
    Beaver 3
    Berks 14
    Bucks 43
    Butler 5
    Cambria 1
    Centre 3
    Chester 40
    Columbia 1
    Cumberland 12
    Dauphin 1
    Delaware 54
    Erie 3
    Fayette 1
    Franklin 1
    Lackawanna 7
    Lancaster 5
    Lebanon 3
    Lehigh 25
    Luzerne 10
    Mercer 1
    Monroe 43
    Montgomery 129
    Montour 1
    Northampton 23
    Philadelphia 128
    Pike 3
    Potter 1
    Schuylkill 3
    Washington 7
    Wayne 3
    Westmoreland 6
    York 10'
  ),
  "2020-03-24" = read_text_table(
    'Adams 6
    Allegheny 58
    Armstrong 1
    Beaver 3
    Berks 16
    Bradford 1 
    Bucks 65
    Butler 6
    Cambria 1
    Carbon 1
    Centre 7 
    Chester 40
    Clearfield 1
    Columbia 1 
    Cumberland 13
    Dauphin 4 
    Delaware 84
    Erie 4
    Fayette 2
    Franklin 3
    Juniata 1
    Lackawanna 15
    Lancaster 10
    Lebanon 3 
    Lehigh 27 
    Luzerne 21
    Mercer 2 
    Monroe 45
    Montgomery 144
    Montour 3 
    Northampton 33
    Philadelphia 177
    Pike 4
    Potter 1
    Schuylkill 5
    Somerset 1
    Washington 9
    Wayne 4
    Westmoreland 11
    York 18'
  ),
  "2020-03-25" = read_text_table(
    'Adams 6
    Allegheny 88
    Armstrong 1
    Beaver 7
    Berks 20
    Bradford 1
    Bucks 86
    Butler 12
    Cambria 1
    Carbon 1
    Centre 8
    Chester 54
    Clearfield 2
    Columbia 1
    Cumberland 13
    Dauphin 10
    Delaware 101
    Erie 4
    Fayette 4
    Franklin 5
    Greene 2
    Juniata 1
    Lackawanna 18
    Lancaster 12
    Lawrence 1
    Lebanon 3
    Lehigh 38
    Luzerne 27
    Lycoming 1
    Mercer 2
    Monroe 51
    Montgomery 172
    Montour 4
    Northampton 44
    Philadelphia 257
    Pike 9
    Potter 1
    Schuylkill 6
    Somerset 2
    Warren 1
    Washington 10
    Wayne 4
    Westmoreland 16
    York 20'
  ),
  "2020-03-26" = read_text_table(
    'Adams	7
    Allegheny	133
    Armstrong	1
    Beaver	13
    Berks	36
    Blair	1
    Bradford	2
    Bucks	107
    Butler	19
    Cambria	1
    Carbon	2
    Centre	9
    Chester	84
    Clearfield	2
    Columbia	3
    Crawford	1
    Cumberland	15
    Dauphin	13
    Delaware	156
    Erie	4
    Fayette	8
    Franklin	5
    Greene	3
    Indiana	1
    Juniata	1
    Lackawanna	28
    Lancaster	21
    Lawrence	1
    Lebanon	4
    Lehigh	63
    Luzerne	36
    Lycoming	1
    Mercer	3
    Monroe	67
    Montgomery	282
    Montour	4
    Northampton	56
    Philadelphia	402
    Pike	15
    Potter	1
    Schuylkill	9
    Somerset	2
    Susquehanna	1
    Warren	1
    Washington	12
    Wayne	6
    Westmoreland	24
    York 	21', sep = "\t"
  ),
  "2020-03-27" = read_text_table(
    'Adams	8
    Allegheny	158
    Armstrong	1
    Beaver	14
    Berks	65
    Blair	1
    Bradford	2
    Bucks	124
    Butler	26
    Cambria	1
    Carbon	2
    Centre	11
    Chester	107
    Clearfield	2
    Columbia	3
    Crawford	1
    Cumberland	16
    Dauphin	18
    Delaware	185
    Erie	7
    Fayette	9
    Franklin	5
    Greene	4
    Indiana	2
    Juniata	1
    Lackawanna	35
    Lancaster	33
    Lawrence	4
    Lebanon	12
    Lehigh	93
    Luzerne	55
    Lycoming	2
    Mercer	4
    Monroe	98
    Montgomery	374
    Montour	4
    Northampton	79
    Northumberland	1
    Philadelphia	530
    Pike	23
    Potter	1
    Schuylkill	13
    Somerset	2
    Susquehanna	1
    Union	1
    Warren	1
    Washington	14
    Wayne	6
    Westmoreland	30
    York	29', sep = "\t"
  ),
  "2020-03-28" = read_text_table(
    'Adams	8
    Allegheny	219
    Armstrong	2
    Beaver	22
    Berks	65
    Blair	2
    Bradford	3
    Bucks	152
    Butler	41
    Cambria	1
    Cameron	1
    Carbon	3
    Centre	15
    Chester	116
    Clarion	1
    Clearfield	2
    Columbia	4
    Crawford	2
    Cumberland	22
    Dauphin	23
    Delaware	226
    Erie	7
    Fayette	10
    Franklin	7
    Greene	6
    Huntingdon	1
    Indiana	2
    Juniata	1
    Lackawanna	51
    Lancaster	45
    Lawrence	8
    Lebanon	15
    Lehigh	109
    Luzerne	65
    Lycoming	2
    Mckean	1
    Mercer	6
    Monroe	106
    Montgomery	411
    Montour	5
    Northampton	94
    Northumberland	1
    Perry	1
    Philadelphia	709
    Pike	27
    Potter	2
    Schuylkill	16
    Snyder	1
    Somerset	2
    Susquehanna	1
    Tioga	1
    Warren	1
    Washington	23
    Wayne	6
    Westmoreland	41
    York	37', sep = "\t"
  ),
  "2020-03-29" = read_text_table(
    'Adams	8
    Allegheny	265
    Armstrong	3
    Beaver	28
    Berks	68
    Blair	3
    Bradford	3
    Bucks	203
    Butler	47
    Cambria	1
    Cameron	1
    Carbon	9
    Centre	22
    Chester	137
    Clarion	1
    Clearfield	2
    Columbia	6
    Crawford	3
    Cumberland	22
    Dauphin	35
    Delaware	276
    Erie	7
    Fayette	10
    Franklin	11
    Greene	6
    Huntingdon	1
    Indiana	2
    Juniata	1
    Lackawanna	56
    Lancaster	67
    Lawrence	8
    Lebanon	19
    Lehigh	151
    Luzerne	94
    Lycoming	3
    Mckean	1
    Mercer	7
    Mifflin	2
    Monroe	135
    Montgomery	488
    Montour	4
    Northampton	126
    Northumberland	1
    Perry	1
    Philadelphia	865
    Pike	33
    Potter	2
    Schuylkill	21
    Snyder	2
    Somerset	2
    Susquehanna	1
    Tioga	1
    Venango	1
    Warren	1
    Washington	24
    Wayne	7
    Westmoreland	47
    York	43', sep = "\t"
  ),
  "2020-03-30" = read_text_table(
    'Adams	8	
    Allegheny	290	2
    Armstrong	3	
    Beaver	44	
    Berks	82	
    Blair	6	
    Bradford	3	
    Bucks	246	4
    Butler	49	2
    Cambria	2	
    Cameron	1	
    Carbon	13	
    Centre	24	
    Chester	146	
    Clarion	1	
    Clearfield	4	
    Columbia	6	
    Crawford	4	
    Cumberland	24	1
    Dauphin	36	
    Delaware	303	4
    Erie	13	
    Fayette	11	
    Franklin	12	
    Greene	7	
    Huntingdon	1	
    Indiana	2	
    Juniata	3	
    Lackawanna	62	2
    Lancaster	97	2
    Lawrence	10	1
    Lebanon	27	
    Lehigh	231	3
    Luzerne	150	3
    Lycoming	4	
    Mckean	1	
    Mercer	7	
    Mifflin	1	
    Monroe	182	7
    Montgomery	540	5
    Montour	10	
    Northampton	184	5
    Northumberland	1	
    Perry	1	
    Philadelphia	1007	7
    Pike	39	1
    Potter	2	
    Schuylkill	30	
    Snyder	2	
    Somerset	2	
    Susquehanna	1	
    Tioga	1	
    Union	4	
    Venango	1	
    Warren	1	
    Washington	26	
    Wayne	10	
    Westmoreland	55	
    York	54	', sep = "\t"
  ),
  "2020-03-31" = read_text_table(
    'Adams	9
    Allegheny	325
    Armstrong	5
    Beaver	52
    Bedford	2
    Berks	110
    Blair	4
    Bradford	7
    Bucks	286
    Butler	60
    Cambria	2
    Cameron	1
    Carbon	17
    Centre	26
    Chester	159
    Clarion	3
    Clearfield	4
    Columbia	7
    Crawford	4
    Cumberland	36
    Dauphin	45
    Delaware	338
    Erie	14
    Fayette	14
    Franklin	19
    Greene	9
    Huntingdon	1
    Indiana	6
    Juniata	3
    Lackawanna	78
    Lancaster	123
    Lawrence	13
    Lebanon	28
    Lehigh	272
    Luzerne	212
    Lycoming	6
    Mckean	1
    Mercer	8
    Mifflin	2
    Monroe	236
    Montgomery	570
    Montour	9
    Northampton	245
    Northumberland	1
    Perry	1
    Philadelphia	1197
    Pike	48
    Potter	2
    Schuylkill	38
    Snyder	2
    Somerset	2
    Susquehanna	1
    Tioga	2
    Union	4
    Venango	3
    Warren	1
    Washington	33
    Wayne	10
    Westmoreland	61
    York	66', sep = "\t"
  )
) %>% 
  map(mutate_all, as.integer) %>% 
  enframe("date", "confirmed") %>% 
  unnest("confirmed") %>% 
  map_dfr(replace_na, 0L) %>%
  mutate_at("date", as.Date)


# County populations --------------------------------------------------------------------------

file <- "docs/PEP_2018_PEPANNRES_with_ann.csv"

text <- read_csv(file, col_names = FALSE)[1:2,]
temp <- read_csv(file, skip = 2, col_names = FALSE)
names(temp) <- text[1,] %>% unlist(use.names = TRUE) %>% make_clean_names()
Hmisc::label(temp) <- text[2, ]

pa_county_population <- temp %>% 
  select(county = geo_display_label,
         estimate_population = respop72018) %>% 
  mutate(county = str_remove(county, "\\sCounty,\\sPennsylvania$"))

library(usethis)

use_data(pa_county_confirmed, overwrite = TRUE)
use_data(pa_county_population, overwrite = TRUE)


# Some graphs ---------------------------------------------------------------------------------

pa_counties <- pa_county_confirmed %>% 
  pivot_longer(col = -date,
               names_to = "county",
               values_to = "cs_cases")

ggplot(pa_counties,
       aes(x = date, y = cs_cases, col = county)) +
  geom_line() +
  geom_smooth(method = "loess",
              aes(group = "model"))


# x <- double(10)
# x[1] <- 20
# 
# for(i in seq_along(x)[-1]) {
#   # x[i] <- x[i-1] * sqrt(2)
#   x[i] <- x[i-1] * 1.33
# }
# 
# plot(x)

pa_counties %>% 
  filter(cs_cases >= 100) %>% 
  group_by(county) %>% 
  mutate(n_days = row_number()) %>% 
  ggplot(aes(x = n_days, y = cs_cases, col = county)) +
  scale_y_log10() +
  geom_smooth(method = "lm",
              se = FALSE,
              formula = "y ~ x",
              aes(group = "fit")) +
  # geom_smooth(method = "lm",
  #             se = FALSE,
  #             linetype = 2,
  #             formula = "y ~ log10(x)",
  #             aes(group = "estimate")) +
  geom_line() +
  labs(x = "Number of days since 100th case",
       y = "Cumulative cases")
plotly::ggplotly()

penn <- pa_counties %>% 
  group_by(date) %>% 
  summarise(cases = sum(cs_cases))

penn_ts <- ts(penn$cases, start = penn$date[1])
n <- length(penn_ts)

library(forecast)

## prediction based on last two weeks
mod1 <- ets(log10(penn_ts[seq(1, n)]), model = "AAN", damped = FALSE, additive.only = FALSE)
mod2 <- ets(log10(penn_ts[seq(1, n)]), model = "AAN", damped = TRUE, additive.only = FALSE)

forecast_h <- 30
res1 <- forecast(mod1, h = forecast_h)
res2 <- forecast(mod2, h = forecast_h)
# res$series
# max(penn$date)
# plot(res)
res_data1 <- cbind(date = c(max(penn$date) + seq(forecast_h)),
      res1$lower %>% sapply(function(x) 10^x) %>% as_tibble %>% set_names(c("lower_80", "lower_90")),
      mean = sapply(res1$mean, function(x) 10^x, simplify = FALSE) %>% unlist(),
      res1$upper %>% sapply(function(x) 10^x) %>% as_tibble %>% set_names(c("upper_80", "upper_90"))) %>% 
  mutate_if(is.double, round)

res_data2 <- cbind(date = c(max(penn$date) + seq(forecast_h)),
                  res2$lower %>% sapply(function(x) 10^x) %>% as_tibble %>% set_names(c("lower_80", "lower_90")),
                  mean = sapply(res2$mean, function(x) 10^x, simplify = FALSE) %>% unlist(),
                  res2$upper %>% sapply(function(x) 10^x) %>% as_tibble %>% set_names(c("upper_80", "upper_90"))) %>% 
  mutate_if(is.double, round)
res_data1 <- res_data1[seq(14), ]
res_data2 <- res_data2[seq(14), ]

ggplot(penn, aes(x = date, y = cases)) +
  geom_line(size = 1.5) +
  geom_ribbon(data = res_data1,
              fill = "red",
              alpha = .15,
              aes(x = date,
                  y = NULL,
                  ymin = lower_90,
                  ymax = upper_90)) +
  geom_ribbon(data = res_data1,
              fill = "red",
              alpha = .3,
              aes(x = date,
                  y = NULL,
                  ymin = lower_80,
                  ymax = upper_80)) +
  geom_point(data = res_data1,
            size = 2,
            color = "darkred",
            aes(x = date, y = mean)) +
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
  scale_y_log10() +
  geom_hline(yintercept = 34000, linetype = 2) +
  theme_bw() +
  labs(title = "Forecast for confirmed COVID-19 cases on Pennsylvania",
       subtitle = paste0("Estimates with 90% and 80% confidence intervals\n",
                         "At 34k cases, assuming 10% cases need ICU,",
                         " all estimate 3400 ICU beds will be filled"),
       caption = "Data are first transformed to log10 values for forecast",
       x = "Date",
       y = "Number of confirmed cases")
plotly::ggplotly(dynamicTicks = TRUE, originalData = FALSE)
