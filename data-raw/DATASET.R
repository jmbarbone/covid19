
# ---------------------------------------------------------------------------------------------
# Load in census population data
# ---------------------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

file <- "docs/nst-est2019-01.xlsx"

state_populations <- read_xlsx(file,
                               range = "A10:M62",
                               col_names = FALSE) %>% 
  drop_na() %>% 
  clean_names() %>% 
  select(state = x1,
         population_estimate = x13) %>% 
  mutate(state = str_remove(state, "^\\."),
         state = recode(state, "Washington, D.C." = "District of Columbia"))
  
usethis::use_data(state_populations, overwrite = TRUE)
