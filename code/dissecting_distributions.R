# notes ----
# dissecting distributions
# sarah.power@alaska.gov
# 8/13/2018

# load ----
library(tidyverse)
library(mixdist)
library(lubridate)
source('code/distribution_functions.R')

# data ----
read_csv('data/ChigISGrunappt2006-2017.csv') %>% 
  dplyr::select(-X9) %>%
  rename(date = Date, 
         prop_early_genetics = Propotionearly, 
         early_esc_genetics = Earlyesc, 
         late_esc_genetics = Lateesc,
         early_catch_genetics = Earlycatch,
         late_catch_genetics = Latecatch,
         early_total_genetics = Earlytotal,
         late_total_genetics = Latetotal) %>%
  mutate(esc = early_esc_genetics + late_esc_genetics,
         catch = early_catch_genetics + late_catch_genetics,
         date = mdy(date),
         date_num = as.numeric(date)) # convert the Date to its numeric equivalent
-> weir_data 



# Filter out just on years data.
weir_data %>% 
  filter(year(date)==2006) -> weir_2006

graph_year(weir_2006)
distribution_estimation(weir_2006)






