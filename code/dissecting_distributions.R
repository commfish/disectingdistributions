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

#2006
weir_data %>% filter(year(date)==2006) -> weir_2006
graph_year(weir_2006)
distribution_estimation(weir_2006)

#2007
weir_data %>% filter(year(date)==2007) -> weir_2007
graph_year(weir_2007)
distribution_estimation(weir_2007)

#2008
weir_data %>% filter(year(date)==2008) -> weir_2008
graph_year(weir_2008)
distribution_estimation(weir_2008)
distribution_estimation(weir_2008, 3, mean_guess_given = c(14050, 14080, 14110 ))

auto_year(weir_data, 2008)

auto_year(weir_data, 2010)
auto_year(weir_data, 2011)
auto_year(weir_data, 2012)
auto_year(weir_data, 2013)
auto_year(weir_data, 2014)
auto_year(weir_data, 2015)
auto_year(weir_data, 2016)
