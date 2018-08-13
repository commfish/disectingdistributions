# notes ----
# disecting distributions
# sarah.power@alaska.gov
# 8/13/2018

# load ----
library(tidyverse)
library(mixdist)
library(here)
#source('code/functions.R')

# data ----
weir_data <- read_csv('data/ChigISGrunappt2006-2017.csv') %>% 
  dplyr::select(-X9) %>%
  dplyr::rename(date = Date, 
                prop_early_genetics = Propotionearly, 
                early_esc_genetics = Earlyesc, 
                late_esc_genetics = Lateesc,
                early_catch_genetics = Earlycatch,
                late_catch_genetics = Latecatch,
                early_total_genetics = Earlytotal,
                late_total_genetics = Latetotal)%>%
  dplyr::mutate(esc = early_esc_genetics + late_esc_genetics,
                catch = early_catch_genetics + late_catch_genetics)

glimpse(weir_data)

