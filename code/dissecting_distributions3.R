# notes ----
# dissecting distributions
# sarah.power@alaska.gov
# 8/13/2018

# load ----
library(fpp2)
library(RDS)
library(tidyverse)
library(mixdist)
library(lubridate)
library(zoo) # to convert numeric date back to a number
source('code/distribution_functions.R')

# data ----
# Chig_data only uses Chignik Bay District harvest
data2018 <- read_csv('data/ChigISGrunappt2006-2017catch.by.district9-3-19.csv') %>% 
  dplyr::select(-Earlycatch, -Latecatch, -Earlytotal, -Latetotal) %>%
  dplyr::rename(
    prop_early_genetics = Propotionearly, 
    early_esc_genetics = Earlyesc, 
    late_esc_genetics = Lateesc) %>%
  mutate(esc = early_esc_genetics + late_esc_genetics,
         date = mdy(Date),
         year = year(date),
         day_of_year = yday(date)) # %>% #-> chig_data # convert the Date to its numeric equivalent 

#Defining harvest Change df_data to the one you want to runt through. 
df_data <- df18lagoon <- data2018 %>% mutate(harvest = lagoon_272_10)
df18ocdb_272_20 <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20)
df18ocdb_272_30 <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20 + ocdb_272_30)

#working on this 
post_harvest <- function(df = data2018){
  df <- df %>% 
    mutate(run = esc + harvest, #catch is our estimate of what would have occured at the wier had there been no fishing, based on migration timing studies.
           run_early_gen = prop_early_genetics*run) %>%
    select(-c(lagoon_272_10:sedm80))
  return(df)
}

df_data <- post_harvest(df_data) 

#not working use loops
df_d <- df_data %>%
  filter(year > 2006) %>%  # 2006 not working. need to check data. remove for now.
  group_split(year) %>%
  purrr::map_df(~ early_look(., year_wanted = year))

auto_year(weir_data, 2006)


year_vector <- c(2007:2008,2010:2018)

out <- list()
#p <-par(mfrow = c(2,5))
for(i in 1:length(year_vector) ){
  png(file = paste0("figures/early_genetics", year_vector[i], ".png"), height = 4, width = 6, units = "in", res = 300)
  out[[i]] <- early_look(df_data, year_vector[i])
  dev.off()
}
#par(p)
eout <- do.call("rbind", out)

auto_year(df_data, 2007)

year_vector <- c(2014:2017)
out <- list()
#par(mfrow = c(2,3))
for(i in 1:length(year_vector) ){
  png(file = paste0("figures/mixture_auto", year_vector[i], ".png"), height = 4, width = 6, units = "in", res = 300)
  #png(file = paste0("figures/mixture_auto", year_vector[i], ".png")) #, height = 4, width = 6, units = "in", res = 300)
  auto_year(df_data, year_vector[i])
  dev.off()
}
par(p)
aout <- do.call("rbind", out)

out <- list()
for(i in year_vector ){
  year_stats(df_data, i)
}
year_stats(df_data, 2007)

out <- list()
for(i in 1:length(year_vector) ) {
  png(file = paste0("figures/year_stats", year_vector[i], ".png"), height = 4, width = 6, units = "in", res = 300)
  year_stats(df_data, year_vector[i])
  dev.off()
}
yout <- do.call("rbind", out)

year_stats(df_data, 2006)
year_stats(df_data, 2007)
year_stats(df_data, 2008)
year_stats(df_data, 2010)
year_stats(df_data, 2011)

e06 <- early_look(df_data, 2006) #06 not working, perhaps check data 

