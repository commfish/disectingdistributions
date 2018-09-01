# notes ----
# dissecting distributions
# sarah.power@alaska.gov
# 8/13/2018

# load ----
library(tidyverse)
library(mixdist)
library(lubridate)
library(zoo) # to convert numeric date back to a number
source('code/distribution_functions.R')

# data ----
read_csv('data/ChigISGrunappt2006-2017.csv') %>% 
  dplyr::select(-X9) %>%
  rename(prop_early_genetics = Propotionearly, 
         early_esc_genetics = Earlyesc, 
         late_esc_genetics = Lateesc,
         early_catch_genetics = Earlycatch,
         late_catch_genetics = Latecatch,
         early_total_genetics = Earlytotal,
         late_total_genetics = Latetotal) %>%
  mutate(esc = early_esc_genetics + late_esc_genetics,
         catch = early_catch_genetics + late_catch_genetics,
         run = esc + catch, #catch is our estimate of what would have occured at the wier had there been no fishing, based on migration timing studies.
         run_early_gen = prop_early_genetics*run,
         date = mdy(Date),
         day_of_year = yday(date)) -> weir_data # convert the Date to its numeric equivalent


early_look(weir_data, 2006) 
early_look(weir_data, 2007) 
early_look(weir_data, 2008) 
early_look(weir_data, 2010) 
early_look(weir_data, 2011) 
early_look(weir_data, 2012) 
early_look(weir_data, 2013) 
early_look(weir_data, 2014) 
early_look(weir_data, 2015) 
early_look(weir_data, 2016) 
early_look(weir_data, 2017) 

auto_year(weir_data, 2006)
auto_year(weir_data, 2007)
auto_year(weir_data, 2008)
auto_year(weir_data, 2010)
auto_year(weir_data, 2011)
auto_year(weir_data, 2012)
auto_year(weir_data, 2013)
auto_year(weir_data, 2014)
auto_year(weir_data, 2015)
auto_year(weir_data, 2016)
auto_year(weir_data, 2017)

#July 4th was a historic cut off between the early and later runs. 
#Which is day 185 or 186 if it is a leap year.
yday("2017-07-04")


year_stats(weir_data, 2006)
year_stats(weir_data, 2007)
year_stats(weir_data, 2008)
year_stats(weir_data, 2010)
year_stats(weir_data, 2011)
year_stats(weir_data, 2012)
year_stats(weir_data, 2013)
year_stats(weir_data, 2014)
year_stats(weir_data, 2015)
year_stats(weir_data, 2016)
year_stats(weir_data, 2017)


#2006 in detail
weir_2006 <- data_prep(weir_data, 2006)
#write.csv(weir_2006, file = "data/weir_2006.csv")

weir_2006<- as.mixdata(weir_2006)
(fit <- mix(as.mixdata(weir_2006), mixparam(mu=c(175, 205, 240), sigma= c(10,10,10)),constr = mixconstr(consigma="SEQ"), dist= 'gamma', iterlim=5000)) 


dist_plot(fit, 2006)
percent_dist(fit, 191) #date when fifty percent is early run. According to distributions: July 10 

year_stats(weir_data, 2006)
weir_2006$dist_percent <- percent_dist(fit, weir_2006$day_of_year)
weir_2006$run_dis <- weir_2006$dist_percent*weir_2006$run
weir_2006$cum <- cumsum(weir_2006$run_dis)


percent_dist(fit, 178) #date when fifty percent is early run. According to distributions? 
pnormfit(fit, 178)










