# notes ----
#Determining possible harvest allocation schemes which should help with run timing estimates of early run transitional timing.
# sarah.power@alaska.gov
# 10/29/2018

# load ----
library(tidyverse)
library(mixdist)
library(data.table)
library(lubridate)
library(here)

# data ----
# load ----
source('distribution_functions.R')
#library(lubridate)
getwd()

# data ----
chig_data <-read_csv('../data/ChigISGrunappt2006-2017catch.by.district.csv') %>% 
  dplyr::select( Date, Propotionearly, Earlyesc, Lateesc) %>%
  dplyr::rename(
    date = Date,
    prop_early_genetics = Propotionearly, 
    early_esc_genetics = Earlyesc, 
    late_esc_genetics = Lateesc
  ) %>%
  mutate(esc = early_esc_genetics + late_esc_genetics,
         date_weir = lubridate::mdy(date),
         date = date_weir,
         day_of_year = lubridate::yday(date_weir))%>% # convert the Date to its numeric equivalent
  filter(year(date_weir) %in% c(2006:2016))

harvest_data <- read_csv('../data/harvest19692017.csv') %>% 
  filter(year %in% c(2006:2016)) %>%
  mutate(date_harvest = mdy(date_begin),
         day_of_year_harvest = yday(date_harvest),
         date_weir = weir_date(area, date_harvest), 
         allocation_percent = allocation(area, date_harvest),
         chig_catch = round(fish_count*allocation_percent, 0),
         month_of_weir = month(date_weir)
  )


# This section to figure out patterns of harvest an what might be different about 2008 and 2015 in particular
#with(harvest_data, table(month_of_weir,year, area ))
unique(harvest_data$area)

xyz <- harvest_data %>% 
  #filter(area %in% c("SEDM")) %>% #, "Western")
  group_by(year, month_of_weir) %>%
  summarise(month_harv = sum(chig_catch)) %>%
  mutate(month_of_weir=paste('m', month_of_weir, sep="_")) 

year_harv <- harvest_data %>%
  group_by(year) %>%
  summarise(year_harv = sum(chig_catch))

xyz <- merge(xyz, year_harv, by = "year") 

#class(xyz)
xyz <- xyz %>%
  mutate(permonth_harv = month_harv/year_harv) %>%
  select(year, month_of_weir, permonth_harv) %>%
  spread(month_of_weir, permonth_harv)


xyz2 <-harvest_data %>% 
  group_by(year, month_of_weir) %>%
  summarise(month_harv = sum(fish_count))
###end section

#create a wide data set
harv <- harvest_data %>% 
  dplyr::select(date_weir, year, month_of_weir, area, chig_catch) %>% 
  tidyr::spread(area, chig_catch)

catch <-aggregate( chig_catch ~ date_weir, data=harvest_data, FUN=sum)

harv <- harv %>% 
  full_join(catch, by = "date_weir") #catch is our estimate of what would have occured at the wier had there been no fishing, based on migration timing studies.

harv$chig_catch%>% 
  replace_na(0)

#Some harvest would have arrived at the weir after the weir is removed, or when the weir was down.
something <-anti_join(harv, chig_data, by ="date_weir") #most of this is 2009 data since we don't have 2009 weir data

all <- full_join(chig_data, harv, by = "date_weir") %>%
  mutate(day_of_year = yday(date_weir))

#need to replace na with 0 so we can sum the columns
all$chig_catch <- all$chig_catch %>% 
  replace_na(0)

all$esc <- all$esc %>% 
  replace_na(0)

all <- all%>%
  mutate(run = chig_catch + esc,
         run_early_gen = prop_early_genetics*run)



year_stats(all, 2006)
year_stats(all, 2007)
year_stats(all, 2008)

year_stats(all, 2010)
year_stats(all, 2011)
year_stats(all, 2012)
year_stats(all, 2013)
year_stats(all, 2014)
year_stats(all, 2015)
year_stats(all, 2016)

year_vector <- c(2006:2008,2010:2017)