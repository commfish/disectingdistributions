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
chig_data <- read_csv('data/ChigISGrunappt2006-2017catch.by.district.csv') %>% 
  dplyr::select(-Earlycatch, -Latecatch, -Earlytotal, -Latetotal) %>%
  dplyr::rename(#date = Date,
         prop_early_genetics = Propotionearly, 
         early_esc_genetics = Earlyesc, 
         late_esc_genetics = Lateesc,
         #early_catch_genetics = Earlycatch,
         #late_catch_genetics = Latecatch,
         #early_total_genetics = Earlytotal,
         #late_total_genetics = Latetotal,
         chignik = Chignik_Hook_Kujulik) %>%
  mutate(esc = early_esc_genetics + late_esc_genetics,
         #catch = early_catch_genetics + late_catch_genetics,
         catch_chignik = Lagoon + chignik,
         run = esc + catch_chignik, #catch is our estimate of what would have occured at the wier had there been no fishing, based on migration timing studies.
         run_early_gen = prop_early_genetics*run,
         date = mdy(Date),
         day_of_year = yday(date)) #-> chig_data # convert the Date to its numeric equivalent

weir_data <- read_csv('data/ChigISGrunappt2006-2017.csv') %>% 
  dplyr::select(-X9) %>%
  dplyr::rename(prop_early_genetics = Propotionearly, 
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
         day_of_year = yday(date)) # %>% # convert the Date to its numeric equivalent

class(weir_data)
class(chig_data)
weir_data <- as.data.frame(weir_data)


year_vector <- c(2006:2008,2010:2017)

early_look(weir_data, 2006) 
early_look(chig_data, 2007) 
early_look(weir_data, 2008) 
early_look(weir_data, 2010) 
early_look(weir_data, 2011) 
early_look(weir_data, 2012) 
early_look(chig_data, 2012) 
early_look(weir_data, 2013) 
early_look(weir_data, 2014) 
early_look(chig_data, 2015) 
early_look(weir_data, 2016) 
early_look(weir_data, 2017) 


for(i in year_vector ){
  distribution_estimation_norms_SEQ_n1(chig_data, i)
}

for(i in year_vector ){
  early_look(chig_data, i)
}

for(i in year_vector ){
  auto_year(chig_data, i)
}
auto_year(weir_data, 2006)
auto_year(weir_data, 2007)
auto_year(chig_data, 2008)
auto_year(weir_data, 2010)
auto_year(weir_data, 2011)
auto_year(chig_data, 2012)
auto_year(weir_data, 2013)
auto_year(weir_data, 2014)
auto_year(chig_data, 2015)
auto_year(weir_data, 2016)
auto_year(weir_data, 2017)

#July 4th was a historic cut off between the early and later runs. 
#Which is day 185 or 186 if it is a leap year.
yday("2017-07-04")



for(i in year_vector){
  year_stats(chig_data, i)
}


year_stats(chig_data, 2006)
year_stats(chig_data, 2007)
year_stats(chig_data, 2008)
year_stats(chig_data, 2010)
year_stats(chig_data, 2011)
year_stats(chig_data, 2012)
year_stats(chig_data, 2013)
year_stats(chig_data, 2014)
year_stats(chig_data, 2015)
year_stats(chig_data, 2016)
year_stats(chig_data, 2017)


#2006 in detail
weir_2006 <- data_prep(weir_data, 2008)
#write.csv(weir_2006, file = "data/weir_2006.csv")

weir_2006<- as.mixdata(weir_2006)
(fit <- mix(as.mixdata(weir_2006), mixparam(mu=c(175, 205, 240), sigma= c(10,10,10)),constr = mixconstr(consigma="SEQ"), dist= 'gamma', iterlim=5000)) 
(fit<- mix(as.mixdata(weir_2006), mixparam(mu=c(175, 205, 240), sigma= c(10,10,10)), constr = mixconstr(consigma="SEQ"), dist='weibull'))  #, iterlim=5000
mean_guess <- c(fit$parameters$mu[1],fit$parameters$mu[2],fit$parameters$mu[3])
plot(fit)
fit$parameters$mu[1]-1

dist_plot(fit, 2006)
percent_dist(fit, 191) #date when fifty percent is early run. According to distributions: July 10 

year_stats(weir_data, 2006)
weir_2006$dist_percent <- percent_dist(fit, weir_2006$day_of_year)
weir_2006$run_dis <- weir_2006$dist_percent*weir_2006$run
weir_2006$cum <- cumsum(weir_2006$run_dis)


percent_dist(fit, 178) #date when fifty percent is early run. According to distributions? 
pnormfit(fit, 178)


# Delay/Lag from Witteveen and Botz 2007

#Location		                Delay 		Stat Area		              Time Period	

#SEDM		                     5	6	  28115-28190		              65% June, 55% July, 50% Aug., 35% Sept. 	
#SEDM		                     5	6	  28115-28130, 28170-28190		65% June, 55% July, 50% Aug., 35% Sept. 
#Perryville District		     3	4	  27540-27560		              50% June, 60% July, 50% Aug., 35% Sept.
#Western District		         2	3	  27330-27394		              50% June, 60% July, 50% Aug., 35% Sept.
#Outer Chignik Bay/Kujulik	 1	2	  27220-27250		              90% June, 95% rest of Season	
#Cape Kumlik		             2	3	  27262-27264		              90% June, 95% rest of Season
#Chignik Bay District        0	1	  27110		                    100% All Season	
#Weir Count		              -1	0				                        100% All Season
#Eastern District		         3	4   27260, 27270-27296		      75% June, 20% July - September	
#Cape Igvak		               5	6	  26275-26295		              75% June, 20% July - September

# %ages to approximated from GSI
#
#1. Chignik managment harvest (yellow in the spreadsheet)
#2. 90% of Igvak harvest (through 7/25)
#3. and 80% of SEDM harvest through 7/25 (excludes NWSS 7/1-7/25)

#outline
# For each year 2006 - 2008 
#Start by getting weir data.
#For each fishing district starting with closest districts and working further away:
#   Add % of harvest to weir and previously added districts.
#   Estimate early run
#   See how well early run estimation matches up with genetics. 
#If addtional harvest makes early run estimation less similar - add on next district - if it still makes early run estiation less similar to genetics estimation then don't add any more areas. 




