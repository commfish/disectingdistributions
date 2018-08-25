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
         run = esc + catch, #catch is our estimate of what would have occured at the wier had there been no fishing, based on migration timing studies.
         date = mdy(date),
         date_num = as.numeric(date)) -> weir_data # convert the Date to its numeric equivalent



#2006
weir_2006 <- data_prep(weir_data, 2006)
write.csv(weir_2006, file = "data/weir_2006.csv")
graph_year(weir_2006)
weir_2006<- as.mixdata(weir_2006)
(fit <- mix(as.mixdata(weir_2006), mixparam(mu=c(13335, 13355), sigma= c(10,10)), dist= 'gamma', iterlim=5000))  #didn't work well
(fit <- mix(as.mixdata(weir_2006), mixparam(mu=c(13335, 13355, 13380), sigma= c(10,10,20)), dist= 'gamma', iterlim=5000))  
(fit <- mix(as.mixdata(weir_2006), mixparam(mu=c(13335, 13355), sigma= c(15,15)), dist= 'weibull', iterlim=5000))  #didn't work
(fit <- mix(as.mixdata(weir_2006), mixparam(mu=c(13335, 13355, 13380), sigma= c(10,10,20)), dist= 'weibull', iterlim=5000))  

dist_plot(fit, 2006)
percent_dist(fit, 13336) #date when fifty percent is early run. According to distributions? 
weir_2006$dist_percent <- percent_dist(fit, weir_2006$date_num)

percent_dist(fit, 13697) #date when fifty percent is early run. According to distributions? 
pnormfit(fit, 13336)

#2007
weir_2007 <- data_prep(weir_data, 2007)
graph_year(weir_2007)
fit2 <- distribution_estimation_norms(weir_2007)
(fit <- mix(as.mixdata(weir_2007), mixparam(mu=c(13687, 13712,13749), sigma= c(10,8, 8)), dist= 'gamma', iterlim=5000)) 
#(fit <- mix(as.mixdata(weir_2007), mixparam(mu=c(13685, 13712, 13749), sigma= c(10,10,10)), dist= 'weibull', iterlim=5000)) #didn't work
#(fit <- mix(as.mixdata(weir_2007), mixparam(mu=c(13685, 13712), sigma= c(10,10)), dist= 'weibull', iterlim=5000)) #didn't work

dist_plot(fit, 2007)
percent_dist(fit, 13697) #date when fifty percent is early run. According to distributions? 
dnormfit(fit, 13697)

#2008
weir_2008 <- data_prep(weir_data, 2008)
graph_year(weir_2008)
fit <- distribution_estimation_norms(weir_2008) #THis didn't work very well
fit <- distribution_estimation(weir_2008, 3, mean_guess_given = c(14050, 14080, 14110 )) # This worked better
(fit <- mix(as.mixdata(weir_2008), mixparam(mu=c(14057, 14082, 14107), sigma= c(10,10,10)), dist= 'weibull', iterlim=5000)) #THis didn't work very well
(fit <- mix(as.mixdata(weir_2008), mixparam(mu=c(14057, 14082), sigma= c(10,10)), dist= 'weibull', iterlim=5000)) #THis didn't work very well

dist_plot(fit, 2008)
percent_dist(fit, 14069) #date when fifty percent is early run. According to distributions? 
dnormfit(fit, 14069)

#2010
weir_2010 <- data_prep(weir_data, 2010)
graph_year(weir_2010)
fit <- distribution_estimation_norms(weir_2010)
(fit <- mix(as.mixdata(weir_2010), mixparam(mu=c(14785, 14800, 14850), sigma= c(10,5,5)), dist= 'gamma', iterlim=10000)) #THis didn't work
(fit <- mix(as.mixdata(weir_2010), mixparam(mu=c(14785, 14800), sigma= c(10,10)), dist= 'gamma', iterlim=5000)) 
(fit <- mix(as.mixdata(weir_2010), mixparam(mu=c(14785, 14800), sigma= c(10,10)), dist= 'weibull', iterlim=5000)) ##THis didn't work
(fit <- mix(as.mixdata(weir_2010), mixparam(mu=c(14785, 14800, 14840), sigma= c(5,10,10)), dist= 'weibull', iterlim=10000)) #THis didn't work

dist_plot(fit, 2010)
percent_dist(fit, 14793) 

#2011
weir_2011 <- data_prep(weir_data, 2011)
graph_year(weir_2011)
fit <- distribution_estimation_norms(weir_2011)
(fit <- mix(as.mixdata(weir_2011), mixparam(mu=c(15140, 15170, 15210), sigma= c(10,5,5)), dist= 'gamma', iterlim=10000)) 
(fit <- mix(as.mixdata(weir_2011), mixparam(mu=c(15140, 15170), sigma= c(10,10)), dist= 'gamma', iterlim=10000)) #THis didn't work as well
(fit <- mix(as.mixdata(weir_2011), mixparam(mu=c(15140, 15170), sigma= c(10,10)), dist= 'weibull', iterlim=10000)) ##THis didn't work
(fit <- mix(as.mixdata(weir_2011), mixparam(mu=c(15140, 15170, 15210), sigma= c(10,10,10)), dist= 'weibull', iterlim=5000)) #THis didn't work

dist_plot(fit, 2011)
percent_dist(fit, 15154)

fit <- auto_year(weir_data, 2012)
percent_dist(fit, 15541)

auto_year(weir_data, 2013)
auto_year(weir_data, 2014)# two distributions?
#2014
weir_2014<- data_prep(weir_data, 2014)
graph_year(weir_2014)
fitpro <- distribution_estimation_norms(weir_2011)
(fit <- mix(as.mixdata(weir_2014), mixparam(mu=c(16240, 16270, 16300), sigma= c(10,20,5)), dist= 'gamma', iterlim=10000)) 
(fit <- mix(as.mixdata(weir_2014), mixparam(mu=c(16240, 16280), sigma= c(10,10)), dist= 'gamma', iterlim=10000)) #THis didn't work as well
(fit <- mix(as.mixdata(weir_2011), mixparam(mu=c(15140, 15170), sigma= c(10,10)), dist= 'weibull', iterlim=10000)) ##THis didn't work
(fit <- mix(as.mixdata(weir_2011), mixparam(mu=c(15140), sigma= c(10)), dist= 'weibull', iterlim=10000)) ##THis didn't work
(fit <- mix(as.mixdata(weir_2014), mixparam(mu=c(15140, 15170, 15210), sigma= c(10,10,10)), dist= 'weibull', iterlim=5000)) #THis didn't work

auto_year(weir_data, 2015)
auto_year(weir_data, 2016)
auto_year(weir_data, 2017)


