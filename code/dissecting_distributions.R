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
  rename(#date = Date,
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
plot(fit)

dist_plot(fit, 2006)
percent_dist(fit, 191) #date when fifty percent is early run. According to distributions: July 10 

year_stats(weir_data, 2006)
weir_2006$dist_percent <- percent_dist(fit, weir_2006$day_of_year)
weir_2006$run_dis <- weir_2006$dist_percent*weir_2006$run
weir_2006$cum <- cumsum(weir_2006$run_dis)


percent_dist(fit, 178) #date when fifty percent is early run. According to distributions? 
pnormfit(fit, 178)


year_stats <- function (df, year_wanted){
  df %>%
    filter(year(date)==year_wanted)-> df
  run_size <-sum(df$run)
  print("Run")
  print(run_size)
  df_fit <- data_prep(df, year_wanted)
  fit <- distribution_estimation_norms_SEQ(df_fit) 
  dist_plot (fit, year_wanted)
  df_fit_early <- data_prep_early(df, year_wanted)
  fit_early <- distribution_estimation_norms(df_fit_early)
  fit <- mix(as.mixdata(df), mixparam(mu=mean(df$day_of_year), sigma=sd(df$day_of_year)), dist="gamma", iterlim=5000)
  print("Dist mean & sd")
  print(c(fit$parameters$mu[1], fit$parameters$sigma[1]))
  print("Gen mean & sd")
  print(c(fit_early$parameters$mu[1], fit_early$parameters$sigma[1]))
  df %>%
    mutate(dist_percent = percent_dist(fit, df$day_of_year),
           run_early_dis = dist_percent*run,
           cum_run_dis = cumsum(run_early_dis),
           cum_run_gen = cumsum(run_early_gen)) ->df
  
  ggplot(df, aes(day_of_year))+
    geom_line(aes(y=dist_percent), colour = "green")+
    geom_line(aes(y=prop_early_genetics), colour = "blue")+
    ggtitle("Genetics vs Distributional Runtiming Assignment")
  
  ggplot(df, aes(day_of_year))+
    geom_line(aes(y=cum_run_dis), colour = "green")+
    geom_line(aes(y=cum_run_gen), colour = "blue")+
    labs(y = "cumulative run", x= "day of the year")+
    ggtitle("Genetics vs Distributional Early Run")
}







