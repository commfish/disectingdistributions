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
graph_year(weir_2006)
 fit <-fitpro <- distribution_estimation_norms(weir_2006)
#fitpro <- mix(weir_2006,mixparam(c(13325, 13350, 13380),.5),"weibull")
#(fitpro <- mix(as.mixdata(weir_2006), mixparam(mu=c(13325, 13350, 13380), sigma=c(10,15,10)), dist="norm")) 
dist_plot(fitpro)
summary(fit)
fit <-fitpro <- distribution_estimation_weibull(weir_2006, 2, mean_guess_given = c(13325, 13350, 13380), c(10,10,10),  distibution_guess = 'weibull')
distribution_estimation_weibull <- function(df, num_of_distributions = 2, mean_guess_given , sigma_guess_given, distibution_guess = 'weibull')
  fit <-fitpro <- distribution_estimation_weibull(weir_2006)

pnormfit(fit, 13338, 2)
pnormfit(fit, 13338, 1)
1-pnormfit(fit, 13338, 1)

tails_difference(fit, 13338, 1, 2)

percent_dist <- function(fit, x, dist_num = 1){
  dnormfit(fit, x, dist_num)/(dnormfit(fit, x, 1)+dnormfit(fit, x, 2)+dnormfit(fit, x, 3)) 
}

percent_dist(fitpro, 13338, 1)
as.Date.numeric(13330)

auto_year(weir_data, 2006)
tails_equal_date(fitpro, dist_a =1, dist_b =2)

weir_2017 <- data_prep(weir_data, 2017)
graph_year(weir_2017)
fit2017 <- distribution_estimation(weir_2017)
dist_plot(fit2017)
(fitpro <- mix(as.mixdata(weir_2017), mixparam(mu=c(17338, 1717370), sigma=c(10,15)), dist="weibull")) 


tails_equal_date(fitpro, dist_a =1, dist_b =2)

pnormfit <- function(fit, x, dist_num =1){
  fit$parameters$pi[dist_num]*pnorm(x, fit$parameters$mu[dist_num],fit$parameters$sigma[dist_num])
}

tails_difference <- function(fit, x, dist_a =1, dist_b =2){
  abs(pnormfit(fit, x, 1) - (1 - pnormfit(fit, x, 2)))
}

tails_equal_date <- function(fit, dist_a =1, dist_b =2){
  # find X (date) when pnorm(dist 1) = 1 - pnorm(dist 2) taking into account the proporiton (pi)
  # each distribution is of the whole multinomial distribution
  
  #find x where
  pnormfit(fitpro, 13338, 2) #is about equal to  
  1 - pnormfit(fitpro, 13338, 1) 
  # AKA 
  pnormfit(fitpro, 13338-1, 2) - 1 + pnormfit(fitpro, 13338-1, 1) # is as small as possible. 
  
  # start with x (date) halfway between 2 means
  current_x <- floor((fit$parameters$mu[1]+fit$parameters$mu[1])/2)
  difference_current <- tails_difference(fit, current_x)
  difference_new <- tails_difference(fit, current_x+1)
  
  # if the next day the difference is greater 
  if(difference_current < difference_new ){
    # then consider the day before
    new_x = current_x - 1
    difference_new = tails_difference(fit, new_x)
    while (difference_new < difference_current){ #keep going back a day until the smallest difference is found
      difference_current <- difference_new
      current_x <- new_x
      new_x <- new_x - 1
      difference_new <- tails_difference(fit, new_x)
    } } else{ # difference_new <= difference_current # keep going forward a day until the smallest difference is found
      while (difference_new < difference_current){
        difference_current <- difference_new
        current_x <- new_x
        new_x = new_x + 1
        difference_new = tails_difference(fit, new_x)
      }
    }
  
  return(current_x) #(as.Date.numeric(current_x)) # date 
}



#2007
weir_data %>% filter(year(date)==2007) -> weir_2007
graph_year(weir_2007)
distribution_estimation(weir_2007)

#2008
weir_data %>% filter(year(date)==2008) -> weir_2008
graph_year(weir_2008)
distribution_estimation(weir_2008) #THis didn't work very well
distribution_estimation(weir_2008, 3, mean_guess_given = c(14050, 14080, 14110 )) # This worked better

auto_year(weir_data, 2006)
auto_year(weir_data, 2007)
auto_year(weir_data, 2008)
auto_year(weir_data, 2009)
auto_year(weir_data, 2010)
auto_year(weir_data, 2011)
auto_year(weir_data, 2012)
auto_year(weir_data, 2013)
auto_year(weir_data, 2014)
auto_year(weir_data, 2015)
auto_year(weir_data, 2016)
auto_year(weir_data, 2017)


