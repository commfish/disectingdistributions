# notes ----
# functions for dissecting distributions
# sarah.power@alaska.gov
# 8/15/2018

# load ----
library(tidyverse)
library(mixdist)
library(lubridate)
#library(here)
#source('code/functions.R')

# functions ----
data_prep <- function(df, year_wanted){
  df %>% 
    filter(year(date)==year_wanted) %>%
  # Create a data frame whose first column are the dates in numeric format
  # and whose second column are the frequencies. 
  # This is required for fitting the mixture. See mixdata {mixdist}
    dplyr::select(date_num, run) -> df
}
data_prep_early <- function(df, year_wanted){
  df %>% 
    filter(year(date)==year_wanted) %>%
    # Create a data frame whose first column are the dates in numeric format
    # and whose second column are the frequencies. 
    # This is required for fitting the mixture. See mixdata {mixdist}
    dplyr::select(date_num, run_early) -> df
}

graph_year <- function(df){
  #Graph daily weir run = escapement + catch for a year ----
  ggplot(df, aes(date_num, run_early)) + 
    geom_line() +
    labs(title = "Early Run Daily weir passage", x = "date in number format", y = "Number of fish")
}

graph_year_early <- function(df){
  #Graph daily weir run = escapement + catch for a year ----
  ggplot(df, aes(date_num, run_early)) + 
    geom_line() +
    labs(title = "Early Run Daily weir passage", x = "date in number format", y = "Number of fish")
}

early_look <- function(df, year_wanted){
  df <- data_prep_early(df, year_wanted)
  fit <- mix(as.mixdata(df), mixparam(mu=mean(df$date_num), sigma=sd(df$date_num)), dist="gamma", iterlim=5000)
  dist_plot(fit, year_wanted)
}


distribution_estimation_norms <- distribution_estimation <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$date_num) -30, mean(df$date_num), mean(df$date_num)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(10, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess), dist=distibution_guess, iterlim=5000))  
  
}

distribution_estimation_norms_MES <- distribution_estimation <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$date_num) -30, mean(df$date_num), mean(df$date_num)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(10, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess),constr = mixconstr(conmu="MES"), dist=distibution_guess, iterlim=5000))  
  
}

distribution_estimation_norms_SEQ <- distribution_estimation <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$date_num) -30, mean(df$date_num), mean(df$date_num)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(10, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess),constr = mixconstr(consigma="SEQ"), dist=distibution_guess, iterlim=5000))  
  
}


distribution_estimation_weibull <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'weibull'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$date_num) -30, mean(df$date_num), mean(df$date_num)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(10, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess), dist=distibution_guess))  #, iterlim=5000
  
}


dist_plot <- function (fitpro, year_wanted){
  #Plot the results  
  plot(fitpro, main=year_wanted ) 
  grid()  
  legend("topright", lty=1, lwd=c(1, 1, 2), c("Original Distribution to be Fit", "Individual Fitted Distributions", "Fitted Distributions Combined"), col=c("blue", "red", rgb(0.2, 0.7, 0.2)), bg="white")  
  
  #Estimated mean date and sigmas.
  summary(fitpro) 
}
  

auto_year<- function (df, year_wanted) {
  df_year <- data_prep(df, year_wanted) 
  graph_year(df_year)
  fitpro <- distribution_estimation_norms(df_year) 
  dist_plot(fitpro, year_wanted )
  fitpro <- distribution_estimation_norms_MES(df_year) 
  dist_plot(fitpro, year_wanted ) 
  fitpro <- distribution_estimation_norms_SEQ(df_year) 
  dist_plot(fitpro, year_wanted ) 
}




dnormfit <- function(fit, x, dist_num = 1){
  #fit$parameters$pi[dist_num]*dnorm(x, fit$parameters$mu[dist_num],fit$parameters$sigma[dist_num])
  dnorm(x, fit$parameters$mu[dist_num],fit$parameters$sigma[dist_num])
}

percent_dist <- function(fit, x, dist_num = 1){
  dnormfit(fit, x, dist_num)/(dnormfit(fit, x, 1)+dnormfit(fit, x, 2)+dnormfit(fit, x, 3)) 
}

pnormfit <- function(fit, x, dist_num =1){
  fit$parameters$pi[dist_num]*pnorm(x, fit$parameters$mu[dist_num],fit$parameters$sigma[dist_num])
}

tails_difference <- function(fit, x, dist_a =1, dist_b =2){
  difference <- abs(pnormfit(fit, x, 1) + 1 - pnormfit(fit, x, 2))
}

tails_equal_date <- function(fit, dist_a =1, dist_b =2){
  # find X (date) when pnorm(dist 1) = 1 - pnorm(dist 2) taking into account the proporiton (pi)
  # each distribution is of the whole multinomial distribution

  #find x where
  pnormfit(fitpro, 13338, 2) #is about equal to  
  1 - pnormfit(fitpro, 13338+1, 1) 
  # AKA 
  pnormfit(fitpro, 13338+1, 2) - 1 + pnormfit(fitpro, 13338+1, 1) # is as small as possible. 
  
  # start with x (date) halfway between 2 means
  current_x <- floor((fit$parameters$mu[dist_a]+fit$parameters$mu[dist_b])/2)
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

