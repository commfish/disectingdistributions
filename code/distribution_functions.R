# notes ----
# functions for dissecting distributions
# sarah.power@alaska.gov
# 8/15/2018

# load ----
library(tidyverse)
library(mixdist)
library(lubridate)
#if(!require("cowplot"))   install.packages("cowplot")

#install.packages("devtools")
#devtools::install_github("ben-williams/FNGr")
#library("FNGr")

#library(gridExtra) #for multiple plots on a page
#library(here)
#source('code/functions.R')

#windowsFonts(Times=windowsFont("Times New Roman"))

theme_sleek <- function(base_size = 12, base_family = "Times") {
  half_line <- base_size/2
  theme_light(base_size = 12, base_family = "Times") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = unit(0.9, "lines"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)
    )
}

#theme_set(theme_sleek())

# functions ----
data_prep <- function(df, year_wanted){
  df %>% 
    filter(year(date)==year_wanted) %>%
  # Create a data frame whose first column are the dates in numeric format
  # and whose second column are the frequencies. 
  # This is required for fitting the mixture. See mixdata {mixdist}
    dplyr::select(day_of_year, run) -> df
}
data_prep_early <- function(df, year_wanted){
  df %>% 
    filter(year(date)==year_wanted) %>%
    # Create a data frame whose first column are the dates in numeric format
    # and whose second column are the frequencies. 
    # This is required for fitting the mixture. See mixdata {mixdist}
    dplyr::select(day_of_year, run_early_gen) -> df
}

year_stats <- function (df, year_wanted){
  df %>%
    filter(year(date)==year_wanted)-> df
  #run_size <-sum(df$run)
  #print("Run")
  #print(run_size)
  df_fit <- data_prep(df, year_wanted)
  fit <- distribution_estimation_norms_SEQ(df_fit) 
  #dist_plot (fit, year_wanted)
  df_fit_early <- data_prep_early(df, year_wanted)
  fit_early <- distribution_estimation_norms(df_fit_early)

  #print("Mean day-of-year & sd based on runtiming distributions alone")
  #print(c(yday(fit$parameters$mu[1]), fit$parameters$sigma[1]))
  #print("Mean day-of-year & sd based on additional genetics information")
  #print(c(yday(fit_early$parameters$mu[1]), fit_early$parameters$sigma[1]))
  df %>%
    mutate(dist_percent = percent_dist(fit, df$day_of_year),
           run_early_dis = dist_percent*run,
           cum_run_dis = cumsum(run_early_dis),
           cum_run_gen = cumsum(run_early_gen)) ->df
  #print("Number of early run by runtiming distribution")
  #print(max(df$cum_run_dis))
  #print("Number of early run by genetics runtiming distribution")
  #print(max(df$cum_run_gen))
  #print("runtiming distribution/genetics runtiming distribution")
  #print(max(df$cum_run_dis)/max(df$cum_run_gen))
  ggplot(df, aes(day_of_year))+
    geom_line(aes(y=dist_percent), colour = "green")+
    geom_line(aes(y=prop_early_genetics), colour = "blue")+
    ggtitle(paste0("Gen vs DistRuntiming Assignment", year_wanted))
  ggplot(df, aes(day_of_year))+
    geom_line(aes(y=cum_run_dis), colour = "green")+
    geom_line(aes(y=cum_run_gen), colour = "blue")+
    labs(y = "cumulative run", x= "day of the year")+
    ggtitle(paste0("Number of run in Genetic(blue) vs Distribution only (green) Early Run ", year_wanted))
}

#year_stats(weir_data, 2017)

graph_year <- function(df){
  #Graph daily weir run = escapement + catch for a year ----
  ggplot(df, aes(day_of_year, run_early)) + 
    geom_line() +
    labs(title = "Early Run Daily weir passage", x = "date in number format", y = "Number of fish")
}

graph_year_early <- function(df){
  #Graph daily weir run = escapement + catch for a year ----
  ggplot(df, aes(day_of_year, run_early)) + 
    geom_line() +
    labs(title = "Early Run Daily weir passage", x = "date in number format", y = "Number of fish")
}

early_look <- function(df, year_wanted){
  df <- data_prep_early(df, year_wanted)
  fit <- mix(as.mixdata(df), mixparam(mu=mean(df$day_of_year), sigma=sd(df$day_of_year)), dist="gamma", iterlim=5000)
  dist_plot(fit, year_wanted)
}


distribution_estimation_norms <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$day_of_year) -30, mean(df$day_of_year), mean(df$day_of_year)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(9, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess), dist=distibution_guess, iterlim=5000))  
  
}

distribution_estimation_norms_MES <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$day_of_year) -30, mean(df$day_of_year), mean(df$day_of_year)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(9, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess),constr = mixconstr(conmu="MES"), dist=distibution_guess, iterlim=5000))  
  
}

distribution_estimation_norms_SEQ <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$day_of_year) -30, mean(df$day_of_year), mean(df$day_of_year)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(9, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess),constr = mixconstr(consigma="SEQ"), dist=distibution_guess, iterlim=5000))  
  
}

distribution_estimation_norms_SEQ_n1 <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$day_of_year) -30, mean(df$day_of_year), mean(df$day_of_year)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(9, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess),constr = mixconstr(consigma="SEQ"), dist=distibution_guess, iterlim=5000)
  
  mean_guess <- c(fitpro$parameters$mu[1],fitpro$parameters$mu[2],fitpro$parameters$mu[3])
  fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess),constr = mixconstr(conmu= "MFX", fixmu = c(TRUE, FALSE, FALSE)), dist=distibution_guess, iterlim=5000)
  
}

distribution_estimation_norms_SFX <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  mean_guess = c(mean(df$day_of_year) -30, mean(df$day_of_year), mean(df$day_of_year)+30) #currently the default is for three distributions.
  sigma_guess = c(8.6, 8.6, 8.6)
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess),constr = mixconstr(consigma="SFX", fixsigma= c(TRUE, FALSE, FALSE)), dist=distibution_guess, iterlim=5000))  
}

distribution_estimation_norms_MFX <-  function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
  mean_guess = c(174, mean(df$day_of_year), mean(df$day_of_year)+30) #currently the default is for three distributions.
  sigma_guess = c(8.6, 8.6, 8.6)
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess),constr = mixconstr(conmu="MFX", fixmu= c(TRUE, FALSE, FALSE)), dist=distibution_guess, iterlim=5000))  
}

distribution_estimation_weibull <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'weibull'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$day_of_year) -30, mean(df$day_of_year), mean(df$day_of_year)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(9, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess), dist=distibution_guess))  #, iterlim=5000
  
}

distribution_estimation_weibull_SEQ <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'weibull'){
  
  if(missing(mean_guess_given)) {
    mean_guess = c(mean(df$day_of_year) -30, mean(df$day_of_year), mean(df$day_of_year)+30) #currently the default is for three distributions.
  } else {
    mean_guess = mean_guess_given
  }
  if(missing(sigma_guess_given)) {
    sigma_guess = rep(9, each = num_of_distributions)
  } else {
    sigma_guess = sigma_guess_given
  }
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess), constr = mixconstr(consigma="SEQ"), dist=distibution_guess))  #, iterlim=5000
  
}

dist_plot <- function (fitpro, year_wanted){
  #Plot the results  
  ggplot(fitpro, main=year_wanted) 
  grid()  
  legend("topright", lty=1, lwd=c(1, 1, 2), c("Original Distribution to be Fit", "Individual Fitted Distributions", "Fitted Distributions Combined"), col=c("blue", "red", rgb(0.2, 0.7, 0.2)), bg="white")  
  
  #Estimated mean date and sigmas.
  #summary(fitpro) 
}

auto_year<- function (df, year_wanted) {
  df_year <- data_prep(df, year_wanted) 
  #graph_year(df_year)
  #fitpro <- distribution_estimation_norms(df_year) 
  #dist_plot(fitpro, year_wanted )
  #fitpro <- distribution_estimation_norms_MFX(df_year) 
  #dist_plot(fitpro, year_wanted ) 
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

#This function still needs work. 
tails_difference <- function(fit, x, dist_a =1, dist_b =2){
  difference <- abs(pnormfit(fit, x, 1) + 1 - pnormfit(fit, x, 2))
}

#This function still needs work. 
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

