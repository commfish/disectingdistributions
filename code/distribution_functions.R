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

graph_year <- function(df){
  #Graph daily weir run = escapement + catch for a year ----
  ggplot(df, aes(date_num, run)) + 
    geom_line() +
    labs(title = "Daily weir passage", x = "date in number format", y = "Number of fish")
}

distribution_estimation <- function(df, num_of_distributions = 3, mean_guess_given , sigma_guess_given, distibution_guess = 'gamma'){
  
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
  
  (fitpro <- mix(as.mixdata(df), mixparam(mu=mean_guess, sigma=sigma_guess), dist=distibution_guess))  
  
}

dist_plot <- function (fitpro){
  #Plot the results  
  plot(fitpro, main="Fit Distributions for a year" )  
  grid()  
  legend("topright", lty=1, lwd=c(1, 1, 2), c("Original Distribution to be Fit", "Individual Fitted Distributions", "Fitted Distributions Combined"), col=c("blue", "red", rgb(0.2, 0.7, 0.2)), bg="white")  
  
  #Estimated mean date and sigmas.
  summary(fitpro) 
}
  

auto_year <- function (df, year_wanted) {
  df_year <- data_prep(df, year_wanted ) 
  graph_year(df_year)
  fitpro <- distribution_estimation(df_year) 
  dist_plot(fitpro)
}


