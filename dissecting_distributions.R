# notes ----
# dissecting distributions
# sarah.power@alaska.gov
# 8/13/2018

# load ----
library(tidyverse)
library(mixdist)
library(lubridate)
library(here)
#source('code/functions.R')

# data ----
read_csv('../data/ChigISGrunappt2006-2017.csv') %>% 
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
         date = mdy(date),
         date_num = as.numeric(date)) -> weir_data # convert the Date to its numeric equivalent
# Filter out just 2006 data.
weir_data %>% 
  filter(year(date)==2006) -> weir_2006


#Graph daily weir escapement for a year ----
ggplot(weir_2006, aes(date_num, esc)) + 
  geom_line() +
  labs(title = "Daily weir passage", x = "date in number format", y = "Number of fish")


# Create a data frame whose first column are the dates in numeric format
# and whose second column are the frequencies. 
# This is required for fitting the mixture. See mixdata {mixdist}
weir_2006 %>% 
  dplyr::select(date_num, esc) -> df


# Guess at mean "number date" and sigmas ----
#Using the above graph guess at Means (numeric date) in Ascending Order, 
#and the associated Sigmas and distribution.  

#Perhaps just use mean numeric date +- 30 in ascending order.IF that doesn't work hard code a guess.
guemea <- c(mean_date - 30, mean_date, mean_date + 30)

#guemea <- # for 2006: c(13320, 13350, 13370)# for 2017: c(17340, 17375, 17410)  
guesig <- c(10, 10, 10)  
guedis <- "norm"  
(fitpro <- mix(as.mixdata(df), mixparam(mu=guemea, sigma=guesig), dist=guedis))  

#Plot the results  
plot(fitpro, main="Fit a Probability Distribution")  
grid()  
legend("topright", lty=1, lwd=c(1, 1, 2), c("Original Distribution to be Fit", "Individual Fitted Distributions", "Fitted Distributions Combined"), col=c("blue", "red", rgb(0.2, 0.7, 0.2)), bg="white")  

#Estimated mean date and sigmas.
summary(fitpro)
