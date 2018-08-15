# notes ----
# dissecting distributions
# sarah.power@alaska.gov
# 8/13/2018

# load ----
library(tidyverse)
library(mixdist)
library(lubridate)
library(scales)
library(ggplot2)
library(here)
#source('code/functions.R')

# data ----
weir_data <- read_csv('data/ChigISGrunappt2006-2017.csv') %>% 
  dplyr::select(-X9) %>%
  dplyr::rename(date = Date, 
                prop_early_genetics = Propotionearly, 
                early_esc_genetics = Earlyesc, 
                late_esc_genetics = Lateesc,
                early_catch_genetics = Earlycatch,
                late_catch_genetics = Latecatch,
                early_total_genetics = Earlytotal,
                late_total_genetics = Latetotal)%>%
  dplyr::mutate(esc = early_esc_genetics + late_esc_genetics,
                catch = early_catch_genetics + late_catch_genetics)
weir_data$date <- as.Date(weir_data$date ,"%m/%d/%y")

glimpse(weir_data)

weir_data <- weir_data %>% filter(year(weir_data$date)==2006)
unique(year(weir_data$date))


#Graph data ----

# convert the Date to its numeric equivalent
# Note that Dates are stored as number of days internally,
# One can convert back and forth since dates don't lend well to numeric algorithms. 
weir_data$date_num <- as.numeric(weir_data$date)

# Create a data frame whose first column are the dates in numeric format
# and whose second column are the frequencies. 
# This is required for fitting the mixture. See mixdata {mixdist}
df <- data.frame(mid=weir_data$date_num, cou=weir_data$esc)  
head(df)

# Graph a plot of daily weir passage
ggplot(data = weir_data, aes(x = date_num, y = esc)) + 
  geom_line() +
  labs(title = "Daily weir passage", x = "date in number format", y = "Number of fish")

# Guess at mean "number date" and sigmas ----
#Using the above graph guess at Means (numeric date) in Ascending Order, 
#and the associated Sigmas and distribution.  
guemea <- c(13320, 13350, 13370)#c(17340, 17375, 17410)  
guesig <- c(10, 10, 10)  
guedis <- "norm"  
(fitpro <- mix(as.mixdata(df), mixparam(mu=guemea, sigma=guesig), dist=guedis))  

#Plot the results  
plot(fitpro, main="Fit a Probability Distribution")  
grid()  
legend("topright", lty=1, lwd=c(1, 1, 2), c("Original Distribution to be Fit", "Individual Fitted Distributions", "Fitted Distributions Combined"), col=c("blue", "red", rgb(0.2, 0.7, 0.2)), bg="white")  

#Estimated mean date and sigmas.
summary(fitpro)
