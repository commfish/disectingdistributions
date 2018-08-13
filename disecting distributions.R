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

qplot(weir_data$esc, geom = "histogram")

weir_data <- weir_data %>% filter(year(weir_data$date)==2017)
unique(year(weir_data$date))
#weir_data$date  <- as.POSIXct(weir_data$date )

#Compute a histogram ----

# convert the Date to its numeric equivalent
# Note that Dates are stored as number of days internally,
# hence it is easy to convert back and forth mentally
weir_data$date_num <- as.numeric(weir_data$date)
min(weir_data$date_num, na.rm = TRUE)

bin <- 60 # used for aggregating the data and aligning the labels

p <- ggplot(weir_data, aes(x = date_num, y = esc))+ 
  geom_histogram(binwidth = bin, colour="white")


ggplot(data = weir_data, aes(x = date_num, y = esc)) + 
 geom_line() 
#  labs(title = "Title", x = "x lable", y = "Number of fish")

# The numeric data is treated as a date,
# breaks are set to an interval equal to the binwidth,
# and a set of labels is generated and adjusted in order to align with bars
p <- p + scale_x_date(breaks = seq(min(weir_data$date_num)-20, # change -20 term to taste
                                   max(weir_data$date_num), 
                                   bin),
                      labels = date_format("%Y-%b"),
                      limits = c(as.Date("2017-01-01"), 
                                 as.Date("2017-12-01")))





#The above graph shows 3 peaks that might be represented by 3 Normal  
#Distributions.  Guess at the 3 Means in Ascending Order, with a guess for  
#the associated 3 Sigmas and fit the distribution.  
guemea <- c(17340, 17375, 17410)  
guesig <- c(4, 3, 2)  
guedis <- "norm"  
(fitpro <- mix(as.mixdata(df), mixparam(mu=guemea, sigma=guesig), dist=guedis))  

#Plot the results  
plot(fitpro, main="Fit a Probability Distribution")  
grid()  
legend("topright", lty=1, lwd=c(1, 1, 2), c("Original Distribution to be Fit", "Individual Fitted Distributions", "Fitted Distributions Combined"), col=c("blue", "red", rgb(0.2, 0.7, 0.2)), bg="white")  




p <- ggplot(data = weir_data, aes(x = date, y = esc)) + 
  geom_histogram() +
  theme_bw() + xlab(NULL) +
  scale_x_datetime(breaks = ?date_breaks("1 month"),
                   labels = date_format("%m-%d"),
                   limits = c(as.POSIXct("2012-01-01"), 
                              as.POSIXct("2013-01-01")) )

p
