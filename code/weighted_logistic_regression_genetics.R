# notes ----
# determining run curves based from genetics
# sarah.power@alaska.gov
# 12/11/2018

# load ----
library(here)
library(cowplot) #used in mutiple plot displays
library(broom) #used for predictions
source('code/distribution_functions.R')
#library(lubridate)
#str_c from stringr
getwd()


# data ----
proportions_data <-read_csv('data/chig_genetics_by_weir_date.csv') %>% 
  mutate(date_weir = lubridate::ymd(str_c(year,"/", month,"/", day)),
         day_of_year = lubridate::yday(date_weir),
         inv_var = 1/((sd_bayes)^2))# %>% 
  #select(-month, -day) %>%
  #filter(year < 2009) 

anchors <- proportions_data %>% 
  group_by(year) %>%
  #summarize(mean_sd = mean(sd_bayes), m_inv_var = mean(inv_var))
  summarize(m_sd = max(sd_bayes), m_inv_var = min(inv_var)) # explore using max in stead of min
#Decided not to use this, and instead use 0 for the weighting factor for the anchors.
#I am leaving the code here in case someone wants to revisit that choice. 

anchor_df <- data.frame("year" = c(2006:2008, 2010:2018, 2006:2008, 2010:2018), 
                       "month" = c(rep(9, 12), rep(5, 12)),
                       "day" = c(rep(1, 12), rep(25, 12)),
                       "black_proportion" = c(rep(0, 12), rep(1, 12)),
                       "chig_proportion" = c(rep(1, 12), rep(0, 12)),
                       "sd_bayes" = anchors$m_sd, #note this automatically repeats 
                       "sample_size" = rep(0, 24),
                       "inv_var" = rep(0, 24) #recommend that the weight is 0. 
                       )

anchor_df <- anchor_df %>% 
  mutate(date_weir = lubridate::ymd(str_c(year,"/", month,"/", day)),
         day_of_year = lubridate::yday(date_weir))

anchor_df <-bind_rows(proportions_data, anchor_df)

logistic_by_year <- function(df, ayear){
  df <- df %>%
    filter(year == ayear)
  ggplot(df, aes(day_of_year, chig_proportion, weight = inv_var)) +
    xlab(str_c(ayear, " day of year")) +
    geom_point(size = 2) +
    geom_smooth( method = "glm", se = 0, size = 2, method.args = list(family = binomial)) 
}

#plots all years
  ggplot(anchor_df , aes(day_of_year, chig_proportion, weight = inv_var)) +
  #xlab(str_c(year, " day of year")) +
  geom_point(size = 2) +
  geom_smooth( method = "glm", se = 0, size = 2, method.args = list(family = binomial)) +
  facet_wrap(~year, ncol = 3,  dir='v')

#individual yearly plots
log2006 <- logistic_by_year(anchor_df, 2006)
log2007 <- logistic_by_year(anchor_df, 2007)
log2008 <- logistic_by_year(anchor_df, 2008)
log2010 <- logistic_by_year(anchor_df, 2010)
log2011 <- logistic_by_year(anchor_df, 2011)
log2012 <- logistic_by_year(anchor_df, 2012)
log2013 <- logistic_by_year(anchor_df, 2013)
log2014 <- logistic_by_year(anchor_df, 2014)
log2015 <- logistic_by_year(anchor_df, 2015)
log2016 <- logistic_by_year(anchor_df, 2016)
log2017 <- logistic_by_year(anchor_df, 2017)
log2018 <- logistic_by_year(anchor_df, 2018)

#combines individual plots together
x <- cowplot::plot_grid(log2006, log2007, log2008, log2010, log2011, log2012, 
                        ncol = 3, align = "hv")                        
y <- cowplot::plot_grid(log2013, log2014, log2015, log2016, log2017, log2018, 
                        ncol = 3, align = "hv")
x
y

# this works out the weighted logistic equation for each year in reguards to "Black Lake" or early run fish. 
black_logistic_eq <- function(df, ayear){
  df <- df %>%
    filter(year == ayear)
  glm(black_proportion ~ day_of_year, weights = inv_var, data = proportions_data, family = "binomial")
}

#computing for one year 
bl2006 <- black_logistic_eq(anchor_df, 2006)

new_data <- data_frame(day_of_year =173)

gives predicted response
augment(bl2006, newdata= new_data, type.predict = "response")
augment(bl2006) # creates a df of all fitted values.
