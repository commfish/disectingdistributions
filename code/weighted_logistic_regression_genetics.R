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
         inv_var = 1/((sd_bayes)^2),
         bl_upr_95ci  = pmin(black_proportion + 1.95*sd_bayes, 1),
         bl_lwr_95ci  = pmax(black_proportion - 1.95*sd_bayes, 0),
         #bl_upr_se = sqrt(bl_upr_est*(1-bl_upr_est)), The BAYES genetics results do not contain the out of sample variance.
         #bl_lwr_se = sqrt(bl_lwr_est*(1-bl_lwr_est)), So you may want these intervals
         #bl_upr_95ci = pmin(bl_upr_est + 1.95*bl_upr_se, 1),
         #bl_lwr_95ci = pmax(bl_upr_est - 1.95*bl_upr_se, 0)
  )

#unique(proportions_data$date_weir)

anchors <- proportions_data %>% 
  group_by(year) %>%
  summarize(m_sd = max(sd_bayes), m_inv_var = min(inv_var), min_black_prop = min(black_proportion), max_black_prop = max(black_proportion)) # explore using max in stead of min
#using the max sd for a year, instead of 0 for the weighting factor for the anchors.
#anchors are guestimates and so a wide estimate of sd is preferable.

anchor_df <- data.frame("year" = c(2006:2008, 2010:2018, 2006:2008, 2010:2018), 
                        "month" = c(rep(5, 12), rep(8, 12)), #May and either Aug or September
                        "day" = c(rep(25, 12), rep(9, 12)),
                        "black_proportion" = c(rep(1, 12), rep(0, 12)),
                        "chig_proportion" = c(rep(0, 12), rep(1, 12)),
                        "sd_bayes" = anchors$m_sd, #note this automatically repeats 
                        "sample_size" = rep(0, 24),
                        "inv_var" = 1/(anchors$m_sd)^2, #rep(0, 24), 
                        #recommend that the weight = inv_var is 0 - But need the weight to be non 0 for the CI otherwise the upr CI is at 100% throughout season for most recent years.  
                        "bl_upr_95ci" = c(rep(1, 12), rep(0, 12)), # no CI really on our anchor points
                        "bl_lwr_95ci" = c(rep(1, 12), rep(0, 12))
)

anchor_df <- anchor_df %>% 
  mutate(date_weir = lubridate::ymd(str_c(year,"/", month,"/", day)),
         day_of_year = lubridate::yday(date_weir))

#This adds anchor data to rest of genetic / run reconstruction data 
anchor_df <-bind_rows(proportions_data, anchor_df)


#### checking code can be removed later#####
x <- anchor_df %>% 
  filter(year == 2015) %>% 
  select(black_proportion, sd_bayes, day_of_year)
x <- data.frame("p" = c(0.905, 0.932, 0.864, 0.894, 0.363, 0.383), "sd" = c(0.054, 0.042, 0.051, 0.061, 0.068, 0.061),  "day" = c(178, 182, 186, 193, 199, 206))
#################

#this function is just the line without CI's 
logistic_by_year <- function(df, ayear){
  df <- df %>%
    filter(year == ayear)     #Try weight = sample_size
    ggplot(df, aes(day_of_year, chig_proportion, weight = inv_var)) + # the SE;s and Cis are very tight when we use sample size or inv_var for the weight. 
    xlab(str_c(ayear, " day of year")) +
    geom_point(size = 2) +
    geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) #+
    #stat_smooth(method = "glm", level = .95 , size = 2, method.args = list(family = quasibinomial))
}
logistic_by_year(anchor_df, 2015)


#this is with CI's and will overwrite the function above.
logistic_by_year <- function(df, ayear){
  #df <- anchor_df
  #ayear <- 2006
  df <- df %>%
    filter(year == ayear) %>%
    mutate(chig_lwr_95ci = 1- bl_upr_95ci, chig_upr_95ci = 1- bl_lwr_95ci) %>%
    select(year, day_of_year, inv_var, chig_proportion, chig_upr_95ci, chig_lwr_95ci) %>%
    #select(year, day_of_year, inv_var, black_proportion, bl_upr_95ci, bl_lwr_95ci) %>%
    gather(key, value, -year, -day_of_year, -inv_var)

  ggplot(df, aes(x = day_of_year, y = value, group = key, weight = inv_var, colour = key)) +
    xlab(str_c(ayear, " day of year")) +
    geom_point(size = 2) +
    geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) 
}
logistic_by_year(anchor_df, 2015)

logistic_by_year_ci <- function(df, ayear){
  #df <- anchor_df
  #ayear <- 2006
  df <- df %>%
    filter(year == ayear) %>%
    select(year, day_of_year, inv_var, black_proportion, bl_upr_95ci, bl_lwr_95ci) 
  
  ggplot(data=df, mapping=aes(x = day_of_year, y = black_proportion, weight = inv_var)) + geom_point() +         
    stat_smooth(method="glm", level = .05, method.args=list(family= quasibinomial))
  
  #ggplot(df, aes(x = day_of_year, y = black_proportion, weight = inv_var)) +
   # xlab(str_c(ayear, " day of year")) +
    #geom_point(size = 2) +
    #geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) +
    #stat_smooth( method = "glm", level =.95, size = 2, method.args = list(family = quasibinomial))
}

#plots proportion of "later" run (chignik) sockeye stocks. 
  ggplot(anchor_df , aes(day_of_year, chig_proportion, weight = inv_var)) +
  #xlab(str_c(year, " day of year")) +
  geom_point(size = 2) +
  #geom_smooth( method = "glm", se = TRUE, level = .95, size = 2, method.args = list(family = quasibinomial)) +
  stat_smooth(method = "glm", se = TRUE, level = .95,  method.args = list(family = quasibinomial))+
  facet_wrap(~year, ncol = 3,  dir='v')
  
#plots proportion of "early" run (black lake) sockeye stocks. 
  ggplot(anchor_df , aes(day_of_year, black_proportion, weight = inv_var)) +
    #xlab(str_c(year, " day of year")) +
    geom_point(size = 2) +
    geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) +
    facet_wrap(~year, ncol = 3,  dir='v')

#individual yearly plots
(log2006 <- logistic_by_year(anchor_df, 2006))
(log2006 <- logistic_by_year_ci(anchor_df, 2006))
confint(model)


    
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
(x <- cowplot::plot_grid(log2006, log2007, log2008, log2010, log2011, log2012, 
                        ncol = 3, align = "hv"))                        
(y <- cowplot::plot_grid(log2013, log2014, log2015, log2016, log2017, log2018, 
                        ncol = 3, align = "hv"))

# this works out the weighted logistic equation for each year in reguards to "Black Lake" or early run fish. 
black_logistic_eq <- function(df, ayear){
  df <- df %>%
    filter(year == ayear)
  glm(black_proportion ~ day_of_year, weights = inv_var, data = proportions_data, family = "quasibinomial")
}



#computing for one year 
bl2006 <- black_logistic_eq(anchor_df, 2006)

bl2007 <- black_logistic_eq(anchor_df, 2007)
bl2008 <- black_logistic_eq(anchor_df, 2008)
bl2010 <- black_logistic_eq(anchor_df, 2010)
bl2011 <- black_logistic_eq(anchor_df, 2011)
bl2012 <- black_logistic_eq(anchor_df, 2012)
bl2013 <- black_logistic_eq(anchor_df, 2013)
bl2014 <- black_logistic_eq(anchor_df, 2014)
bl2015 <- black_logistic_eq(anchor_df, 2015)
bl2016 <- black_logistic_eq(anchor_df, 2016)
bl2017 <- black_logistic_eq(anchor_df, 2017)

for(i in year_vector ){
  as.name(paste0("bl",i)) <- black_logistic_eq(anchor_df, i)
}



new_data <- data_frame(day_of_year =173)

#gives predicted response
augment(bl2006, newdata= new_data, type.predict = "response")
augment(bl2006) # creates a df of all fitted values.

# to graph Confidence Intervals from: https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression
#mod <- bl2006
foo <- anchor_df %>% filter(year == 2006)
mod <- glm(black_proportion ~ day_of_year, data = foo, family = quasibinomial)
preddata <- with(foo, data.frame(day_of_year = seq(min(day_of_year), max(day_of_year), length = 30)))
preds <- predict(mod, newdata = preddata, type = "link", se.fit = TRUE) #list with fit and se.fit

str(preds)

critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

fit2 <- mod$family$linkinv(fit)
upr2 <- mod$family$linkinv(upr)
lwr2 <- mod$family$linkinv(lwr)

preddata$lwr <- lwr2 
preddata$upr <- upr2 
ggplot(data=foo, mapping=aes(x = day_of_year, y = black_proportion)) + geom_point() +         
  stat_smooth(method="glm", level = .95, method.args=list(family= quasibinomial)) + 
  geom_line(data=preddata, mapping=aes(x = day_of_year, y=upr), col="red") + 
  geom_line(data=preddata, mapping=aes(x = day_of_year, y=lwr), col="red") 
#this doesn't make sense the red lines should surround the blue one.

