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
         chig_lwr_95ci = 1- bl_upr_95ci, 
         chig_upr_95ci = 1- bl_lwr_95ci
         #bl_upr_se = sqrt(bl_upr_est*(1-bl_upr_est)), The BAYES genetics results do not contain the out of sample variance.
         #bl_lwr_se = sqrt(bl_lwr_est*(1-bl_lwr_est)), So you may want these intervals
         #bl_upr_95ci = pmin(bl_upr_est + 1.95*bl_upr_se, 1),
         #bl_lwr_95ci = pmax(bl_upr_est - 1.95*bl_upr_se, 0)
  )

#unique(proportions_data$date_weir)

anchors <- proportions_data %>% 
  group_by(year) %>%
  summarize(m_sd = min(sd_bayes), m_inv_var = min(inv_var), min_black_prop = min(black_proportion), max_black_prop = max(black_proportion)) # explore using max in stead of min
#using the max sd for a year, instead of 0 for the weighting factor for the anchors.
#anchors are guestimates and so a wide estimate of sd is preferable as it give the point less weight.

anchor_df <- data.frame("year" = c(2006:2008, 2010:2018, 2006:2008, 2010:2018), 
                        "month" = c(rep(5, 12), rep(8, 12)), #May and either Aug or September
                        "day" = c(rep(25, 12), rep(9, 12)),
                        "black_proportion" = c(rep(1, 12), rep(0, 12)),
                        "chig_proportion" = c(rep(0, 12), rep(1, 12)),
                        "sd_bayes" = anchors$m_sd, #note this automatically repeats 
                        "sample_size" = rep(1, 24),
                        "inv_var" = 1/(anchors$m_sd)^2, #rep(0, 24), 
                        #recommend that the weight = inv_var is 0 - But need the weight to be non 0 for the CI otherwise the upr CI is at 100% throughout season for most recent years.  
                        "bl_upr_95ci" = c(rep(1, 12), rep(0, 12)), # no CI really on our anchor points
                        "bl_lwr_95ci" = c(rep(1, 12), rep(0, 12))
)

anchor_df <- anchor_df %>% 
  mutate(date_weir = lubridate::ymd(str_c(year,"/", month,"/", day)),
         day_of_year = lubridate::yday(date_weir),
         chig_lwr_95ci = 1- bl_upr_95ci, 
         chig_upr_95ci = 1- bl_lwr_95ci)

#This adds anchor data to rest of genetic / run reconstruction data 
anchor_df <-bind_rows(proportions_data, anchor_df)


#### checking code can be removed later#####
x <- anchor_df %>% 
  filter(year == 2015) %>% 
  select(black_proportion, sd_bayes, day_of_year)
x <- data.frame("p" = c(0.905, 0.932, 0.864, 0.894, 0.363, 0.383), "sd" = c(0.054, 0.042, 0.051, 0.061, 0.068, 0.061),  "day" = c(178, 182, 186, 193, 199, 206))
#################

#This function is graphs the CI as it relates to the within sample variance produced from genetic estimates
logistic_by_year <- function(df, ayear){
  #df <- df %>%
    #filter(year == ayear)     #Try weight = sample_size
    ggplot(df, aes(day_of_year, chig_proportion)) + # the SE;s and Cis are very tight when we use sample size or inv_var for the weight. 
    xlab(str_c(ayear, " day of year")) +
    geom_point(size = 2) +
    geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) +
    #stat_smooth(method = "glm", level = .95 , size = 2, method.args = list(family = quasibinomial))
    facet_wrap(~year, ncol = 4)
}
logistic_by_year(anchor_df, 2008)


#This function graphs CI's created with the upper, mean, and lower CI of the genetic estimates,  and will overwrite the function above.
logistic_by_year <- function(df, ayear){
  df <- anchor_df
  #ayear <- 2006
  df <- df %>%
    #filter(year == ayear) %>%
    mutate(chig_lwr_95ci = 1- bl_upr_95ci, chig_upr_95ci = 1- bl_lwr_95ci) %>%
    select(year, day_of_year, inv_var, chig_proportion, chig_upr_95ci, chig_lwr_95ci) %>%
    #select(year, day_of_year, inv_var, black_proportion, bl_upr_95ci, bl_lwr_95ci) %>%
    gather(key, value, -year, -day_of_year, -inv_var)

  ggplot(df, aes(x = day_of_year, y = value, group = key, weight = inv_var, fill = key, colour = key)) +
    xlab(str_c(ayear, " day of year")) +
    geom_point(size = 2) +
    geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) +
    facet_wrap(~year, ncol = 4)
}
logistic_by_year(anchor_df, 2012)

logistic_by_year_sample_size <- function(df, ayear){
  df <- anchor_df
  #ayear <- 2006
  df <- df %>%
    #filter(year == ayear) %>%
    mutate(chig_lwr_95ci = 1- bl_upr_95ci, chig_upr_95ci = 1- bl_lwr_95ci) %>%
    select(year, day_of_year, sample_size, chig_proportion, chig_upr_95ci, chig_lwr_95ci) %>%
    #select(year, day_of_year, sample_size, black_proportion, bl_upr_95ci, bl_lwr_95ci) %>%
    gather(key, value, -year, -day_of_year, -sample_size)
  
  ggplot(df, aes(x = day_of_year, y = value, group = key, weight = sample_size, fill = key, colour = key)) +
    xlab(str_c(ayear, " day of year")) +
    geom_point(size = 2) +
    geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) +
    facet_wrap(~year, ncol = 4)
}
logistic_by_year_sample_size(anchor_df, 2008)

min_day_of_year <-min(anchor_df$day_of_year)
max_day_of_year <-max(anchor_df$day_of_year)


##############
#This function creates the ppper and lower CI's using 
#the upper and lower CIS of the logistic regressions made with 
#the upper and lower CIS from the genetic samples.

#weight = sample size
#family = quasibinomial

logistic_by_year_ci <- function(df= anchor_df, ayear){
  df <- df %>%
    #df <- anchor_df %>%
    #filter(year == 2018)
    filter(year == ayear)
  #new_df <- data.frame(day_of_year = seq(min(df$day_of_year), max(df$day_of_year)))
  mod <- glm(chig_proportion ~ day_of_year, data = df, weight = sample_size, family = quasibinomial)
  mod_upr <- glm(chig_upr_95ci ~ day_of_year, data = df, weight = sample_size, family = quasibinomial)
  mod_lwr <- glm(chig_lwr_95ci ~ day_of_year, data = df, weight = sample_size, family = quasibinomial)
  preddata<- with(df, data.frame(day_of_year = seq(min(df$day_of_year), max(df$day_of_year))))
  preds <- predict(mod, newdata = preddata, type = "link", se.fit = TRUE)
  preds_upr <- predict(mod_upr, newdata = preddata, type = "link", se.fit = TRUE)
  preds_lwr <- predict(mod_lwr, newdata = preddata, type = "link", se.fit = TRUE)
  
  critval <-  qnorm(0.975) #1.96 ## approx 95% CI
  upr <- preds_upr$fit + (critval * preds_upr$se.fit)
  lwr <- preds_lwr$fit - (critval * preds_lwr$se.fit)
  fit <- preds$fit
  
  fit2 <- mod$family$linkinv(fit)
  upr2 <- mod_upr$family$linkinv(upr)
  lwr2 <- mod_lwr$family$linkinv(lwr)
  
  preddata$lwr <- lwr2 
  preddata$upr <- upr2 
  ggplot(data = df, mapping = aes(x = day_of_year,y = chig_proportion)) + geom_point() +         
    stat_smooth(method="glm", method.args=list(family = quasibinomial)) + 
    geom_line(data = preddata, mapping=aes(x = day_of_year, y = upr), col="red") + 
    geom_line(data = preddata, mapping=aes(x = day_of_year, y = lwr), col="red") +
    geom_ribbon(data = preddata, mapping=aes(x = day_of_year, ymin = lwr, ymax = upr))
    theme_bw() 
  #return(preddata)  # returns the predicted data but not the 
}

logistic_by_year_ci(anchor_df,2013)

logistic_by_year(anchor_df, 2012)


df <- anchor_df %>%
  select(year, day_of_year, chig_proportion) %>%
  #select(year, day_of_year, inv_var, black_proportion, bl_upr_95ci, bl_lwr_95ci) %>%
  gather(key, value, -year -day_of_year, -inv_var)

ggplot(df, aes(x = day_of_year, y = chig_proportion)) +
  xlab(str_c(ayear, " day of year")) +
  geom_point(size = 2) +
  geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) +
  facet_wrap( ~ year, ncol = 4) +
  theme_bw()



# this works out the weighted logistic equation for each year in reguards to "Black Lake" or early run fish. 
black_logistic_eq <- function(df, ayear){
  df <- df %>%
    filter(year == ayear)
  glm(black_proportion ~ day_of_year, weights = inv_var, data = proportions_data, family = "quasibinomial")
}
chig_logistic_eq <- function(df, ayear){
  df <- df %>%
    filter(year == ayear)
  glm(chig_proportion ~ day_of_year, weights = inv_var, data = df, family = "quasibinomial")
}
df<- ayear <- NULL

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

ch2006 <- chig_logistic_eq(anchor_df, 2006)
ch2007 <- chig_logistic_eq(anchor_df, 2007)
ch2008 <- chig_logistic_eq(anchor_df, 2008)
ch2010 <- chig_logistic_eq(anchor_df, 2010)
ch2011 <- chig_logistic_eq(anchor_df, 2011)
ch2012 <- chig_logistic_eq(anchor_df, 2012)
ch2013 <- chig_logistic_eq(anchor_df, 2013)
ch2014 <- chig_logistic_eq(anchor_df, 2014)
ch2015 <- chig_logistic_eq(anchor_df, 2015)
ch2016 <- chig_logistic_eq(anchor_df, 2016)
ch2017 <- chig_logistic_eq(anchor_df, 2017)
ch2018 <- chig_logistic_eq(anchor_df, 2018)

new_data <- data_frame(day_of_year =c(145:222))

#gives predicted response
fit_df <- augment(bl2006, newdata= new_data) #, type.predict = "response")
sy <- sqrt(summary(bl2006)$dispersion)
#augment(bl2006) # creates a df of all fitted values.
fit_df <- fit_df %>%
  mutate(pred.se = sy*sqrt((.se.fit/sy)^2+1))


predict(bl2006, newdata= new_data, interval = "response")
summary(bl2006)

##
predict.glm(bl2006, se.fit = TRUE,  newdata= new_data, type = "response")




##############The code below if for the average logistic model
#This call on the yearly logistic eq.s from weighted_logistic_regression_genetics.R
gen_calc <- function(df, ayear){
  df <- df %>%
    filter(year == ayear) %>%
    select(day_of_year, run, date, year)
  new_data <- df %>%
    select(day_of_year)
  gen <- augment(eval(as.name(paste0("bl",ayear))), newdata= new_data, type.predict = "response") 
  df <- left_join(df, gen, by = "day_of_year")  %>% 
    rename(prop_early_genetics = .fitted, prop_early_genetics_se = .se.fit) %>% 
    mutate(run_early_gen = prop_early_genetics*run) 
}

length(c(145:222))
max(anchor_df$day_of_year)
#This call on the yearly logistic eq.s from weighted_logistic_regression_genetics.R
ch_gen_calc <- function(df, ayear){
  #df <- anchor_df
  #ayear <- 2006
  df <- df %>%
    filter(year == ayear) %>%
    select(day_of_year, year)
  #day_of_year <- c(176, 182, 188, 194, 199, 204) # days chosen were those day_of_year form 2017
  day_of_year <- c(145:222)
  year <- rep(ayear, 78)
  new_data <- data.frame(day_of_year, year)
  gen <- augment(eval(as.name(paste0("ch",ayear))), newdata= new_data, type.predict = "response") 
  new_data <- left_join(new_data, gen, by = c("day_of_year", "year"))  %>% 
    rename(chig_proportion = .fitted, chig_proportion_se = .se.fit) %>%
    mutate(inv_var = 1/(chig_proportion_se)^2, 
           up95ci = pmin(chig_proportion + 1.96*chig_proportion_se, 1),
           lw95ci = pmax(chig_proportion - 1.96*chig_proportion_se, 0))
}
ch_samp_2006 <- ch_gen_calc(anchor_df, 2006)
ch_samp_2007 <- ch_gen_calc(anchor_df, 2007)
ch_samp_2008 <- ch_gen_calc(anchor_df, 2008)
ch_samp_2010 <- ch_gen_calc(anchor_df, 2010)
ch_samp_2011 <- ch_gen_calc(anchor_df, 2011)
ch_samp_2012 <- ch_gen_calc(anchor_df, 2012)
ch_samp_2013 <- ch_gen_calc(anchor_df, 2013)
ch_samp_2014 <- ch_gen_calc(anchor_df, 2014)
ch_samp_2015 <- ch_gen_calc(anchor_df, 2015)
ch_samp_2016 <- ch_gen_calc(anchor_df, 2016)
ch_samp_2017 <- ch_gen_calc(anchor_df, 2017)
ch_samp_2018 <- ch_gen_calc(anchor_df, 2018)

#This adds anchor data to rest of genetic / run reconstruction data 
avg_data <-bind_rows(ch_samp_2006, ch_samp_2007,ch_samp_2008, ch_samp_2010, ch_samp_2011, ch_samp_2012,
                     ch_samp_2013, ch_samp_2014, ch_samp_2015, ch_samp_2016, ch_samp_2017, ch_samp_2018)

avg_df <- avg_data %>%
  mutate(up95ci = pmin(chig_proportion + 1.96*chig_proportion_se, 1),
         lw95ci = pmax(chig_proportion - 1.96*chig_proportion_se, 0),
         day_of_year = factor(day_of_year)) %>%
  group_by(day_of_year) %>%
  summarize(chig_proportion = mean(chig_proportion), #chig_proportion_se = sd(chig_proportion), 
            up95ci = mean(up95ci), lw95ci = mean(lw95ci)) %>% # the mean of the up95ci & lw95Ci is used.
  mutate(day_of_year = as.integer(as.character(day_of_year))) %>%
  gather(key, value, -day_of_year)

ggplot(avg_df, aes(x = day_of_year, y = value, group = key, colour = key)) +
  xlab(str_c("Average model day of year")) +
  #geom_point(size = 2) +
  geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) 

ch_2006_df <- ch_samp_2006 %>% 
  select(day_of_year, chig_proportion, up95ci, lw95ci) %>%
  rename(chig_proportion_2006 = chig_proportion,
         up95ci_2006 = up95ci, 
         lw95ci_2006 = lw95ci ) %>%
  gather(key, value, -day_of_year)

combine_df <- bind_rows(avg_df, ch_2006_df)

ggplot(combine_df, aes(x = day_of_year, y = value, group = key, colour = key)) +
  xlab(str_c(" day of year")) +
  #geom_point(size = 2) +
  geom_smooth( method = "glm", se = TRUE, size = 2, method.args = list(family = quasibinomial)) 

avg_log_reg <- glm(chig_proportion ~ day_of_year, data = avg_df, family = "quasibinomial")
summary(avg_log_reg)
ggplot(avg_df , aes(day_of_year, chig_proportion)) + # the SE;s and Cis are very tight when we use sample size or inv_var for the weight. 
    xlab(str_c(" day of year")) +
    geom_point(size = 1) +
    geom_smooth( method = "glm", se = TRUE, size = 1, method.args = list(family = quasibinomial)) 

#This function is graphs the CI as it relates to the within sample variance produced from genetic estimates
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
logistic_by_year(anchor_df, 2015)

###########The code above is for the average logistic model

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

