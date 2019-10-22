# notes ----
# dissecting distributions
# sarah.power@alaska.gov
# 8/13/2018

# load ----
library(fpp2)
library(RDS)
library(tidyverse)
library(mixdist)
library(lubridate)
library(grid)
library(gridExtra)
library(cowplot)
library(zoo) # to convert numeric date back to a number
source('code/distribution_functions.R')

# data ----
# Chig_data only uses Chignik Bay District harvest
data2018 <- read_csv('data/ChigISGrunappt2006-2017catch.by.district9-3-19.csv') %>% 
  dplyr::select(-Earlycatch, -Latecatch, -Earlytotal, -Latetotal) %>%
  dplyr::rename(
    prop_early_genetics = Propotionearly, 
    early_esc_genetics = Earlyesc, 
    late_esc_genetics = Lateesc) %>%
  mutate(esc = early_esc_genetics + late_esc_genetics,
         date = mdy(Date),
         year = year(date),
         day_of_year = yday(date)) # %>% #-> chig_data # convert the Date to its numeric equivalent 

nas <- map_df(data2018, function(x) sum(is.na(x)))# figure out home many columns have NA's need purrr package

data2018[is.na(data2018)] <- 0 # replace NAs with 0.

# Defining harvest Change df_data to the one you want to run through.
# df_data <- dfweironly <- data2018 %>% mutate(harvest = 0)
# df_data <- df18lagoon <- data2018 %>% mutate(harvest = lagoon_272_10)
# df_data <- df18ocdb_272_20 <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20)
# df_data <- df18ocdb_272_30 <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20 + ocdb_272_30)
 df_data <- outercb <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20 + ocdb_272_30 + ocdb_272_40)
# df_data <- outercb <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20 + ocdb_272_30 + ocdb_272_40)
# df_data <- all <- data2018 %>% mutate(harvest = data2018 %>% select(lagoon_272_10:sedm80) %>% rowSums())
# df_data <- kujulik <- data2018 %>% mutate(harvest = data2018 %>% select(lagoon_272_10:kjbd_272_53) %>% rowSums())
# df_data <- kumlik <- data2018 %>% mutate(harvest = data2018 %>% select(lagoon_272_10:kmbd80_272_64) %>% rowSums())
# df_data <- west_kuj <- data2018 %>% mutate(harvest = data2018 %>% select(lagoon_272_10:kjbd_272_53, western) %>% rowSums())


# More data processing.
post_harvest <- function(df = data2018){
  df <- df %>% 
    mutate(run = esc + harvest, #catch is our estimate of what would have occured at the wier had there been no fishing, based on migration timing studies.
           run_early_gen = prop_early_genetics*run) %>%
    select(-c(lagoon_272_10:sedm80))
  return(df)
}

df_data <- post_harvest(df_data)



year_vector <- c(2006:2008,2010:2018)

dgen <- read_csv('data/chig_genetics_by_weir_date.csv') %>%
  mutate(date = as.Date(paste(year, month, day, sep = '-')),
         day_of_year = yday(date))
  

# figures ----
out <- list()
#p <-par(mfrow = c(2,5))
for(i in 1:length(year_vector) ){
  png(file = paste0("figures/early_genetics", year_vector[i], ".png"), height = 4, width = 6, units = "in", res = 300)
  out[[i]] <- early_look(df_data, year_vector[i])
  dev.off()
}
#par(p)
eout <- do.call("rbind", out)

for(i in 1:length(year_vector) ){
  png(file = paste0("figures/mixture_auto", year_vector[i], ".png"), height = 4, width = 6, units = "in", res = 300)
  #png(file = paste0("figures/mixture_auto", year_vector[i], ".png")) #, height = 4, width = 6, units = "in", res = 300)
  auto_year(df_data, year_vector[i])
  dev.off()
}

plots <- list()
for(i in 1:length(year_vector) ) {
  plots[[i]] <- year_stats(df_data, year_vector[i])
  ggsave(filename = paste0("figures/CDF",year_vector[i], ".png", sep = ""), device = png(), width = 6, height = 4, units = "in", dpi = 300)
}

do.call(grid.arrange, plots)
plots[[2]]

plots = lapply(1:9, function(.x) year_stats(,year_vector[i]))
require(gridExtra)
do.call(grid.arrange,  plots)

y06 <- year_stats(df_data, 2006)
y07 <- year_stats(df_data, 2007)
y08 <- year_stats(df_data, 2008)
y10 <- year_stats(df_data, 2010)
y11 <- year_stats(df_data, 2011)
y12 <- year_stats(df_data, 2012)
y13 <- year_stats(df_data, 2013)
y14 <- year_stats(df_data, 2014)
y15 <- year_stats(df_data, 2015)
y16 <- year_stats(df_data, 2016)
y17 <- year_stats(df_data, 2017)
y18 <- year_stats(df_data, 2018)



fig <- cowplot::plot_grid(y06, y07, y08, y10, y11, y12, y13, y14, y15, y16, y17, y18, ncol = 3)
fig

# y label
y.grob <- textGrob("Count of early run sockeye", gp=gpar(col="black", fontsize=15), rot=90)
#add y label to plot  
#https://stackoverflow.com/questions/33114380/centered-x-axis-label-for-muliplot-using-cowplot-package
fig <- grid.arrange(arrangeGrob(fig, left = y.grob))

fig <- plot_grid(fig, ncol = 1, rel_heights = c(1, 0.1)) # rel_heights values control title margins
fig <- add_sub(fig, "Day of year")

ggsave(filename = paste0("figures/fig_year_stats", ".png", sep = ""), device = png(), width = 7, height = 9, units = "in", dpi = 300)

#analysis ----


#grab the values from y06$df that match those in dgen
y06$df

m <- rbind(y06$df, y07$df, y08$df, y10$df, y11$df, y12$df, y13$df, y14$df, y15$df, y16$df, y17$df, y18$df) %>%
  dplyr::select(prop_early_genetics, date, year, day_of_year, dist_percent)

length(m$year)
length(dgen$year)
length(new$year)
head(new)

new <- dplyr::inner_join(dgen, m, by = c("year", "day_of_year")) %>%
  dplyr::select(-month, -day, -chig_proportion, -date.y)

ks.test(new$black_proportion, new$dist_percent)

new1 <- new %>%
  dplyr::filter(year == 2013) %>% 
  dplyr::select(black_proportion, dist_percent)# %>% #, day_of_year)

ks.test(new1$black_proportion, new1$dist_percent)

  

# might need to remove coding after this line

data <- df_data %>%
  group_by(year) %>%
  nest()

df06 <- data.frame(data[[2]][1])
df07 <- data.frame(data[[2]][2])
df08 <- data.frame(data[[2]][3])
df10 <- data.frame(data[[2]][4])
df11 <- data.frame(data[[2]][5])
df12 <- data.frame(data[[2]][6])
df13 <- data.frame(data[[2]][7])
df14 <- data.frame(data[[2]][8])
df15 <- data.frame(data[[2]][9])
df16 <- data.frame(data[[2]][10])
df17 <- data.frame(data[[2]][11])
df18 <- data.frame(data[[2]][12])

dfa <- df_data %>%
  select(year, day_of_year, run) %>%
  group_by(year) %>%
  nest() %>%
  mutate(fit = map(data, ~ distribution_estimation_norms_SEQ(df = .))) #,

fit06 <- dfa[[3]][1]
fit07 <- dfa[[3]][2]
fit08 <- dfa[[3]][3]
fit10 <- dfa[[3]][4]
fit11 <- dfa[[3]][5]
fit12 <- dfa[[3]][6]
fit13 <- dfa[[3]][7]
fit14 <- dfa[[3]][8]
fit15 <- dfa[[3]][9]
fit16 <- dfa[[3]][10]
fit17 <- dfa[[3]][11]
fit18 <- dfa[[3]][12]



#Must use group_by(day_of_year) %>% to get correct calculations
df06 %>%
  dplyr::group_by(day_of_year) %>% 
  dplyr::mutate(dist_percent = percent_dist(fit06, day_of_year),
                run_early_dis = dist_percent*run) -> df06 #doesn't work

df <- df06
df06 %>%
  dplyr::group_by(year) %>% 
  dplyr::mutate(cum_run_dis = cumsum(replace_na(run_early_dis, 0)),
                cum_run_gen = cumsum(replace_na(run_early_gen, 0))) %>%
  dplyr::ungroup(year) -> df #   %>% View(cum_run_gen) #

p_early_dis <- sum(df$run_early_dis)/sum(df$run)
p_early_gen <- sum(df$run_early_gen)/sum(df$run)


min(df$day_of_year, na.rm = TRUE)

# this is for simultaneous graphing. 
df_long <- df %>%
  gather(model_type, proportions, dist_percent, prop_early_genetics)
length(df_long$proportions)


ks.test(df$dist_percent, df$prop_early_genetics)

#ecdf.ksCI(df$dist_percent)

ggplot(df, aes(day_of_year, prop_early_genetics)) +
  geom_point(size=2) + theme_light()
ggplot(df, aes(day_of_year, dist_percent)) +
  geom_point(size=2) + theme_light()

ggplot(df_long, aes(x = day_of_year, y = proportions), color = model_type) +
  geom_point() + theme_light()




         dist1 = map(., ~ dnorm(.day_of_year, .[[3]][1][[1]][1][[1]][1, 2], .[[3]][1][[1]][1][[1]][1, 3])))

         dist1 = map(., ~ dnorm(.day_of_year, .[[3]][1][[1]][1][[1]][1, 2], .[[3]][1][[1]][1][[1]][1, 3])))
         dist_percent = map(data, ~ dnorm(day_of_year, .$) 
                              percent_dist(., .$day_of_year))

  dfa[[3]][1][[1]][1][[1]][1, 2] # last digit = 2 implies mu,  =3 implies sigma
  dfa[[2]][2][[1]][1]#day of year
  $parameters
  
  dnorm(x, fit$parameters$mu[dist_num],fit$parameters$sigma[dist_num])

  fit <- distribution_estimation_norms_SEQ(df_fit) 
  
  
  #Must use group_by(day_of_year) %>% to get correct calculations
  df %>%
    dplyr::group_by(day_of_year) %>% 
    dplyr::mutate(dist_percent = percent_dist(fit,day_of_year),
                  run_early_dis = dist_percent*run) -> df
  
  df %>%
    dplyr::group_by(year) %>% 
    dplyr::mutate(cum_run_dis = cumsum(replace_na(run_early_dis, 0)),
                  cum_run_gen = cumsum(replace_na(run_early_gen, 0))) %>%
    dplyr::ungroup(year) -> df #   %>% View(cum_run_gen) #


         

