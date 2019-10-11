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
#library(gridExtra)
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

# Defining harvest Change df_data to the one you want to run through. 
# df_data <- df18lagoon <- data2018 %>% mutate(harvest = lagoon_272_10)
# df_data <- df18ocdb_272_20 <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20)
# df_data <- df18ocdb_272_30 <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20 + ocdb_272_30)
# df_data <- outercb <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20 + ocdb_272_30 + ocdb_272_40)
# df_data <- outercb <- data2018 %>% mutate(harvest = lagoon_272_10 + ocdb_272_20 + ocdb_272_30 + ocdb_272_40)
 df_data <- all <- data2018 %>% mutate(harvest = data2018 %>% select(lagoon_272_10:sedm80) %>% rowSums())
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



fig <- cowplot::plot_grid(y06, y07, y08, y10, y11,  y12, ncol = 3)
fig

df_data %>% group_split(year)%>%
  map_df(year_stats(., year))

         
df_data %>%
  group_by(year) %>%
  nest() %>%
  mutate( graph = map_df(year_stats(., year))) %>%
  unnest()

