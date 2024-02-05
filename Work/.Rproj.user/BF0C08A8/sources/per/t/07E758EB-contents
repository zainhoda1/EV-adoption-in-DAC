library(logitr)
library(tidyverse)
library(fastDummies)
library(here)



DOE_J40_DAC <-read_csv(here('data','DOE_J40_DAC.csv'))

DOE_J40_DAC

df <- filter(DOE_J40_DAC, DAC_indicator == 1)

hist(x = df$over_30min_commute_pct, 
     breaks = 10,
     main = "Percent of total population with a drive time to employment >= 30 minutes",
     ylab = "count of tract",
     xlab= "fraction of population")


df_outage <- filter(df, grid_outage_duration <= 100)

hist(x = df_outage$grid_outage_duration, 
                   breaks = 10,
                   main = "Average duration of power outage events (in minutes) that occurred for all census tracts in each county from 2017-2020 ",
                   ylab = "count of tract",
                   xlab= "Average duration (in minutes)")



hist(x = df$avg_transport_burden, 
     breaks = 20,
     main = "Transportation Costs % Income for the Regional Typical Household ",
     ylab = "count of tract",
     xlab= "% of Income")


df_no_car <- filter(df, no_car_pct <= 0.2)

hist(x = df_no_car$no_car_pct, 
     breaks = 20,
     main = "Percent of total population with no vehicle(s) available ",
     ylab = "count of tract",
     xlab= "% of Population")


df_low_pop <- filter(df, population <= 1000)

hist(x = df_low_pop$population, 
     breaks = 20,
     main = "population of Tract",
     ylab = "count of tract",
     xlab= "population count")



