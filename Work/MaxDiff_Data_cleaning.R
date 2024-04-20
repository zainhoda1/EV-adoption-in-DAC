# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)


here()
p1 <- read_csv(here("data", "survey_data_download", "DAC_P1.csv"))
p2 <- read_csv(here("data", "survey_data_download", "DAC_P2.csv"))
p3 <- read_csv(here("data", "survey_data_download", "DAC_P3.csv"))
p4 <- read_csv(here("data", "survey_data_download", "DAC_P4.csv"))
p5 <- read_csv(here("data", "survey_data_download", "DAC_P5.csv"))


p1 <- p1 %>% 
  mutate(
    created = ymd_hms(created, tz = "EST"),
    ended =  ymd_hms(ended, tz = "EST"),
    time_sec_p1 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session,ip_address, time_sec_p1, mc_car_make_1 , mc_car_make_2, zip_code)


p2 <- p2 %>% 
  mutate(
    created = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p2 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session, time_sec_p2, weeklyTravel, parking)


p3 <- p3 %>% 
  mutate(
    created = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p3 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session, time_sec_p3)


p4 <- p4 %>% 
  mutate(
    created = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p4 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session, time_sec_p4, name_electric_vehicle, max_subsidy)


p5 <- p5 %>% 
  mutate(
    created = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p5 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session,created, time_sec_p5, yearOfBirth, income, feedback)


# Join all parts together using the session variable
data <- p1 %>% 
  left_join(p2, by = "session") %>% 
  left_join(p3, by = "session") %>% 
  left_join(p4, by = "session") %>% 
  left_join(p5, by = "session") %>% 
  # No longer need session variable
  select(-session)
head(data)


# Filter out bad responses ---------

nrow(data)
#2024-04-18 16:25:54

# Drop people who got screened out
data <- data %>% 
  filter(created > '2024-04-16 00:00:00')   %>% 
  filter(time_sec_p1 > 0) 
nrow(data)
