# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)


here()
p1 <- read_csv(here("data", "survey_data_download_v2", "DAC_P1_v2.csv"))
p2 <- read_csv(here("data", "survey_data_download_v2", "DAC_P2_v2.csv"))
p3 <- read_csv(here("data", "survey_data_download_v2", "DAC_P3_v2.csv"))
p4 <- read_csv(here("data", "survey_data_download_v2", "DAC_P4_v2.csv"))
p5 <- read_csv(here("data", "survey_data_download_v2", "DAC_P5_v2.csv"))


p1 <- p1 %>% 
  mutate(
    created_p1 = ymd_hms(created, tz = "EST"),
    ended =  ymd_hms(ended, tz = "EST"),
    time_sec_p1 = as.numeric(ended - created_p1, units = "secs")) %>%
  # Select important columns
  select(session,created_p1, ip_address, time_sec_p1, mc_car_make_1 , zip_code)


p2 <- p2 %>% 
  mutate(
    created_p2 = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p2 = as.numeric(ended - created_p2, units = "secs")) %>%
  # Select important columns
  select(session,created_p2, time_sec_p2, weeklyTravel, parking)


p3 <- p3 %>% 
  mutate(
    created_p3 = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p3 = as.numeric(ended - created_p3, units = "secs")) %>%
  # Select important columns
  select(session,created_p3, time_sec_p3)


p4 <- p4 %>% 
  mutate(
    created_p4 = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p4 = as.numeric(ended - created_p4, units = "secs")) %>%
  # Select important columns
  select(session,created_p4, time_sec_p4, name_electric_vehicle, max_subsidy)


p5 <- p5 %>% 
  mutate(
    created_p5 = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p5 = as.numeric(ended - created_p5, units = "secs"),
    age = (2024 - yearOfBirth))  %>%
  # Select important columns
  select(session,created_p5, time_sec_p5,gender, yearOfBirth, age, income, feedback)


# Join all parts together using the session variable
data <- p1 %>% 
  left_join(p2, by = "session") %>% 
  left_join(p3, by = "session") %>% 
  left_join(p4, by = "session") %>% 
  left_join(p5, by = "session") %>% 
  # No longer need session variable
  select(-session) %>% 
  mutate(total_time_min = (time_sec_p1+time_sec_p2+time_sec_p3+time_sec_p4+time_sec_p5)/60)
  


head(data)



# Filter out bad responses ---------

nrow(data)
#2024-04-18 16:25:54

# Drop people who got screened out
data <- data %>% 
  filter(time_sec_p5> 0) 
nrow(data)


median_age <-  median(data$age)  


gender_table <- data.frame(table(data$gender))


income_ranges <- data.frame(table(data$income))
