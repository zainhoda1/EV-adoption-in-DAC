# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)


here()
p1 <- read_csv(here("data", "run 3", "DAC_P1_v2_3.csv"))
p2 <- read_csv(here("data", "run 3", "DAC_P2_v2_3.csv"))
p3 <- read_csv(here("data", "run 3", "DAC_P3_v2_3.csv"))
p4 <- read_csv(here("data", "run 3", "DAC_P4_v2_3.csv"))
p5 <- read_csv(here("data", "run 3", "DAC_P5_v2_3.csv"))


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
  select(session,created_p2, time_sec_p2, current_car_purchase, 
    current_car_status, current_car_price, monthlypayment,
    gas_mileage, weeklyTravel, fuel_type, parking, neighbor_info,
    household_vehicle, ride_friend__carpool, walk_bicycle_scooter,
    taxi__uber, public_transit, buying_timing, future_vehicle_type,
    future_car_condition, future_car_finance, future_car_price,
    future_downpayment, future_car_category)


p3 <- p3 %>% 
  mutate(
    created_p3 = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p3 = as.numeric(ended - created_p3, units = "secs")) %>%
  # Select important columns
  select(session,respondentID, created_p3, time_sec_p3)


p4 <- p4 %>% 
  mutate(
    created_p4 = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p4 = as.numeric(ended - created_p4, units = "secs")) %>%
  # Select important columns
  select(session,created_p4,
    time_sec_p4,  run_on_gas, plugged_in,
    electric_vehicle_Y_N, name_electric_vehicle, max_subsidy)


p5 <- p5 %>% 
  mutate(
    created_p5 = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p5 = as.numeric(ended - created_p5, units = "secs"),
    age = (2024 - yearOfBirth))  %>%
  # Select important columns
  select(session,created_p5, time_sec_p5,
    feedback,yearOfBirth, gender,
    hispanic, ethnicity, people, employment_status, income,education,
    housing, own_rent, electric_bill, political_views, party, age )


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
  filter(time_sec_p5> 0) %>%
  filter(time_sec_p3> 0) 
nrow(data)


#data01 <- data
#data02 <- data
#data03 <- data
#data04 <- data
#data06 <- data

#final_data <- rbind(rbind(rbind(rbind(data01,data02 ),data03),data04),data06)
data <- final_data

#write_csv(final_data, "data/demographic_info.csv")

median_age <-  median(data$age)  


gender_table <- data.frame(table(data$gender))


income_ranges <- data.frame(table(data$income))

#----------------------------------------------------
# Getting modes of transport data:

household <- data.frame(table(data$household_vehicle))  %>% 
        rename(household_vehicle = Freq)
transit<- data.frame(table(data$public_transit)) %>% 
        rename(public_transit = Freq)
walk<- data.frame(table(data$walk_bicycle_scooter)) %>% 
        rename(walk_bicycle_scooter = Freq)
ride<- data.frame(table(data$ride_friend__carpool)) %>% 
        rename(ride_friend_carpool = Freq)
taxi<- data.frame(table(data$taxi__uber)) %>% 
        rename(taxi_uber = Freq)


different_transportation_modes <- walk %>% 
  left_join(transit, by = c("Var1")) %>% 
  left_join(household, by = c("Var1"))  %>% 
  left_join(ride, by = c("Var1")) %>% 
  left_join(taxi, by = c("Var1"))  %>% 
  replace(is.na(.), 0)

#----------------------------------------------------

current_car_purchase_type<- data.frame(table(data$current_car_purchase))  %>% 
  rename(car_purchase_type = Var1, count = Freq)

Current_car_price <- data.frame(table(data$current_car_price))  %>% 
  rename(car_price_band = Var1, count = Freq)

monthly_payment <- data.frame(table(data$monthlypayment))  %>% 
  rename(monthly_payment_band = Var1, count = Freq)

refueling_frequncy <- data.frame(table(data$weeklyTravel))  %>% 
  rename(refueling_rate = Var1, count = Freq)

#-----------------------------------------------------

# --------- EV knowledge ---------

ev_subsidy <- data.frame(table(data$max_subsidy))  %>% 
  rename(subsidy = Var1, count = Freq)


values <- c('$1,000', '$2500', '$5000', '$7,500', '$10,000', 'I’m not sure')
subsidy <- c('1','2','3','4','5','6')
ev_subsidy_df <- data.frame(values, subsidy )

ev_subsidy_df <- ev_subsidy_df %>% 
  left_join(ev_subsidy, by = c( "subsidy")) 

# ------------------------------------------




