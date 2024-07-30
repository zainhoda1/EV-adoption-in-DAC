# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)
library(kableExtra)


mtcars %>% kbl(format = 'latex', booktabs = TRUE)
here()
data <- read_csv(here("data", "demographic_info.csv"))


median_age <-  median(data$age)  


gender_table <- data.frame(table(data$gender))

gender_table %>% kbl(format = 'latex', booktabs = TRUE)


income_ranges <- data.frame(table(data$income))
salary_order <- - data.frame(8,9,2,10,11,12,13,3,4,5,6,7,1,14)
#income_ranges1 <- bind_cols(income_ranges, salary_order)

#income_ranges1 <- rbind(income_ranges , data.frame(salary_order = c(8,9,2,10,11,12,13,3,4,5,6,7,1,14)))

income_ranges %>% kbl(format = 'latex', booktabs = TRUE)

employment <- data.frame(table(data$employment_status))
employment %>% kbl(format = 'latex', booktabs = TRUE)

education_range <- data.frame(table(data$education))

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

different_transportation_modes %>% kbl(format = 'latex', booktabs = TRUE)

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

# ------------------- EV knowledge -------------------

ev_subsidy <- data.frame(table(data$max_subsidy))  %>% 
  rename(subsidy = Var1, count = Freq)


values <- c('$1,000', '$2500', '$5000', '$7,500', '$10,000', 'I’m not sure')
subsidy <- c('1','2','3','4','5','6')
ev_subsidy_df <- data.frame(values, subsidy )

ev_subsidy_df <- ev_subsidy_df %>% 
  left_join(ev_subsidy, by = c( "subsidy")) 

# ----------------------------------------------------

education_table <- data.frame(table(data$education))

education_table %>% kbl(format = 'latex', booktabs = TRUE)

