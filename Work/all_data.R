# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)


data <- read_csv(here("data", "entire_data.csv"))

here()

# p1 <- read_csv(here("data", "run 6", "DAC_P1_v2_6.csv"))
# p2 <- read_csv(here("data", "run 6", "DAC_P2_v2_6.csv"))
# p3 <- read_csv(here("data", "run 6", "DAC_P3_v2_6.csv"))
# p4 <- read_csv(here("data", "run 6", "DAC_P4_v2_6.csv"))
# p5 <- read_csv(here("data", "run 6", "DAC_P5_v2_6.csv"))

# 
# p1 <- p1 %>% 
#   mutate(
#     created_p1 = ymd_hms(created, tz = "EST"),
#     ended =  ymd_hms(ended, tz = "EST"),
#     time_sec_p1 = as.numeric(ended - created_p1, units = "secs")) %>%
#   # Select important columns
#   select(session,created_p1, ip_address, time_sec_p1, mc_car_make_1 , zip_code)
# 
# 
# p2 <- p2 %>% 
#   mutate(
#     created_p2 = ymd_hms(created),
#     ended =  ymd_hms(ended),
#     time_sec_p2 = as.numeric(ended - created_p2, units = "secs")) %>%
#   # Select important columns
#   select(session,created_p2, time_sec_p2, current_car_purchase, 
#     current_car_status, current_car_price, monthlypayment,
#     gas_mileage, weeklyTravel, fuel_type, parking, neighbor_info,
#     household_vehicle, ride_friend__carpool, walk_bicycle_scooter,
#     taxi__uber, public_transit, buying_timing, future_vehicle_type,
#     future_car_condition, future_car_finance, future_car_price,
#     future_downpayment, future_car_category)
# 
# 
# p3 <- p3 %>% 
#   mutate(
#     created_p3 = ymd_hms(created),
#     ended =  ymd_hms(ended),
#     time_sec_p3 = as.numeric(ended - created_p3, units = "secs")) %>%
#   # Select important columns
#   select(session,respondentID, created_p3, time_sec_p3,  starts_with("maxdiff"))
# 
# 
# p4 <- p4 %>% 
#   mutate(
#     created_p4 = ymd_hms(created),
#     ended =  ymd_hms(ended),
#     time_sec_p4 = as.numeric(ended - created_p4, units = "secs")) %>%
#   # Select important columns
#   select(session,created_p4,
#     time_sec_p4,  run_on_gas, plugged_in,
#     electric_vehicle_Y_N, name_electric_vehicle, max_subsidy)
# 
# 
# p5 <- p5 %>% 
#   mutate(
#     created_p5 = ymd_hms(created),
#     ended =  ymd_hms(ended),
#     time_sec_p5 = as.numeric(ended - created_p5, units = "secs"),
#     age = (2024 - yearOfBirth))  %>%
#   # Select important columns
#   select(session,created_p5, time_sec_p5,
#     feedback,yearOfBirth, gender,
#     hispanic, ethnicity, people, employment_status, income,education,
#     housing, own_rent, electric_bill, political_views, party, age )
# 
# 
# # Join all parts together using the session variable
# data <- p1 %>% 
#   left_join(p2, by = "session") %>% 
#   left_join(p3, by = "session") %>% 
#   left_join(p4, by = "session") %>% 
#   left_join(p5, by = "session") %>% 
#   # No longer need session variable
#   select(-session) %>% 
#   mutate(total_time_min = (time_sec_p1+time_sec_p2+time_sec_p3+time_sec_p4+time_sec_p5)/60)
# 
# 
# 
# head(data)
# 
# 
# 
# # Filter out bad responses ---------
# 
# nrow(data)
# #2024-04-18 16:25:54
# 
# # Drop people who got screened out
# data <- data %>% 
#   filter(time_sec_p5> 0) %>%
#   filter(time_sec_p3> 0) 
# nrow(data)
# 
# 
# #data01 <- data
# #data02 <- data
# #data03 <- data
# #data04 <- data
# #data06 <- data
# 
# final_data <- rbind(rbind(rbind(rbind(data01,data02 ),data03),data04),data06)
# #data <- final_data
# 
# write_csv(final_data, "data/entire_data.csv")

# Drop people with income over $60,000
#data <- data %>% 
# filter(income ==  '$75,000 to less than $100,000')

low_income_group <- c('Prefer not to answer', 'Less than $15,000' ,
                      '$15,000 to less than $30,000', '$30,000 to less than $40,000',
                      '$40,000 to less than $50,000', '$50,000 to less than $60,000',
                      '$60,000 to less than $75,000')


data <- data %>% filter(income %in% low_income_group)

# nrow(data)

median_age <-  median(data$age)  


gender_table <- data.frame(table(data$gender))
gender_table <- gender_table %>% rename(  Gender = Var1, Frequency = Freq)


income_ranges <- data.frame(table(data$income))
income_ranges  <- income_ranges %>% rename(  Income_bracket = Var1, Frequency = Freq)

employment_ranges <- data.frame(table(data$employment_status))
employment_ranges  <- employment_ranges %>% rename( Employment_status = Var1, Frequency = Freq)


education_ranges <- data.frame(table(data$education))
education_ranges  <- education_ranges %>% rename( Education_status = Var1, Frequency = Freq)



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
  replace(is.na(.), 0) %>% 
  rename(usage = Var1)

#----------------------------------------------------

current_car_purchase_type<- data.frame(table(data$current_car_purchase))  %>% 
  rename(car_purchase_type = Var1, count = Frequency)

Current_car_price <- data.frame(table(data$current_car_price))  %>% 
  rename(car_price_band = Var1, count = Frequency)

monthly_payment <- data.frame(table(data$monthlypayment))  %>% 
  rename(monthly_payment_band = Var1, count = Frequency)

refueling_frequncy <- data.frame(table(data$weeklyTravel))  %>% 
  rename(refueling_rate = Var1, count = Frequency)

party_affiliation <- data.frame(table(data$party))  %>% 
  rename(Party = Var1, count = Freq)

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






# Create choice data ---------
#For best choice ------------
# First convert the data to long format
choiceData <- data %>% 
  pivot_longer(
    cols =  ends_with("best"),
    names_to = "qID",
    values_to = "choice") %>% 
  # Convert the qID variable to a number
  mutate(qID = parse_number(qID))


# Read in choice questions and join it to the choiceData
survey <- read_csv("https://raw.githubusercontent.com/zainhoda1/EV-adoption-in-DAC/main/Work/data/choice_options1.csv")
choiceData <- choiceData %>% 
  rename(respID = respondentID) %>% 
  left_join(survey, by = c("respID", "qID"))

# Convert choice column to 1 or 0 based on if the alternative was chosen 
choiceData <- choiceData %>% 
  mutate(choice = ifelse(choice == altID, 1, 0)) 
# Drop unused variables
#select(-image, -cbcPractice, -cbcAllSame)

head(choiceData)


choices_shown <- choiceData %>%  count(attribute)
selected_best <- choiceData %>% filter(choice ==1 ) %>%  count(attribute)

df = merge(x =choices_shown , y=selected_best , by = "attribute")
df <- df %>% rename(total_shown = n.x,  total_best = n.y)

attribute_showing_frequency <- median(df$total_shown) / no_of_respondents

# ---------------------------------------------------
# ---------------------------------------------------
# ---------------------------------------------------

#For worst choice ------------
# First convert the data to long format
choiceData1 <- data %>% 
  pivot_longer(
    cols =  ends_with("worst"),
    names_to = "qID",
    values_to = "choice") %>% 
  # Convert the qID variable to a number
  mutate(qID = parse_number(qID))


# Read in choice questions and join it to the choiceData
survey <- read_csv("https://raw.githubusercontent.com/zainhoda1/EV-adoption-in-DAC/main/Work/data/choice_options1.csv")
choiceData1 <- choiceData1 %>% 
  rename(respID = respondentID) %>% 
  left_join(survey, by = c("respID", "qID"))

# Convert choice column to 1 or 0 based on if the alternative was chosen 
choiceData1 <- choiceData1 %>% 
  mutate(choice = ifelse(choice == altID, 1, 0)) 
# Drop unused variables
#select(-image, -cbcPractice, -cbcAllSame)

selected_worst <- choiceData1 %>% filter(choice ==1 ) %>%  count(attribute)

df1 = merge(x =df , y=selected_worst , by = "attribute")
df1 <- df1 %>% rename(  total_worst = n)

df1$B_W <- (df1$total_best- df1$total_worst) 

df1$avg_B_W <- ((df1$total_best- df1$total_worst) / attribute_showing_frequency) /no_of_respondents



# install.packages("ggplot2")
library(ggplot2)

#B_W <- 
ggplot(df1, aes(x = reorder(attribute, B_W), y = B_W)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 4,          # Background color
           color = "white") + # Border color) +
  xlab("attribute") +
  ylab("Best-Worst") +
  coord_flip() 



#-----------------------------------------------------------------

# second way of inferring from BWS:


df1$Sqrt_B_W  <- sqrt(df1$total_best / (df1$total_worst+0.5))
max_value_sqrt <-  max(df1$Sqrt_B_W) 
df1$relative_importance <-  (df1$Sqrt_B_W * 100) / max_value_sqrt



#Sqrt_B_W <-
ggplot(df1, aes(x = reorder(attribute, relative_importance), y = relative_importance)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 5,          # Background color
           color = "white") + # Border color) +
  xlab("Attributes") +
  coord_flip() 


#----------------------------------------------------------------


#gender_distribution <-
ggplot(gender_table, aes(x = reorder(Gender,Frequency), y = Frequency)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 5,          # Background color
           color = "white") + # Border color) +
  xlab("Attributes") #+coord_flip() 

#----------------------------------------------------------------


#education_distribution <-
ggplot(education_ranges, aes(x = Frequency, y = Education_status)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 5,          # Background color
           color = "white") + # Border color) +
  xlab("Count") #+coord_flip() 

#----------------------------------------------------------------


#Refueling <-

ggplot(party_affiliation, aes(x = reorder(Party, count), y = count)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 5,          # Background color
           color = "white") + # Border color) +
  xlab("Party Affiliation") +
  coord_flip() 




#----------------------------------------------------------------




#Party Affiliation <-

ggplot(refueling_frequncy, aes(x = reorder(refueling_rate, count), y = count)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 5,          # Background color
           color = "white") + # Border color) +
  xlab("Refueling Rate") +
  coord_flip() 




#----------------------------------------------------------------

  
  
  

  


