# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)


here()
p1 <- read_csv(here("data", "run 1", "DAC_P1_v2_1.csv"))
p2 <- read_csv(here("data", "run 1", "DAC_P2_v2_1.csv"))
p3 <- read_csv(here("data", "run 1", "DAC_P3_v2_1.csv"))
p4 <- read_csv(here("data", "run 1", "DAC_P4_v2_1.csv"))
p5 <- read_csv(here("data", "run 1", "DAC_P5_v2_1.csv"))


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
  select(respondentID, session,created_p3, time_sec_p3,  starts_with("maxdiff"))


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
  filter(time_sec_p5> 0) %>%
  filter(time_sec_p3> 0) 
nrow(data)

no_of_respondents <- nrow(data)

median_age <-  median(data$age)  

data01 <- data
#data02 <- data
#data03 <- data
#data04 <- data
#data06 <- data

final_data <- rbind(rbind(rbind(rbind(data01,data02 ),data03),data04),data06)
#data <- final_data

#write_csv(final_data, "data/maxdiff_info.csv")

gender_table <- data.frame(table(data$gender))


income_ranges <- data.frame(table(data$income))

age_ranges <-  data.frame(table(data$age))




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

B_W <- ggplot(df1, aes(x = reorder(attribute, B_W), y = B_W)) +
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
  
  

Sqrt_B_W <- ggplot(df1, aes(x = reorder(attribute, relative_importance), y = relative_importance)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 5,          # Background color
           color = "white") + # Border color) +
  xlab("Relative Importance") +
  coord_flip() 

