# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)
library(kableExtra)


mtcars %>% kbl(format = 'latex', booktabs = TRUE)
here()
data <- read_csv(here("data", "maxdiff_info.csv"))

no_of_respondents <- nrow(data)



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

df1$avg_B_W <- round(((df1$total_best- df1$total_worst) / attribute_showing_frequency) /no_of_respondents, digits = 2)




# install.packages("ggplot2")
library(ggplot2)

B_W <- ggplot(df1, aes(x = reorder(attribute, avg_B_W), y = avg_B_W)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 4,          # Background color
           color = "white") + # Border color) +
  xlab("Attribute") +
  ylab("Average Best-Worst") +
  coord_flip() 


#data <- read_csv(here("data", "maxdiff_info.csv"))
ggsave("B_W.png", B_W, path = here("figs")) 

#-----------------------------------------------------------------

# second way of inferring from BWS:


df1$Sqrt_B_W  <- round(sqrt(df1$total_best / (df1$total_worst+0.5)), digits = 2)
max_value_sqrt <-  max(df1$Sqrt_B_W) 
df1$relative_importance <-  round((df1$Sqrt_B_W * 100) / max_value_sqrt, digits = 2)



Sqrt_B_W <- ggplot(df1, aes(x = reorder(attribute, relative_importance), y = relative_importance)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           fill = 5,          # Background color
           color = "white") + # Border color) +
  xlab("Attribute") +
  coord_flip() 

ggsave("Sqrt_B_W.png", Sqrt_B_W, path = here("figs")) 

# ---------------------------------------------------------------

df_temp <- df1 %>% arrange(desc(avg_B_W))

df_temp %>% kbl(format = 'latex', booktabs = TRUE)

