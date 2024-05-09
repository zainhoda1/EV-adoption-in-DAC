
# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)



data <- read_csv(here('data', 'choice_options1.csv'))


total <- 20238

test<- seq(from=1, to = total, by =1)
rows <- rep(test, times = 5)
my_dataframe <- (as.data.frame(rows))

use<- my_dataframe[order(my_dataframe$rows),]

length(use)

data$obsID  <-use

# Simulate choices ----

# Simulate choices based on a utility model

data_mnl1 <- cbc_choices(
  design = data,
  obsID = "obsID"
)

names(data_mnl1)[names(data_mnl1) == 'choice'] <- 'max_pref'


# Simulate choices based on a utility model

data_mnl2 <- cbc_choices(
  design = data_mnl1,
  obsID = "obsID"
)


names(data_mnl2)[names(data_mnl2) == 'choice'] <- 'min_pref'

write_csv(data_mnl2, here('data', 'simulated_data.csv'))



max_pref <- data_mnl2 %>%
   filter(max_pref > 0) %>%
  count(attribute, max_pref, sort = TRUE)

min_pref <- data_mnl2 %>%
  filter(min_pref > 0) %>%
  count(attribute, min_pref, sort = TRUE)



