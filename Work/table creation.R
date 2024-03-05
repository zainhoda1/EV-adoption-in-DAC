# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)

att <- seq(22)
options <-seq(5)

n_resp <- 100
result <- list()

#set3 <- sample(x = att, size = 22, replace = FALSE)

for (i in 1:n_resp) {
  
  # Get random attribute IDs
  set1 <- sample(x = att, size = 22, replace = FALSE)
  set2 <- sample(x = att, size = 8, replace = FALSE)
  design <- data.frame(
    respID = i, 
    qID = rep(seq(6), each = 5), 
    rep(options, times= 6),
    attID = c(set1, set2)
  )
  result[[i]] <- design
}

design <- do.call(rbind, result)

# Check counts
table(design$attID)


design$issue <- with(design, ifelse(attID == 1, 'Certified Pre-Owned (CPO) vehicle',
                              ifelse(attID ==2 , 'Brand reputation',
                              ifelse(attID ==3 , 'Maintenance records available',     
                              ifelse(attID ==4 , 'Number of previous owners',
                              ifelse(attID ==5 , 'Vehicle history report available',
                              ifelse(attID ==6 , 'Where was the vehicle bought/used?',
                              ifelse(attID ==7 , 'Vehicle fuel efficiency',
                              ifelse(attID ==8 , 'Vehicle warranty',
                              ifelse(attID ==9 , 'Customer satisfaction return window',
                              ifelse(attID ==10 , 'Spare tires and jacking tools included',     
                              ifelse(attID ==11 , 'Pre-approved loan for the vehicle',
                              ifelse(attID ==12 , 'Pre-purchase independent inspection',
                              ifelse(attID ==13 , 'Test-driving the vehicle',
                              ifelse(attID ==14 , 'Mileage on the vehicle',
                              ifelse(attID ==15 , 'Purchase price',
                              ifelse(attID ==16 , 'Vehicle seating capacity',
                              ifelse(attID ==17 , 'Options and features',
                              ifelse(attID ==18 , 'Resale value',     
                              ifelse(attID ==19 , 'Insurance cost',
                              ifelse(attID ==20 , 'Vehicle age',
                              ifelse(attID ==21 , 'Interior/exterior condition',
                              ifelse(attID ==22 , 'Cargo space', 'empty'
                                     )))))))))))))))))))))))


# Check counts
table(design$issue)

write_csv(design, here('data', 'choice_options.csv'))
