# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)

att <- seq(22)
options <-seq(5)

n_resp <- 100
result <- list()

atts <- data.frame(
  attribute = c(
    'Certified Pre-Owned (CPO) vehicle',
    'Brand reputation',
    'Maintenance records available',
    'Number of previous owners',
    'Vehicle history report available',
    'Where was the vehicle bought/used?',
    'Vehicle fuel efficiency',
    'Vehicle warranty',
    'Customer satisfaction return window',
    'Spare tires and jacking tools included',
    'Pre-approved loan for the vehicle',
    'Pre-purchase independent inspection',
    'Test-driving the vehicle',
    'Mileage on the vehicle',
    'Purchase price',
    'Vehicle seating capacity',
    'Options and features',
    'Resale value',
    'Insurance cost',
    'Vehicle age',
    'Interior/exterior condition',
    'Cargo space'
  )
)
atts$attID <- seq(nrow(atts))

n_q <- 6
n_atts <- 5

for (i in 1:n_resp) {
  
  # Get random attribute IDs
  set1 <- sample(x = att, size = nrow(atts), replace = FALSE)
  set2 <- sample(x = att, size = n_q*n_atts - nrow(atts), replace = FALSE)
  design <- data.frame(
    respID = i, 
    qID = rep(seq(n_q), each = n_atts),
    altID = rep(options, times = n_q),
    attID = c(set1, set2)
  )
  result[[i]] <- design
}

design <- do.call(rbind, result)

# Check counts
table(design$attID)

# Join on attribute labels
design <- design %>%
  left_join(atts, by = 'attID')

# Check counts
table(design$issue)

write_csv(design, here('data', 'choice_options.csv'))
