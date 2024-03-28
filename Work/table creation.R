# Load libraries
library(tidyverse)
library(cbcTools)
library(readr)
library(here)

att <- seq(17)
options <-seq(5)

n_resp <- 15000
result <- list()

atts <- data.frame(
  attribute = c(
    'Certified Pre-Owned (CPO) vehicle',
    'Brand/Make',
    'Number of previous owners',
    'Vehicle history report available',
    'Where was the vehicle previously used?',
    'Vehicle fuel efficiency',
    'Vehicle warranty',
    'Return window',
    'Pre-approved loan for the vehicle',
    'Pre-purchase independent inspection',
    'Test-driving the vehicle',
    'Mileage on the vehicle',
    'Purchase price',
    'Vehicle seating capacity',
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

# Removing duplicates
#design1<- design %>% distinct(respID, attID, .keep_all = TRUE)


# Check counts
table(design$issue)

info<-  design %>%
  group_by(respID, qID) %>%
  summarize(distinct_points = n_distinct(attID))

to_remove <- unique(info[info$distinct_points< 5, ]$respID)
to_remove

print(length(to_remove))

design2 <- design[!(design$respID %in% to_remove ),]

write_csv(design2, here('data', 'choice_options.csv'))
