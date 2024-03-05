library(tidyverse)

# Just a data frame connecting the level name to the level id

levels <- c(
  'mileage', 
  'price', 
  'extended_warranty', 
  'cpo',
  'return_option', 
  'inspection', 
  'clean_title'
)
attributes <- data.frame(
  level_id = seq(length(levels)), 
  level_name = levels
)


# Generate all the possible combinations, then filter out only the ones
# where you have 5 unique attribute levels

n_atts <- 15
levels <- seq(n_atts)

full <- expand.grid(
  v1 = levels, 
  v2 = levels,
  v3 = levels,
  v4 = levels,
  v5 = levels
) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(
    names_to = 'version',
    values_to = 'level', 
    cols = -id
  ) %>% 
  group_by(id) %>% 
  mutate(n_levels = n_distinct(level)) 
candidates <- full %>% 
  filter(n_levels == 5) %>% 
  pivot_wider(names_from = version, values_from = level) %>% 
  select(-n_levels) %>% 
  # Overwrite ids
  ungroup() %>% 
  mutate(id = row_number())
  
# Randomly select from the candidates to make a design

n_resp <- 5000
n_q_per_resp <- 5
random_rows <- sample(
  x = seq(nrow(candidates)), 
  size = n_resp*n_q_per_resp, 
  replace = FALSE
)
design <- candidates[random_rows,]
design$resp_id <- rep(seq(n_resp), each = n_q_per_resp)
design$q_id <- rep(seq(n_q_per_resp), times = n_resp)
head(design)

