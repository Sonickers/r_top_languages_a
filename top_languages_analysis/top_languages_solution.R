# Load necessary packages
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read_csv("stack_overflow_data.csv")

# R trends / Percentage of R questions in 2020
data_percentage <- data %>%
  mutate(percentage = (num_questions / year_total) * 100)

r_over_time <- data_percentage %>%
  filter(tag == "r")

r_percentage <- r_over_time %>%
  filter(year == 2020) %>%
  select(percentage) %>%
  as.numeric()

# Five most asked-about tags between 2015-2020
highest_tags <- data %>% 
  filter(year >= 2015) %>% 
  group_by(tag) %>% 
  summarize(tag_total = sum(num_questions)) %>% 
  arrange(desc(tag_total))

highest_tags <- head(highest_tags$tag, n = 5)

print(highest_tags)

# Tag percentage increase over time
percentage_years <- data %>%
  group_by(year) %>%
  mutate(year_total = sum(num_questions)) %>%
  ungroup() %>%
  mutate(percentage = num_questions / year_total * 100)

percentage_tags <- percentage_years %>%
  arrange(tag, year) %>%
  group_by(tag) %>%
  mutate(ratio = percentage / lag(percentage)) %>%
  ungroup()

highest_ratio_tag <- percentage_tags %>%
  slice_max(ratio, n = 1) %>%
  select(tag)

highest_ratio_tag <- highest_ratio_tag$tag

print(highest_ratio_tag)