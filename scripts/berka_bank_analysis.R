# loading required libraries --------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggalluvial)

# loading other scripts do be used here ---------------------------------------
source("./scripts/00_setting_environment.R")
source("./scripts/01_functions.R")
source("./scripts/02_data_ingestion.R")

# performing data analysis ----------------------------------------------------

# gender distribution of clients in the bank
ggplot(data = client) +
  aes(x = gender) +
  geom_bar(fill = "#188977") +
  labs(title = "Gender distribution of clients in the bank",
    x = "Gender (m = men, w = women)",
    y = "Total clients") +
  theme_minimal()

# gender distribution of clients in the bank over the decades
clientGenderOverDecades <- client %>% 
  group_by(decade = as.integer(substr(client$birth_number, 1,1)) * 10, gender = client$gender) %>% 
  count()

ggplot(data = clientGenderOverDecades) +
  aes(x = decade, fill = gender, weight = n) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  geom_bar() +
  geom_line(aes(y = n, color = gender)) +
  labs(title = "Gender distribution of clients in the bank over the decades",
       subtitle = "Gender (m = men, w = women)",
       x = "Decades",
       y = "Total clients") +
  theme_minimal() +
  facet_wrap(vars(gender))

# alluvial diagram representation of gender, age group and region
clientGenderAgeGroupByRegion <- client %>% 
  mutate(age_group = ifelse(age < 21, "young", ifelse(age >= 21 & age <= 60, "adult", "senior"))) %>% 
  inner_join(district, by = "district_id") %>% 
  group_by(age_group, gender, region) %>% 
  count()
  
ggplot(data = clientGenderAgeGroupByRegion, aes(axis1 = region, axis2 = age_group, y = n)) +
  scale_x_discrete(limits = c("region", "age group"), expand = c(.1, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = gender)) +
  geom_stratum() + 
  geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("Alluvial diagram representation",
          "Gender and Age Group by Region")









