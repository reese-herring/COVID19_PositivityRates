#install necessary R packages
install.packages("readr")
install.packages("tidyverse")

#load necessary libraries
library(tidyverse)
library(readr)

#load dataset
covid_df <- read.csv("covid19.csv")

#examine metadata
dim(covid_df)
vector_cols <- colnames(covid_df)
vector_cols

head(covid_df)
glimpse(covid_df)

#create tibble of containing just country level data
covid_df_all_states <- covid_df %>%
  filter(Province_State == "All States") %>%
  select(-Province_State)

#filter out fully cumulative data and keep daily COVID-19 positivity counts
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)

#find top 10 most tested countries and arrange in descending order
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarize(tested = sum(daily_tested),
            positive = sum(daily_positive),
            active = sum(active),
            hospitalized = sum(hospitalizedCurr)) %>%
  arrange(-tested)

covid_df_all_states_daily_sum

covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
covid_top_10

#create vectors of each column from table of top 10 most tested countries
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

#find positivity rate by country and identify top 3
positive_cases/tested_cases
positive_tested_top_3 <- c("United Kingdom" = 0.113, "United States" = 0.109, 
                          "Turkey" = 0.081)
positive_tested_top_3

#Create matrix of top ciuntiries with top 3 positivity rates
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163942, 2980960, 0)

covid_mat <- rbind(united_kingdom, united_states, turkey)
colnames(covid_mat) <- c("Ratio", "Tested", "Positive", "Active", "Hospitalized")
covid_mat

question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)

data_structure_list <- list(covid_df, covid_df_all_states,
                            covid_df_all_states_daily, covid_df_all_states_daily_sum,
                            covid_top_10, covid_mat, countries, tested_cases, 
                            positive_cases, active_cases, hospitalized_cases)
covid_analysis_list <- list(question, answer, data_structure_list)
covid_analysis_list[[2]]


