library(tidyverse)
library(httr)
library(jsonlite)
library(rjson)
library(tidyjson)

#CA_data <- rjson::fromJSON(file="https://api-election.cbsnews.com/api/public/counties2/2020/G/CA/P")

json_url <- paste0("https://api-election.cbsnews.com/api/public/counties2/2020/G/CA/P")

# GET the JSON data
response <- GET(json_url)
json_data <- content(response, "text")
CA_data <- fromJSON(json_data, simplifyVector = FALSE)


county_data <- CA_data$race$counties
candidate_data <- CA_data$race$candidates

remove(CA_data) #remove OG data

county_data_unnest <- county_data %>%
  spread_all() %>% #converts json into rows/columns
  select(name, fips, precinctsOf, precinctsIn, pctPrecIn, pctExpVote, totalVote, ts) %>% #select only columns we want/need
  enter_object(candidates) %>% #go into column that's still nested
  gather_array %>% #adds array numbers & duplicates rows to correspond to OG rows
  spread_all() %>% #converts candidate vote numbers into rows/columns
  select(name, fips, precinctsOf, precinctsIn, pctPrecIn, pctExpVote, totalVote, ts, vote, pct, id) %>% #select only columns we want/need
  as_data_frame.tbl_json() #drops the json column at the end that we don't need anymore

remove(county_data) #remove nested county data

candidate_data_unnest <- candidate_data %>%
  spread_all() %>% #converts json into rows/columns
  select(raceCandidateId, color, party, lastName, fullName, id) %>% #select only columns we want/need
  as_data_frame.tbl_json() #drops the json column at the end that we don't need anymore

remove(candidate_data) #remove nested candidate data

county_candidate_data <- merge(county_data_unnest, candidate_data_unnest, by="id", all.x = TRUE) %>%
  select(name, fips, precinctsOf, precinctsIn, pctPrecIn, pctExpVote, totalVote, ts, vote, pct, fullName)

remove(county_data_unnest, candidate_data_unnest) #remove not merged data

county_candidate_data_clean <- county_candidate_data %>%
  pivot_wider(names_from = fullName, values_from = c(vote, pct)) %>%
  mutate(across(everything(), .fns = ~replace_na(.,0)))

write.csv(county_candidate_data_clean, "county_candidate_data_clean.csv", row.names = FALSE)

remove(county_candidate_data) #remove not cleaned data