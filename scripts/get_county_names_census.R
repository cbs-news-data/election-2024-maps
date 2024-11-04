library(tidycensus)
library(tidyverse)
library(httr)
library(jsonlite)
library(tidyjson)
library(stringr)

counties <- get_acs(
  geography = "county",
  variables = "B01001_001",
  year = 2020
) %>% 
  select(GEOID, NAME)

write.csv(counties, "data/county_names.csv", row.names = FALSE)


#alaska data

json_url <- paste0("https://api-election.cbsnews.com/api/public/counties/2020/G/AK/P")

# GET the JSON data
response <- GET(json_url)
json_data <- content(response, "text")
AK_data <- fromJSON(json_data, simplifyVector = FALSE)


county_data <- AK_data$race$counties
candidate_data <- AK_data$race$candidates

remove(CT_data) #remove OG data

county_data_unnest <- county_data %>%
  spread_all() %>% #converts json into rows/columns
  select(name, fips, totalExpVote, totalVote, timeStamp) %>% #select only columns we want/need
  enter_object(candidates) %>% #go into column that's still nested
  gather_array %>% #adds array numbers & duplicates rows to correspond to OG rows
  spread_all() %>% #converts candidate vote numbers into rows/columns
  select(name, fips, totalExpVote, totalVote, timeStamp, fullName, vote) %>% #select only columns we want/need
  as_data_frame.tbl_json() #drops the json column at the end that we don't need anymore

county_candidate_data_clean <- county_data_unnest %>%
  pivot_wider(names_from = fullName, values_from = c(vote)) %>%
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>% 
  mutate(ts_datetime = as.POSIXct(timeStamp,format="%Y-%m-%dT%H:%M:%S", tz="GMT")) #change datetime to datetime

AK_NAMES <- county_candidate_data_clean %>% 
  select(name, fips) %>% 
  rename(ak_district = name) %>% 
  mutate(ak_district = paste0(ak_district, ", Alaska"))

write.csv(AK_NAMES, "data/ak_names.csv", row.names = FALSE)
