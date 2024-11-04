library(sf)
library(tidyverse)
library(tidycensus)
library(stringr)


counties <- get_acs(
  geography = "county",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2022
)

counties_states <- counties %>% 
  separate(col = NAME, into = c("COUNTY", "STATE"), sep = "\\, ") %>% 
  filter(STATE != "Connecticut" & STATE != "Rhode Island" & STATE != "Massachusetts" & STATE != "Maine" & STATE != "New Hampshire" & STATE != "Vermont")

northeast <- sf::read_sf("data/Northeastern_States_Town_Boundary_Set.geojson")

northeast_states <- northeast %>% 
  filter(STATE_COD == "CT" | STATE_COD == "RI" | STATE_COD == "MA" | STATE_COD == "ME" | STATE_COD == "NH" | STATE_COD == "VT")

maine <- northeast %>% 
  filter(STATE_COD == "ME") %>% 
  filter(LABEL_FLAG == "True") %>% 
  select(STATE_COD, STATE_NAME, TOWN_NAME) %>% 
  mutate(TOWN_NAME = str_replace_all(TOWN_NAME, "Plt", "Plt."))

#test maine

json_url <- paste0("https://api-election.cbsnews.com/api/public/counties2/2024/G/ME/P")

# GET the JSON data
response <- GET(json_url)
json_data <- content(response, "text")
data <- fromJSON(json_data, simplifyVector = FALSE)


county_data <- data$race$counties
candidate_data <- data$race$candidates

remove(data) #remove OG data

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

county_candidate_data_clean_maine <- merge(county_candidate_data_clean, maine, by.x="name", by.y="TOWN_NAME", all.x=TRUE)
