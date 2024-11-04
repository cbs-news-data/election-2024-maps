library(tidyverse)
library(httr)
library(jsonlite)
library(tidyjson)
library(stringr)

county_names_fips <- read.csv("data/county_names.csv") %>% 
  mutate(GEOID = str_pad(as.character(GEOID), 5, pad = "0")) #add leading 0s

#get alaska changes 
ak_names <- read.csv("data/ak_names.csv") %>% 
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) #add leading 0s

# Get a vector of state abbreviations
state_abbreviations <- c(state.abb)

#set up empty dataframe
all_counties <- data.frame()

# Loop through each state abbreviation
for (state in state_abbreviations) {
  # Construct the URL with the current state abbreviation
  json_url <- paste0("https://api-election.cbsnews.com/api/public/counties/2024/G/", state, "/P")
  
  # GET the JSON data
  response <- GET(json_url)
  json_data <- content(response, "text")
  data <- jsonlite::fromJSON(json_data, simplifyVector = FALSE)
  #data <- fromJSON(json_data)
  
  print(state)
  print(json_url)
  
  county_data <- data$race$counties
  candidate_data <- data$race$candidates
  
  remove(data) #remove OG data
  
  county_data_unnest <- county_data %>%
    spread_all() %>% #converts json into rows/columns
    select(name, fips, totalExpVote, totalVote, timeStamp) %>% #select only columns we want/need
    mutate(state = state) %>% 
    enter_object(candidates) %>% #go into column that's still nested
    gather_array %>% #adds array numbers & duplicates rows to correspond to OG rows
    spread_all() %>% #converts candidate vote numbers into rows/columns
    select(name, fips, state, totalExpVote, totalVote, timeStamp, fullName, vote) %>% #select only columns we want/need
    as_data_frame.tbl_json() #drops the json column at the end that we don't need anymore
  
  county_candidate_data_clean <- county_data_unnest %>%
    pivot_wider(names_from = fullName, values_from = c(vote)) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>% 
    mutate(ts_datetime = as.POSIXct(timeStamp,format="%Y-%m-%dT%H:%M:%S", tz="GMT")) %>% #change datetime to datetime
    mutate(fips = case_when(fips == "29095c" ~ "29095",
                            fips == "29095s" ~ "29095",
                            TRUE ~ fips))
  
  county_candidate_data_clean_grouped <- county_candidate_data_clean %>% 
    group_by(state, fips) %>% 
    summarise(totalExpVote = sum(totalExpVote),
              totalVote = sum(totalVote),
              timeStamp = max(ts_datetime),
              `vote_Harris` = sum(`Kamala Harris`),
              `vote_Trump` = sum(`Donald Trump`))
  
  
  #bind to all_counties
  all_counties <- bind_rows(all_counties, county_candidate_data_clean_grouped)
  
}

all_counties_fix_padding <- all_counties %>% 
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) #add leading 0s

all_counties_names <- merge(all_counties_fix_padding, county_names_fips, by.x="fips", by.y="GEOID", all.x=TRUE) #merge


all_counties_clean <- all_counties_names %>% 
  mutate(vote_Other = totalVote-(`vote_Harris`+`vote_Trump`)) %>% 
  mutate(pctExpVote = (totalVote/totalExpVote)*100) %>% 
  mutate(pctExpVote = case_when(is.na(pctExpVote) == TRUE ~ 0,
                                TRUE ~ pctExpVote)) %>% 
  mutate(`pct_Harris` = (`vote_Harris`/totalVote)*100) %>%
  mutate(`pct_Trump` = (`vote_Trump`/totalVote)*100) %>%
  mutate(ts_pretty = format(as.POSIXct(timeStamp), format = "%B %d, %Y %I:%M %p", tz="America/New_York")) %>% #format it pretty with ET tz
  mutate(ts_pretty = str_replace_all(as.character(ts_pretty), " 0", " ")) %>% #get rid of leading zeros
  mutate(ts_pretty = paste0("Updated: ",ts_pretty, " ET")) %>% #add ET time zone at the end
  mutate(ts_pretty = case_when(ts_pretty == "Updated: December 31, 000 7:03 PM ET" ~ "No voting data yet", #if timestamp still placeholder, change it to "no voting data yet"
                               TRUE ~ ts_pretty)) %>% 
  mutate(fips_new = case_when(state == "AK" ~ str_replace_all(fips, "029", "020"),
                          TRUE ~ fips)) %>% 
  mutate(leader = case_when(`pct_Harris` > `pct_Trump` ~ "Harris",
                            `pct_Harris` < `pct_Trump` ~ "Trump",
                            TRUE ~ "NA")) %>% 
  mutate(at_least_20pct_in = case_when(pctExpVote >= 20 ~ "20pctExpVoteIn",
                                       TRUE ~ "lessThan20pctIn")) %>% 
  mutate(leader_margin = `pct_Harris` - `pct_Trump`) %>% 
  mutate(leader_margin_safe = case_when(at_least_20pct_in == "20pctExpVoteIn" ~  leader_margin,
                                   TRUE ~ NA)) %>% 
  mutate(leader_margin_abs = abs(leader_margin_safe))

all_counties_clean <- merge(all_counties_clean, ak_names, by="fips", all.x=TRUE) %>% 
  mutate(NAME = case_when(state == "AK" ~ ak_district,
                          TRUE ~ NAME)) %>% 
  select(-fips) %>% 
  rename(fips = fips_new)
  
write.csv(all_counties_clean, "output/all_counties_clean_2024.csv", row.names = FALSE)


