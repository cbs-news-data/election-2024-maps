library(tidyverse)
library(httr)
library(jsonlite)
library(tidyjson)
library(stringr)


# Get a vector of state abbreviations
state_abbreviations <- c(state.abb)

#set up empty dataframe
all_counties <- data.frame()

# Loop through each state abbreviation
for (state in state_abbreviations) {
  # Construct the URL with the current state abbreviation
  json_url <- paste0("https://api-election.cbsnews.com/api/public/counties2/2024/G/", state, "/P")
  
  # GET the JSON data
  response <- GET(json_url)
  json_data <- content(response, "text")
  data <- fromJSON(json_data, simplifyVector = FALSE)
  
  print(state)
  print(json_url)
  
  county_data <- data$race$counties
  candidate_data <- data$race$candidates
  
  remove(data) #remove OG data
  
  county_data_unnest <- county_data %>%
    spread_all() %>% #converts json into rows/columns
    select(name, fips, pctExpVote, totalVote, ts) %>% #select only columns we want/need
    enter_object(candidates) %>% #go into column that's still nested
    gather_array %>% #adds array numbers & duplicates rows to correspond to OG rows
    spread_all() %>% #converts candidate vote numbers into rows/columns
    select(name, fips, pctExpVote, totalVote, ts, vote, pct, id) %>% #select only columns we want/need
    as_data_frame.tbl_json() #drops the json column at the end that we don't need anymore
  
  remove(county_data) #remove nested county data
  
  candidate_data_unnest <- candidate_data %>%
    spread_all() %>% #converts json into rows/columns
    select(raceCandidateId, color, party, lastName, fullName, id) %>% #select only columns we want/need
    as_data_frame.tbl_json() #drops the json column at the end that we don't need anymore
  
  remove(candidate_data) #remove nested candidate data
  
  county_candidate_data <- merge(county_data_unnest, candidate_data_unnest, by="id", all.x = TRUE) %>%
    select(name, fips, pctExpVote, totalVote, ts, vote, pct, fullName)
  
  remove(county_data_unnest, candidate_data_unnest) #remove not merged data
  
  county_candidate_data_clean <- county_candidate_data %>%
    pivot_wider(names_from = fullName, values_from = c(vote, pct)) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>% 
    mutate(state = state)
  
  #THIS IS FOR TESTING!!! GENERATE RANDOM NUMBERS FOR pctExpVote, vote_Harris, vote_Trump, pct_Harris, pct_Trump
  county_candidate_data_clean$pctExpVote <- sample(100, size = nrow(county_candidate_data_clean), replace = TRUE)
  county_candidate_data_clean$vote_Harris <- sample(100, size = nrow(county_candidate_data_clean), replace = TRUE)
  county_candidate_data_clean$vote_Trump <- sample(100, size = nrow(county_candidate_data_clean), replace = TRUE)
  county_candidate_data_clean$pct_Harris <- sample(100, size = nrow(county_candidate_data_clean), replace = TRUE)
  county_candidate_data_clean$pct_Trump <- sample(100, size = nrow(county_candidate_data_clean), replace = TRUE)
  
  #bind to all_counties
  all_counties <- bind_rows(all_counties, county_candidate_data_clean)
  
}

all_counties_clean_TEST <- all_counties %>% 
  mutate(ts = case_when(state == "AL" ~ "2024-11-05T10:25:25Z", #manually change timestamp so we can test the case_when
                        TRUE ~ ts)) %>% 
  mutate(vote_Other = totalVote-(`vote_Harris`+`vote_Trump`),
         pct_Other = 100-(`pct_Harris`+`pct_Trump`)) %>% #get "other" votes that aren't main candidates
  select(fips, name, state, pctExpVote, totalVote, `vote_Harris`, `vote_Trump`, vote_Other, `pct_Harris`, `pct_Trump`, pct_Other, ts) %>%  #select only the columns we want
  mutate(ts_datetime = as.POSIXct(ts,format="%Y-%m-%dT%H:%M:%SZ", tz="GMT")) %>% #change datetime to datetime
  mutate(ts_pretty = format(as.POSIXct(ts_datetime), format = "%B %d, %Y %I:%M %p", tz="America/New_York")) %>% #format it pretty with ET tz
  mutate(ts_pretty = str_replace_all(as.character(ts_pretty), " 0", " ")) %>% #get rid of leading zeros
  mutate(ts_pretty = paste0(ts_pretty, " ET")) %>% #add ET time zone at the end
  mutate(ts_pretty = case_when(ts_pretty == "January 1, 001 12:03 AM ET" ~ "No voting data yet", #if timestamp still placeholder, change it to "no voting data yet"
                               TRUE ~ ts_pretty)) %>% 
  mutate(fips = str_pad(as.character(fips), 5, pad = "0")) %>% #add leading 0s
  mutate(fips = case_when(state == "AK" ~ str_replace_all(fips, "029", "020"),
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
  
write.csv(all_counties_clean_TEST, "output/TEST_TEST_all_counties_clean_2024_TEST_TEST.csv", row.names = FALSE)


