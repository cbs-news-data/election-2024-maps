library(tidyverse)
library(httr)
library(jsonlite)
library(tidyjson)
library(stringr)

# Get a vector of state abbreviations
state_abbreviations <- c(state.abb)

#get a vector of years
years = c("2016", "2020", "2024")

#set up empty dataframe
all_counties <- data.frame()

#loop through each year
for (year in years) {
  print(year)
  
  # Loop through each state abbreviation
  for (state in state_abbreviations) {
    
    json_url <- paste0("https://api-election.cbsnews.com/api/public/counties/",year,"/G/",state,"/P")
    
    # GET the JSON data
    response <- GET(json_url)
    json_data <- content(response, "text")
    data <- fromJSON(json_data, simplifyVector = FALSE)
    
    print(json_url)
    
    
  }
  
}



# json_url <- paste0("https://api-election.cbsnews.com/api/public/counties/2020/G/CA/P")
# 
# # GET the JSON data
# response <- GET(json_url)
# json_data <- content(response, "text")
# CA_data <- fromJSON(json_data, simplifyVector = FALSE)
# 
# 
# county_data <- CA_data$race$counties
# 
# county_data_unnest <- county_data %>% 
#   spread_all() %>% 
#   select(name, fips, pctExpVote, totalVote, timeStamp, candName, registration, totalVote, candVote, candPct) %>% #select only columns we need
#   as_data_frame.tbl_json() %>% 
#   mutate(year = "2020")
  
