library(tidyverse)

setwd('.')
# Source: https://www.umassd.edu/cas/polisci/resources/us-district-court-database/
courts <- readxl::read_excel('data/courts_data.xlsx')

# adminsident who Appointed Judge
# Limit to FDR and later for consistent party policy
# 32 Franklin D. Roosevelt 1
# 33  Harry Truman 1
# 34  Dwight Eisenhower 0
# 35 John F. Kennedy 1
# 36 Lyndon Johnson 1
# 37  Richard Nixon 0
# 38  Gerald Ford 0 
# 39 Jimmy Carter 1
# 40  Ronald Reagan 0
# 41  George H. W. Bush 0
# 42  Bill Clinton 1
# 43  George W. Bush 0
# 44  Barack Obama 1 

# Label each as either rep 0 or dem 1
# This is by hand
dems <- c(32,33,35,36,39,42,44)

env_cases <- courts %>% 
  # Limit to FDR and later 
  # Only environmental cases
  filter(casetype == 19, appres >= 32) %>% 
  # Convert to binary categorical
  mutate(dem_appointed = (appres %in% dems)*1,
  # Recast Dem = 1, Rep = 0, Other = 2
  # Really tough case when error that I was stuck on for thirty minutes
  party = if_else(party == 1,1,if_else(party == 2,0,3)))

# Source: https://www.kaggle.com/harshitagpt/us-presidents
admins <- read_csv('data/us_presidents.csv')

admins <- admins %>% 
  # Regex to get start dates
  # Make sure that all are doubles / numeric types
  mutate(start = as.numeric(str_extract(start, "[^, ]*$")),
         party = (party == 'Democratic') * 1) %>%
  # Limit to FDR and later 
  filter(start >= 1932) %>% 
  select(start,party)

# Get end of administration dates from next administration start dates
start_years <- unlist(c(admins['start']))
names(start_years) <- NULL
end_years <- append(start_years[2:length(start_years)],c(2021))

admins['end'] <- as.numeric(end_years)   

# Re-select to order tibble, preference
admins <- admins %>% 
  select(start,end,party)

# It works, but maybe built in solution exists
get_current_admin <- function(year){
  for(row in c(1:nrow(admins))){
    if(between(year, admins$start[row], admins$end[row])){
      return(admins$party[row])
    }
  }
  return(NULL)
}

# Get the administration party for each year 
# Dem = 1, Rep = 0
env_cases['admin'] <- map_dbl(env_cases$year,get_current_admin)

# Drop redundant columns 
env_cases <- env_cases %>% 
  select(-casetype,-category,-casnum,-appres) 

# Write to disk
env_cases %>% 
  write.csv(file = 'data/env_courts.csv',row.names=FALSE)