library(tidyverse)

source("R/pipeline/load-data.R")
source("R/pipeline/clean-data.R")
source("R/pipeline/utils.R")

dat_raw <- load_data(use_cache = TRUE)
dat_clean <- clean_data(dat_raw, use_cache = FALSE)

dat_clean |> 
  
  distinct(name, .keep_all = TRUE) |> 
  
  ggplot() +
  geom_bar(aes(x = gender))

### Still to do:

#   - bring in incumbency status
#   - see if I can get each person's date of first election
#   - bring in dates of elections so I can create time until election
#   - will probably want to aggregate interjection counts by week?

# Found all that nice data from the Australian Electoral Commission:
https://results.aec.gov.au/
  
dat_clean |> 
  
  distinct(date) |> 
  
  arrange(date) 
  
#  select(date)
  
  select(
    date,
    name, 
    gender, 
    party,
    electorate, 
    fedchamb_flag,
    interject,
    unique_id
  )
  
  colnames()

  group_by(name) |> 
  
  transmute(
    name   = name,
    gender = gender,
    party  = party,
    n_interjections = sum(interject)
  )

