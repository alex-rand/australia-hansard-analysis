source("R/pipeline/load-data.R")
source("R/pipeline/utils.R")

dat_raw <- load_data()



dat$data <- dat$data %>% map(align_timestamp)
  
dat_clean <- dat %>% 
  
  unnest(data) %>% 
  
  janitor::clean_names() %>% 
    
  filter(!name %in% c(
    "business start",
    "Honourable members",
    "The SPEAKER",
    "stage direction",
    "Opposition members"
  )) %>% 
  
  distinct(name, gender)
  
  head() %>% 
  
  view()
  
  colnames()
