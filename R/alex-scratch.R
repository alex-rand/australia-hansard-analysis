source("R/pipeline/load-data.R")
source("R/pipeline/clean-data.R")
source("R/pipeline/utils.R")

dat_raw <- load_data(use_cache = TRUE)
dat_clean <- clean_data(dat_raw, use_cache = FALSE)

dat_clean %>% 
  
  distinct(name, .keep_all = TRUE) %>% 
  
  view()

  group_by(name) %>% 
  
  transmute(
    name = name,
    jects = sum(interject, na.rm = TRUE)
  ) %>% 
  
  distinct(name, .keep_all = TRUE) %>% 
  
  arrange(name) %>% 
  
  view()


