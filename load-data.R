library(tidyverse)
library(rvest)

### Scrape the webpage to get all the addresses
pagesource <- read_html("https://zenodo.org/record/7336076")

file_names <- pagesource %>% 
  
  # Get raw HTML 
  html_text() %>% 
  
  # Extract all csv filenames
  stringr::str_extract_all("\\s\\d\\d\\d\\d.*.csv\\s") %>% 
  
  # Turn it into a table as opposed to a list
  magrittr::extract2(1) %>% 
  
  # Give it nice table-y features
  as.data.frame() %>% 
  
  # Do some cleaning
  mutate(across(where(is.character), str_trim))

  
  
file_names %>% 
  
  as.data.frame() 
  
  # Probs a smarter regexp would avoid these steps
  stringr::str_trim() 
  
  stringr::str_replace(1, "\\n", "")
  
  



 read_csv("https://zenodo.org/record/7336076/files/2011-05-10-main.csv")
