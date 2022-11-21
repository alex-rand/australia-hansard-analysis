library(tidyverse)
library(rvest)

### Scrape the webpage 
pagesource <- read_html("https://zenodo.org/record/7336076")

### Extract all the file names
file_names <- pagesource %>% 
  
  # Get raw HTML 
  html_text() %>% 
  
  # Extract all csv file names
  stringr::str_extract_all("\\s\\d\\d\\d\\d.*.csv\\s") %>% 
  
  # Turn it into a table as opposed to a list
  magrittr::extract2(1) %>% 
  
  # Give it nice table-y features
  as.data.frame() %>% 
  
  rename("file_name" = 1) %>% 
  
  # Do some cleaning
  mutate(
    across(where(is.character), str_trim),
    date = str_extract(file_name, "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d"),
    data = vector("list", length = nrow(.))
  )

### Download all the files
for(i in 1:nrow(file_names)){
  
  print(i)
  
  file_names$data[[i]] <- read_csv(paste0("https://zenodo.org/record/7336076/files/", file_names$file_name[i]))
  
}

### Save the big file
saveRDS(file_names, "data/all-tables.rds")

### Looks like there are some column type inconsistencies to be fixed before we can 
### combine all the tables into one big dataset.

# Declare cleaning function (could also have used anonymous function but I forget the syntax)
align_timestamp <- function(dat){
  
  res <- dat %>% mutate(time.stamp = as.character(time.stamp))
  
  res
  
}

### Load in the data...
file_names <- read_csv("data/all-tables.csv")

# Apply the function
file_names$data <- file_names$data %>% map(align_timestamp)

file_names %>% 
  
  unnest(data)




pryr::object_size(file_names)
what
