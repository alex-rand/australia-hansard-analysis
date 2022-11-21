##### This script contains useful functions for the data cleaning

### Looks like there are some column type inconsistencies to be fixed before we can 
### combine all the tables into one big dataset.

align_timestamp <- function(dat){
  
  res <- dat %>% mutate(time.stamp = as.character(time.stamp))
  
  res
  
}

### Remove some unhelpful names
remove_non_person_names <- function(dat){
  
  res <- dat %>% 
    
    filter(!name %in% c(
      "business start",
      "Honourable members",
      "The SPEAKER",
      "stage direction",
      "Opposition members"
    )) 
  
  res
  
}
