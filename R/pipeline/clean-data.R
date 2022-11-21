##### This function takes the large dataset returned by 
##### load_data() and does some helpful cleaning to prepare
##### the data for analysis. 

clean_data <- function(dat, use_cache = TRUE){
  
  cache_path <- "data/dat-clean.rds"
  
  ### Try to load cached data 
  if(file.exists(cache_path) & use_cache == TRUE) {
    res <- readRDS(cache_path)
    return(res)
  }
  
  ### Otherwise, proceed with cleaning
  
  ### Looks like there are some column type inconsistencies to be fixed before we can 
  ### combine all the tables into one big dataset.
  dat$data <- dat$data |> purrr::map(align_timestamp)
  
  ### Now unnest and clean
  res <- dat |> 
    
    tidyr::unnest(data) |> 
    
    janitor::clean_names() |> 
    
    standardize_names() |> 
    
    remove_non_person_names() |>
    
    resolve_middle_names() |> 
    
    resolve_formal_titles() |>
      
    resolve_formal_titles_abbreviated() |>
    
    fill_info_by_member()
   
  return(res)
  
}
