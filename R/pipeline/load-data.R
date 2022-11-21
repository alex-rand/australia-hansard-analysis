##### This function scrapes the data from Zenodo
##### and combines it into a single large dataset,
##### since the platform doesn't seem to offer a 
##### a 'download all' button

load_data <- function(use_cache = TRUE){
  
  cache_path <- "data/dat-raw.rds"
  
  ### Try to load cached data 
  if(file.exists(cache_path) & use_cache == TRUE) {
    res <- readRDS(cache_path)
    return(res)
  }
  
  ### If no cached data then load directly from Zenodo
  else{
    
    message("No data found in cache path: loading directly from Zenodo. This could take about 20 minutes.")
    
    ### Scrape the webpage 
    zenodo_page <- read_html("https://zenodo.org/record/7336076")
    
    ### Extract all the file names
    res <- zenodo_page |> 
      
      # Get raw HTML 
      rvest::html_text() |> 
      
      # Extract all csv file names
      stringr::str_extract_all("\\s\\d\\d\\d\\d.*.csv\\s") |> 
      
      # Turn it into a table as opposed to a list
      magrittr::extract2(1) |> 
      
      # Give it nice table-y features
      as.data.frame() |> 
      
      dplyr::rename("file_name" = 1) |> 
      
      # Do some cleaning
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.character), stringr::str_trim),
        date = stringr::str_extract(file_name, "\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d"),
        data = vector("list", length = nrow(.))
      )
    
    ### Download all the files
    for(i in 1:nrow(res)){
      
      print(paste0(i, "/", nrow(res)))
      
      res$data[[i]] <- readr::read_csv(paste0("https://zenodo.org/record/7336076/files/", res$file_name[i]))
      
    }
    
    ### Save the big file
    saveRDS(res, "data/dat-raw.rds")
    
    return(res)
    
  }
  
}



