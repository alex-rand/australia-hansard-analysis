##### This script contains useful functions for the data cleaning

### Looks like there are some column type inconsistencies to be fixed before we can 
### combine all the tables into one big dataset.

align_timestamp <- function(dat){
  
  res <- dat |> mutate(time.stamp = as.character(time.stamp))
  
  res
  
}

### Remove some unhelpful names. But tbh is this an appropriate missing data strategy? Could try harder.
remove_non_person_names <- function(dat){
  
  res <- dat |> 
    
    dplyr::filter(!name %in% c(
      "business start",
      "honourable members",
      "the speaker",
      "stage direction",
      "opposition members",
      "a government member",
      "an honourable member",
      "an opposition member",
      "the deputy speaker",
      "honourable member and senators",
      "mr prime minister",
      "government members"
    )) 
  
  res
  
}

### There are lots of unhelpful variations on peoples' names. 
### We can make progress on fixing this by standardizing the strings
standardize_names <- function(dat){
  
  res <- dat |> 
    
    mutate(
      name = stringr::str_to_lower(name), # all to lowercase
      name = stringr::str_remove_all(name, "\\(.*\\)"), # aemove anything in parentheses
      name = stringr::str_remove_all(name, "[[:punct:]]"), # remove all punctuation
      name = stringr::str_remove_all(name, "mp.*"), # remove the term 'MP' and anything that follow it
      name = stringr::str_squish(name) # remove weird within-string white space
    )
  
  res

}

### Some people have their middle name excluded sometimes, 
### which leads to duplicates. Similarly, sometimes peoples'
### intros like 'mr' and 'dr' are included, and sometimes not.
### One way to resolve this is to split the name into separate tokens, 
### then do some basic matching. Won't catch everything, but I think
### will catch the majority of issues.
resolve_middle_names <- function(dat){
  
  adjusted_names <- dat |> 
    
    dplyr::distinct(name, .keep_all = TRUE) |> 
    
    tidyr::separate(name, into = c("1", "2", "3"), remove = FALSE) |>  
    
    dplyr::group_by(`1`, `2`) |> 
    
    dplyr::add_tally(name = "dummy_tally") |> 
    
    dplyr::ungroup() |> 
    
    dplyr::mutate(
      
      `3` = dplyr::case_when(
        dummy_tally > 1 ~ NA_character_,
        dummy_tally <=1 ~ `3`
      ),
      
      adjusted_name = paste0(`1`, " ", `2`)
      
    ) |> 
    
    dplyr::select(name, adjusted_name)
  
  res <- dat |> 
    
    dplyr::left_join(adjusted_names) |> 
    
    dplyr::mutate(name = adjusted_name) |> 
    
    dplyr::select(-adjusted_name)
  
  res
    
}

### Same idea as above, but for titles like 'dr' and 'mr' when the name is of format mr. alex rand
resolve_formal_titles <- function(dat){
  
  adjusted_names <- dat |> 
    
    dplyr::distinct(name, .keep_all = TRUE) |> 
    
    tidyr::separate(name, into = c("1", "2", "3"), remove = FALSE) |>  
    
    dplyr::mutate(adjusted_name = dplyr::case_when(
      is.na(`3`)  ~ name,
      !is.na(`3`) ~ paste(`3`, `2`)
    )) |> 
    
    dplyr::select(name, adjusted_name)
  
  res <- dat |> 
    
    dplyr::left_join(adjusted_names) |> 
    
    dplyr::mutate(name = adjusted_name) |> 
    
    dplyr::select(-adjusted_name)
  
  res
  
}

### Same idea as above, but when only the abbreviated name is given, like mr. rand
### The idea here is to only match when it is unambiguous, IE that family name
### is only associated with one person. 
### Wrote this one while tired, probably not very efficient
resolve_formal_titles_abbreviated <- function(dat){
  
  adjusted_names <- dat |> 
    
    dplyr::distinct(name, .keep_all = TRUE) |> 
    
    tidyr::separate(name, into = c("1", "2", "3"), remove = FALSE) |>  
    
   dplyr:: mutate(
      
      last_name     = ifelse(is.na(`3`) & nchar(`1`) <= 3, `2`, `1`),
      first_name    = ifelse(is.na(`3`) & nchar(`1`) <= 3, NA_character_, `2`)
      
    ) |> 
    
    dplyr::group_by(last_name) |> 
    
    # how many first names are associated with this last name?
    dplyr::mutate(n_this_name = n_distinct(first_name, na.rm = TRUE)) |>  
    
    dplyr::filter(n_this_name == 1) |> 
    
    # construct the adjusted names
    dplyr::mutate(
      first_name    = ifelse(nchar(`1`) <= 3, NA_character_, `2`),
      last_name     = ifelse(nchar(`1`) <= 3, `2`, `1`)
    ) |> 
    
    tidyr::fill(first_name, .direction = "downup") |> 
    
    dplyr::ungroup() |> 
    
    dplyr::mutate(adjusted_name = paste0(last_name, " ", first_name)) |> 
    
    dplyr::select(name, adjusted_name)

  res <- dat |> 
    
    dplyr::left_join(adjusted_names) |> 
    
    dplyr::mutate(name = ifelse(is.na(adjusted_name), name, adjusted_name)) |> 
    
    dplyr::select(-adjusted_name)
  
  res
  
}

### Now that the names are fixed, we can fill in all the info 
### that was missing in the rows with weird versions of names.
### The alternative would be to run these functions before
### interfacing with the AustralianPoliticians package.
### Lindsay probably has those scripts.
fill_info_by_member <- function(dat){
  
  res <- dat |> 
    
    dplyr::group_by(name) |> 
    
    tidyr::fill(name_id,    .direction = "downup") |> 
    tidyr::fill(electorate, .direction = "downup") |> 
    tidyr::fill(party,      .direction = "downup") |> 
    tidyr::fill(unique_id,  .direction = "downup") |>
    tidyr::fill(gender,     .direction = "downup")
  
  res

}

