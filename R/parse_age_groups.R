# takes as input a responses df containing an age column with JSON key value pairs
# and returns the same df with each age group added as a column

parse_age_groups <- function(responses) {
  
  age_parsed_list <- list()
  
  for (i in seq_along(responses$age)) {
    
    parsed <- jsonlite::fromJSON(responses$age[i])
    age_parsed_list[[i]] <- parsed
  }
  
  age_parsed <- data.table::rbindlist(age_parsed_list, fill = TRUE) |> 
    as.data.frame()
  
  names <- sapply(names(age_parsed), FUN = function(x) paste0("age_", x))
  names(age_parsed) <- names
  
  age_parsed[is.na(age_parsed)] <- 0
  
  responses_age_parsed <- responses |> 
    bind_cols(age_parsed) |> 
    select(-age)
  
  return(responses_age_parsed)
}




