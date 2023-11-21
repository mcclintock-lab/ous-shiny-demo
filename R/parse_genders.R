# takes as input a responses df containing a gender column with JSON key value pairs
# and returns the same df with each gender category added as a column

parse_genders <- function(responses) {
  
  gender_parsed_list <- list()
  
  for (i in seq_along(responses$gender)) {
    
    parsed <- jsonlite::fromJSON(responses$gender[i])
    gender_parsed_list[[i]] <- parsed
  }
  
  gender_parsed <- data.table::rbindlist(gender_parsed_list, fill = TRUE) |> 
    as.data.frame()
  
  names <- sapply(names(gender_parsed), FUN = function(x) paste0("gender_", x)) |> 
    str_to_lower() |> 
    snakecase::to_snake_case()
  names(gender_parsed) <- names
  
  gender_parsed[is.na(gender_parsed)] <- 0
  
  responses_gender_parsed <- responses |> 
    bind_cols(gender_parsed) |> 
    select(-gender)
  
  return(responses_gender_parsed)
}




