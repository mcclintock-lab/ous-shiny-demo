# takes as input a responses df containing a gender column with JSON key value pairs
# and returns the same df with each gender category added as a column

parse_genders <- function(responses, genders) {
  
  gender_parsed_list <- list()
  
  for (i in seq(1, nrow(responses))) {
    
    parsed <- jsonlite::fromJSON(responses$gender[i])
    gender_parsed_list[[i]] <- parsed
  }
  
  gender_parsed <- data.table::rbindlist(gender_parsed_list, fill = TRUE) |> 
    as.data.frame()
  
  # add columns for any unrepresented genders
  unrep <- setdiff(genders, names(gender_parsed))
  
  for (i in seq_along(unrep)) {
    
    col_name <- unrep[i]
    gender_parsed[col_name] <- 0
  }
  
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




