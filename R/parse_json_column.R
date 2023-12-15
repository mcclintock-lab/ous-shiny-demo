# takes as input a responses df containing a column with JSON key value pairs as a string
# and returns the same df with each attribute category added as a column

parse_json_column <- function(responses, column, all_column_values) {
  
  column_parsed_list <- list()
  
  for (i in seq(1, nrow(responses))) {
    
    parsed <- jsonlite::fromJSON(responses[[column]][i])
    column_parsed_list[[i]] <- parsed
  }
  
  column_parsed <- data.table::rbindlist(column_parsed_list, fill = TRUE) |> 
    as.data.frame()
  
  # add columns for any unrepresented categories
  unrep <- setdiff(all_column_values, names(column_parsed))
  
  for (i in seq_along(unrep)) {
    
    col_name <- unrep[i]
    column_parsed[col_name] <- 0
  }
  
  # clean names - not using to_snake_case on age preserves "<" and "+" characters
  if (column != "age") {
    
    names <- sapply(names(column_parsed), FUN = function(x) paste0(column, "_", x)) |> 
      str_to_lower() |> 
      snakecase::to_snake_case()
    names(column_parsed) <- names
    
  } else {
    
    names <- sapply(names(column_parsed), FUN = function(x) paste0(column, "_", x)) |> 
      str_to_lower()
    names(column_parsed) <- names
  }
  
  
  
  
  column_parsed[is.na(column_parsed)] <- 0
  
  responses_column_parsed <- responses |> 
    bind_cols(column_parsed) |> 
    select(-!!column)
  
  return(responses_column_parsed)
}




