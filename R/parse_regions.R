# takes as input a responses df containing a region column with JSON key value pairs
# and returns the same df with each region added as a column

parse_regions <- function(responses, region_column, regions) {
  
  region_parsed_list <- list()
  regions_represented <- c()
  
  for (i in seq(1, nrow(responses))) {
    
    parsed <- jsonlite::fromJSON(responses[[i, region_column]])
    region_parsed_list[[i]] <- parsed
    
    represented <- names(parsed)[region_parsed_list[[i]] != 0] |> 
      paste0(collapse = ",")
    regions_represented <- c(regions_represented, represented)
  }
  
  region_parsed <- data.table::rbindlist(region_parsed_list, fill = TRUE) |> 
    as.data.frame()
  
  # add columns for any unrepresented regions
  unrep <- setdiff(regions, names(region_parsed))
  
  for (i in seq_along(unrep)) {
    
    col_name <- unrep[i]
    region_parsed[col_name] <- 0
  }
  
  names <- sapply(names(region_parsed), FUN = function(x) paste0(region, "_", x)) |> 
    str_to_lower() |> 
    snakecase::to_snake_case()
  names(region_parsed) <- names
  
  region_parsed[is.na(region_parsed)] <- 0
  
  responses_region_parsed <- responses |> 
    bind_cols(region_parsed) |> 
    select(-!!region) |> 
    mutate("regions_represented" = regions_represented)
  
  return(responses_region_parsed)
}




