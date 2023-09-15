# create additional objects for use in app


# table for DT data explorer
make_datatable <- function(responses) {
  
  # create df with desired columns
  response_table <- respondent_info #|>
  #   select(
  #     date, time, response_id, name, facilitator_name, account_email,
  #     sector, gender, contains("born"), n_rep, opt_in) |> 
  #   mutate(
  #     response_id = as.character(response_id)) |> 
  #   rename("Response ID" = response_id,
  #          "Number of people represented" = n_rep,
  #          "Opt in for review" = opt_in,
  #          "Age" = yrborn)
  # 
  # colnames(response_table) <- gsub("_", " ",
  #                                  str_to_title(colnames(response_table)))
  
  # output datatable object
  table = datatable(response_table,
            filter = list(position = "top"),
            plugins = "accent-neutralise",
            editable = TRUE,
            options = list(
              pageLength = 50,
              scrollX = TRUE,
              scroller = TRUE,
              lengthChange = FALSE))
  
  return(table)
}

# table of exact duplicate responses 
make_dups_table <- function() {
  
  exact_dups <- shapes[duplicated(shapes$geometry) | duplicated(shapes$geometry, fromLast = TRUE), ] |> 
    select(response_id) |> 
    # use area as means of summing shapes for comparison across response ids with multiple sector responses
    mutate(area = as.numeric(st_area(.))) |> 
    as.data.frame() |> 
    group_by(response_id) |> 
    summarize(area = sum(area)) |> 
    group_by(area) |> 
    # create identifier for identical response pairs / groups
    mutate(duplicate_id = cur_group_id()) |> 
    ungroup() |> 
    select(-area)
  
  n_dups <- nrow(exact_dups)
  
  assign("n_dups", n_dups, envir = .GlobalEnv)
  
  datatable(exact_dups,
            colnames = c("Response ID", "Duplicate ID"),
            options = list(pageLength = 50, lengthChange = FALSE, dom = "t"))
  
}

# data corrections table
make_corrections_table <- function(corrections) {
  table <- datatable(
    corrections,
    colnames = c("Response ID", "Correction", "User", "Date", "Fixed"),
    escape = FALSE,
    options = list(
      lengthChange = FALSE,
      dom = "t",
      columnDefs = list(list(
        className = 'dt-center', targets = 1
      ))
    )
  )
  
  return(table)
}






