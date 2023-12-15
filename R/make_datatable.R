# create additional objects for use in app


# table for DT data explorer
make_datatable <- function(responses, edit_data_status) {
  # create df with desired columns
  response_table <- responses 
  
  
  if (edit_data_status == FALSE) {

    table = DT::datatable(
      response_table,
      filter = list(position = "top"),
      plugins = "accent-neutralise",
      editable = FALSE,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        lengthChange = FALSE
      )
    ) 
    
  } else {
    
    table = DT::datatable(
      response_table,
      filter = list(position = "top"),
      plugins = "accent-neutralise",
      editable = list(target='cell', disable = list(columns = c(2,3,5))),
      selection = "none",
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        scroller = TRUE,
        lengthChange = FALSE
      )
    ) 
  }
  
  return(table)
}

# table of exact duplicate responses
make_dups_table <- function() {
  exact_dups <-
    shapes[duplicated(shapes$geometry) |
             duplicated(shapes$geometry, fromLast = TRUE),] |>
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
  
  DT::datatable(
    exact_dups,
    colnames = c("Response ID", "Duplicate ID"),
    class = list(stripe = FALSE),
    options = list(
      pageLength = 50,
      lengthChange = FALSE,
      dom = "t",
    )
  )
  
}

# data corrections table
make_corrections_table <- function(corrections, edit_data_status) {
  
  if (edit_data_status == FALSE) {
    
    table <- DT::datatable(
      corrections,
      colnames = c("Response ID", "Correction", "Reason", "User", "Date", "Fixed"),
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = "t",
        scrollX = TRUE,
        scrollY = TRUE,
        language = list(emptyTable = "No corrections submitted"),
        columnDefs = list(list(
          className = 'dt-center', targets = 1
        ))
      )
    ) |>
      DT::formatStyle(
        columns = 0:ncol(corrections),
        valueColumns = "fixed",
        backgroundColor = styleEqual("✅", "#dbf5d7")
      )
    
    return(table)
    
  } else {
    
    table <- DT::datatable(
      corrections,
      colnames = c("Response ID", "Correction", "Reason", "User", "Date", "Fixed"),
      escape = FALSE,
      selection = "none",
      editable = list(target='cell', disable = list(columns = c(4,5,6))),
      options = list(
        pageLength = 10,
        dom = "t",
        scrollX = TRUE,
        scrollY = TRUE,
        language = list(emptyTable = "No corrections submitted"),
        columnDefs = list(list(
          className = 'dt-center', targets = 1
        ))
      )
    ) |>
      DT::formatStyle(
        columns = 0:ncol(corrections),
        valueColumns = "fixed",
        backgroundColor = styleEqual("✅", "#dbf5d7")
      )
    
    return(table) 
  }
}
