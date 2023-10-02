

make_change_log <- function(df1, df2, current_log, user) {
  
  diff <- df1 != df2
  
  diff_indexes <- which(diff, arr.ind = TRUE) |> 
    as.data.frame()
  
  diff_ids <- diff_indexes$row
  
  edited_fields <- c()
  
  for (i in nrow(diff_indexes):1) {
    
    edited_fields <- c(names(df1)[diff_indexes$col[i]], edited_fields)
  }
  
  orig_values <- c()
  
  for (i in nrow(diff_indexes):1) {
    
    orig_values <- c(df1[[diff_indexes$row[i], diff_indexes$col[i]]], orig_values) |> 
      as.character()
  }
  
  new_values <- c()
  
  for (i in nrow(diff_indexes):1) {
    
    new_values <- c(df2[[diff_indexes$row[i], diff_indexes$col[i]]], new_values) |> 
      as.character()
  }
  
  diff_log <- tibble(
    "datetime" = as.character(now() |> substr(1, 19)),
    "response_id" = as.character(diff_ids),
    "edited_field" = edited_fields,
    "original_value" = orig_values,
    "new_value" = new_values,
    "user" = user
  )
  
  combined_log <- bind_rows(current_log, diff_log)
  
  assign("change_log", combined_log, envir = .GlobalEnv)

}

make_change_log_table <- function(change_log) {
  
    table <- datatable(
      change_log,
      colnames = c("datetime", "response_id", "edited_field", "original_value", "new_value", "user"),
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = "t",
        scrollX = TRUE,
        scrollY = TRUE,
        language = list(emptyTable = "No changes yet"),
        columnDefs = list(list(
          className = 'dt-center', targets = 1
        ))
      )
    )
    
    return(table)
}


