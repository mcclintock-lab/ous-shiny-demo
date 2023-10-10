# compares existing responses for recent changes in seasketch

compare_existing_responses <- function(existing_imported_responses, existing_local_responses, change_log) {
  
  if (nrow(change_log) != 0 & length(existing_imported_responses) != 0 ) {
    
    last_updated_local <- change_log |> 
      select(response_id, datetime_updated_local = datetime) |> 
      group_by(response_id) |> 
      filter(datetime == max(datetime)) |> 
      ungroup()
    
    last_updated_imported <- existing_imported_responses |> 
      select(response_id = id, datetime_updated_imported = updated_at_utc)
    
    update_comparison <- last_updated_local |> 
      left_join(last_updated_imported)
    
    responses_to_be_updated <- update_comparison |> 
      filter(datetime_updated_imported > datetime_updated_local)
    
    if (nrow(responses_to_be_updated) == 0) {
      
      return(existing_local_responses)
      
    } else {
      
      unchanged <- existing_local_responses |> 
        filter(!id %in% to_be_updated$response_id)
      
      updated_existing_responses <- existing_local_responses |> 
        bind_rows(to_be_updated)
      
      return(updated_existing_responses)
    }
    
  } else {
    
    return(existing_local_responses)
  }
}