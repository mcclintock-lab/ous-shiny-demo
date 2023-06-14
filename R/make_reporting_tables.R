# make a table with figures used in final OUS reports

# project-specific variables
region <- "region"
region_label <- "region"

# totals ----

make_reporting_totals_table <- function() {
  
  # individual respondents and individuals represented
  n_resp_rep <- respondent_info %>%
    group_by(region) %>% 
    summarize("individual_respondents" = n(),
              "individuals_represented" = sum(max_rep))
  
  # sector responses
  n_sec_resp <- responses %>% 
    group_by(region) %>% 
    count(name = "sector_responses")
  
  # individuals responded in multiple sectors
  resp_mult_sec <- responses %>% 
    group_by(region, response_id) %>% 
    count() %>% 
    mutate(multi = ifelse(n > 1, TRUE, FALSE))
  
  names(resp_mult_sec) <- c(region, "response_id", "n", "multi")
  
  resp_mult_sec <- resp_mult_sec %>%   
    group_by(region) %>% 
    summarize(multi_sector_responses = sum(multi))
  
  # shapes
  n_shapes <- shapes %>% 
    as.data.frame() %>% 
    group_by(region) %>% 
    count(name = "shapes_drawn")
  
  # join
  reporting_totals <- n_resp_rep %>% 
    left_join(n_sec_resp) %>% 
    left_join(resp_mult_sec) %>% 
    left_join(n_shapes) %>% 
    filter(.data[["region"]] != "") %>% 
    rename("Individual respondents" = "individual_respondents",
           "Individuals represented" = "individuals_represented",
           "Sector responses" = "sector_responses",
           "Multi-sector responses" = "multi_sector_responses",
           "Shapes drawn" = "shapes_drawn") %>% 
    pivot_longer(cols = all_of(2:6), names_to = "Metric", values_to = "value") %>% 
    pivot_wider(names_from = all_of(1), values_from = value) %>% 
    mutate(Metric = factor(Metric, levels = c("Individual respondents", "Individuals represented", "Sector responses", "Shapes drawn", "Multi-sector responses")))
  
  datatable(reporting_totals)
  
  assign("reporting_totals", reporting_totals, envir = .GlobalEnv)
  
}

# by sector ----

make_reporting_sector_table <- function() {
  
  # sector responses and individuals represented
  n_resp_rep <- responses %>%
    group_by(region, sector) %>% 
    summarize("individuals_represented" = sum(n_rep),
              "sector_responses" = n(),
              "percent_total_sector_responses" = 
                round(sector_responses * 100 / nrow(responses), 2))
  
  # shapes
  n_shapes <- shapes %>% 
    as.data.frame() %>% 
    group_by(region, sector) %>% 
    count(name = "shapes_drawn")
  
  reporting_by_sector <- n_resp_rep %>% 
    left_join(n_shapes) %>% 
    ungroup() %>% 
    complete(.data[["region"]], sector) %>%
    filter(.data[["region"]] != "") %>% 
    mutate(
      "Individuals represented" = ifelse(is.na(individuals_represented), 0, individuals_represented),
      "Sector responses" = ifelse(is.na(sector_responses), 0, sector_responses),
      "Percent total sector responses" = ifelse(is.na(percent_total_sector_responses), 0, percent_total_sector_responses),
      "Shapes drawn" = ifelse(is.na(shapes_drawn), 0, shapes_drawn),
    ) %>%
    select(-c("individuals_represented", "sector_responses", "percent_total_sector_responses", "shapes_drawn")) %>% 
    pivot_longer(cols = all_of(3:6), names_to = "Metric", values_to = "value") %>% 
    pivot_wider(names_from = all_of(1), values_from = value) %>% 
    mutate(Metric = factor(Metric, levels = c("Individuals represented", "Sector responses", "Percent total sector responses", "Shapes drawn"))) %>% 
    rename("Sector" = "sector") 
  
  datatable(reporting_by_sector)
  
  assign("reporting_by_sector", reporting_by_sector, envir = .GlobalEnv)
  
}






