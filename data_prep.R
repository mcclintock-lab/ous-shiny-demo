# prep data for all phases
librarian::shelf(tidyverse, sf, janitor, here)
source("R/project_variables.R")
source("R/parse_json_column.R")
source("R/parse_regions.R")

writeLines("---\n** RUNNING DATA PREP **\n---")

existing_local_responses <- readRDS("data/temp/responses.RDS")
existing_local_respondent_info <- readRDS("data/temp/respondent_info.RDS")
existing_local_ids <- unique(existing_local_respondent_info$response_id)

# create respondent_info df ----

imported_responses_raw <- read_csv("data/responses.csv")

existing_imported_responses <- imported_responses_raw |> 
  filter(id %in% existing_local_ids)

removed_response_ids <- setdiff(existing_local_ids, imported_responses_raw$id)

new_responses <- imported_responses_raw |> 
  filter(!id %in% existing_local_ids)

n_new_responses <- nrow(new_responses)

n_removed_response_ids <- length(removed_response_ids)

# filter out any responses that were removed on seasketch
existing_local_responses <- existing_local_responses |> 
  filter(!response_id %in% removed_response_ids)

existing_local_respondent_info <- existing_local_respondent_info |> 
  filter(!response_id %in% removed_response_ids)

# if new responses exist, process them
if (n_new_responses != 0) {
  
  prep_statement <- paste0("---\n** ADDING ", n_new_responses, " NEW RESPONSES **\n---")
  writeLines(prep_statement)
  
  # create cleaned version of data exported from seasketch
  new_respondent_info <- new_responses |>
    mutate(
      date = as.Date(substr(created_at_utc, 1, 10)),
      time = substr(created_at_utc, 12, 19),
      .after = id
    ) |> 
  select(-all_of(columns_to_remove)) |>
    clean_names() |>
    rename(
      response_id = id,
      sector = what_sectors_do_you_representfor_sectors,
    ) |>
    filter(!str_detect(sector, "Unknown"),
           sector != "") |>
    mutate(gender = ifelse(gender == "", NA, gender),
           phone_number = as.character(phone_number))
  
  # parse json columns
  for (i in seq(1, nrow(json_columns))) {
    
    new_respondent_info <- parse_json_column(
      new_respondent_info,
      json_columns[[i, 1]],
      get(json_columns[[i, 2]])
    )
  }
  
  new_respondent_info <- parse_regions(new_respondent_info, region, region_list)
  
  # create sector new_responses df ---- 
  
  # create df of individual entries pertaining to each sector a new_respondent drew a shape(s) for
  new_responses <- new_respondent_info %>% 
    # regex below finds commas surrounded by non-white space and replaces JUST the comma with "|"
    # needed to differentiate between commas *within* and *between* sector names
    mutate(sector = gsub("(\\S),(\\S)", "\\1|\\2", sector)) %>% 
    separate_rows(sector, sep = "\\|")
  
  # bind new and old responses
  respondent_info <- existing_local_respondent_info |> 
    bind_rows(new_respondent_info)
  
  responses <- existing_local_responses |> 
    bind_rows(new_responses)

# clean shapes ----

# read in shapes
shapes_raw <- read_sf("data/shapes.json")

shapes <- shapes_raw |>
  dplyr::select(-c(area_sq_meters, feature_name, survey_id)) |>
  rename(value = priority)

# join shapes with csv info
shapes <- shapes |>
  right_join(responses) |>
  st_make_valid() |>
  select(
    all_of(shape_attributes_to_keep),
    contains(region)
  ) |>
  st_make_valid()


# clip to eez
eez <- read_sf("data/eez.fgb") |> 
  st_make_valid()

shapes <- st_intersection(shapes, eez)

# write temporary files ----

write_rds(responses, here("data/temp/responses.RDS"))
write_rds(respondent_info, here("data/temp/respondent_info.RDS"))
write_rds(shapes, here("data/temp/shapes.RDS"))

# if no new responses were added but existing responses were removed on seasketch
} else if (n_removed_response_ids != 0) {
  
  prep_statement <- paste0("---\n** REMOVING ", n_removed_response_ids, " OLD RESPONSES **\n---")
  writeLines(prep_statement)
  
  responses <- existing_local_responses
  respondent_info <- existing_local_respondent_info
  
  shapes <- shapes |>
    right_join(responses) |>
    st_make_valid() |>
    select(
      all_of(shape_attributes_to_keep),
      all_of(shape_specific_attributes),
      contains(region)
    )
  
  write_rds(responses, "data/temp/responses.RDS")
  write_rds(respondent_info, "data/temp/respondent_info.RDS")
  write_rds(shapes, "data/temp/shapes.RDS")

} else {
  
  writeLines("---\n** NO CHANGES TO PROCESS **\n---")
  
}

# date that temp files were created
temp_data_date <- Sys.time()
write_rds(temp_data_date, here("data/temp/temp_data_date.RDS"))

# data that data were downloaded
data_date <- file.info("data/responses.csv")$mtime |> 
  format(format = "%Y-%m-%d %H:%M:%S %Z")
write_rds(data_date, here("data/temp/data_date.RDS"))


