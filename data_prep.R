# prep data for all phases
librarian::shelf(tidyverse, sf, janitor, here)
source("R/parse_age_groups.R")
source("R/parse_genders.R")

writeLines("---\n** RUNNING DATA PREP **\n---")

existing_local_responses <- readRDS("data/temp/responses.RDS")
existing_local_respondent_info <- readRDS("data/temp/respondent_info.RDS")
existing_local_ids <- unique(existing_local_respondent_info$response_id)

# create respondent_info df ----

imported_responses_raw <- read_csv("data/responses.csv")

existing_imported_responses <- imported_responses_raw |> 
  filter(id %in% existing_local_ids)

new_responses <- imported_responses_raw |> 
  filter(!id %in% existing_local_ids)

n_new_responses <- nrow(new_responses)

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
  select(
    -c(
      account_email,
      is_logged_in,
      is_duplicate_ip,
      survey_id,
      created_at_utc,
      updated_at_utc,
      is_practice
    )
  ) |>
    clean_names() |>
    rename(
      response_id = id,
      sector = what_sectors_do_you_representfor_sectors,
    ) |>
    filter(!str_detect(sector, "Unknown"),
           sector != "") |>
    mutate(gender = ifelse(gender == "", NA, gender))
  
  
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
    response_id,
    name,
    facilitator_name,
    sector,
    !!region,
    is_facilitated
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

} else {
  
  writeLines("---\n** NO NEW RESPONSES TO PROCESS **\n---")
  
}

# date that temp files were created
temp_data_date <- Sys.time()
write_rds(temp_data_date, here("data/temp/temp_data_date.RDS"))

# data that data were downloaded
data_date <- file.info("data/responses.csv")$mtime |> 
  format(format = "%Y-%m-%d %H:%M:%S %Z")
write_rds(data_date, here("data/temp/data_date.RDS"))


