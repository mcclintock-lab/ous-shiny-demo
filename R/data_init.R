# for first data prep of a new project - before any existing responses need to be taken into account

librarian::shelf(tidyverse, sf, janitor, here)
source("R/parse_age_groups.R")
source("R/parse_genders.R")

region <- "region"

writeLines("---\n** RUNNING DATA INIT **\n---")

# clean responses ----

responses_file <- "data/responses.csv"
shapes_file <- "data/shapes.json"

responses_raw <- read_csv(responses_file)

## respondent info ----
## create cleaned version of data exported from seasketch - one row per response_id
respondent_info <- responses_raw |>
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
  mutate(gender = ifelse(gender == "", NA, gender),
         phone_number = as.character(phone_number))

respondent_info <- parse_age_groups(respondent_info)
respondent_info <- parse_genders(respondent_info)

## sector responses ----
## create df of individual entries pertaining to each sector a respondent drew a shape(s) for
responses <- respondent_info %>% 
  # regex below finds commas surrounded by non-white space and replaces JUST the comma with "|"
  # needed to differentiate between commas *within* and *between* sector names
  mutate(sector = gsub("(\\S),(\\S)", "\\1|\\2", sector)) %>% 
  separate_rows(sector, sep = "\\|")

# clean shapes ----

## read in shapes
shapes_raw <- read_sf(shapes_file)

## join shapes with csv info
shapes <- shapes_raw |>
  right_join(responses) |>
  st_make_valid() |>
  select(
    response_id,
    name,
    facilitator_name,
    sector,
    !!region,
    value = priority,
    feature_name,
    is_facilitated
  ) |>
  st_make_valid()


## clip to eez
eez <- read_sf("data/eez.fgb") |> 
  st_make_valid()

shapes <- st_intersection(shapes, eez)

# write temporary files ----

write_rds(responses, here("data/temp/responses.RDS"))
write_rds(respondent_info, here("data/temp/respondent_info.RDS"))
write_rds(shapes, here("data/temp/shapes.RDS"))

# date that temp files were created
temp_data_date <- Sys.time()
write_rds(temp_data_date, here("data/temp/temp_data_date.RDS"))

# data that data were downloaded
data_date <- file.info("data/responses.csv")$mtime |> 
  format(format = "%Y-%m-%d %H:%M:%S %Z")
write_rds(data_date, here("data/temp/data_date.RDS"))

