# prep data for all phases
librarian::shelf(tidyverse, sf, janitor, here)

writeLines("---\n** RUNNING DATA PREP **\n---")

launch_date <- "2022-09-06"

# create respondent_info df ----

# read in csv linking sector to its respective "*_no" pattern
sector_ids <- read.csv(here("data/brazil_sector_ids.csv"))

# read in csv
responses_raw <- read.csv(paste0("data/responses-", data_date, ".csv")) %>%
  mutate(date = as.Date(substr(created_at_utc,1,10)),
         time = substr(created_at_utc,12,19))


# create df of pertinent info related to each response_id
respondent_info <- responses_raw %>%
  clean_names() %>% 
  rename(response_id = id,
         sector = what_sectors_do_you_representfor_sectors,
         vessel_func = "function",
         p_city = p_state) %>% 
  filter(!str_detect(sector, "Unknown"),
         consent == "true",
         is_practice == "false",
         sector != "") %>% 
  mutate(age_range = cut(.data$yrborn, c(1,13,24,34,44,54,64,74,101),
                         labels=c("under 14", "14-24", "25–34", "35–44",
                                  "45–54", "55–64", "65–74", "75+"))) %>% 
  mutate(gender = ifelse(gender == "", NA, gender))


# create sector responses df ---- 

# create df of individual entries pertaining to each sector a respondent drew a shape(s) for
responses <- respondent_info %>% 
  # regex below finds commas surrounded by non-white space and replaces JUST the comma with "|"
  # needed to differentiate between commas *within* and *between* sector names
  mutate(sector = gsub("(\\S),(\\S)", "\\1|\\2", sector)) %>% 
  separate_rows(sector, sep = "\\|") %>% 
  left_join(sector_ids)

# nasty loop that copies the value from each row's sector-respective "*_no" column into "n_rep" column
n_rep <- vector(mode = "integer", length = nrow(responses))

for (i in 1:nrow(responses)) {
  
  # export id associated with current row
  curr_export_id <- responses[[i, "export_id"]]
  
  # find value in sector-respective "*_no" column
  curr_n_rep <- responses[[i, names(select(responses, matches(paste0("^", curr_export_id))))]]
  
  # assign value to current index in `n_rep` vec unless NA, in which case value assumed 1
  if (!is.na(curr_n_rep)) {
    n_rep[i] <- curr_n_rep
    
  } else {
    n_rep[i] <- 1
  }
}

# add column to df
responses$n_rep <- as.numeric(n_rep)

# clean up columns
responses <- responses %>% 
  select(-matches("_no"), -export_id)


# add max_rep to respondent_info
max_rep <- responses %>% 
  group_by(response_id) %>% 
  summarize(max_rep = max(n_rep))

respondent_info <- respondent_info %>% 
  left_join(max_rep)




# clean shapes ---- 

# read in shapes
shapes_raw <- read_sf(paste0("data/shapes-", data_date, ".json"))

shapes <- shapes_raw %>% 
  dplyr::select(-c(area_sq_meters, feature_name, survey_id)) %>% 
  rename(value = priority)

# join shapes with csv info
shapes <- shapes %>% 
  right_join(responses, by = c("response_id", "sector")) %>% 
  st_make_valid() %>%  # make any invalid shapes valid
  select(response_id, name, facilitator_name, sector, is_facilitated, state)

# function to clip shapes
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

land <- read_sf(here("data/admin_boundaries/brazil_land_2020.gpkg"))

# punch out the land from the shapes 
shapes <- st_erase(shapes, land)


# write temporary files
temp_data_date <- Sys.time()

write_rds(temp_data_date, here("data/temp/temp_data_date.RDS"))
write_rds(responses, here("data/temp/responses.RDS"))
write_rds(respondent_info, here("data/temp/respondent_info.RDS"))
write_sf(shapes, here("data/temp/shapes.fgb"))
 

gc()






