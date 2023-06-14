# prep data for all phases
librarian::shelf(tidyverse, sf, janitor, here)

writeLines("---\n** RUNNING DATA PREP **\n---")

# create respondent_info df ----

# read in csv linking sector to its respective "*_no" pattern
sector_ids <- read.csv(here("data/demo_sector_ids.csv"))


# respondent info is our randomly generated dataframe
respondent_info <- readRDS("data/temp/random_data.RDS")


# create sector responses df ---- 

# create df of individual entries pertaining to each sector a respondent drew a shape(s) for
responses <- respondent_info %>% 
  # regex below finds commas surrounded by non-white space and replaces JUST the comma with "|"
  # needed to differentiate between commas *within* and *between* sector names
  mutate(sector = gsub("(\\S),(\\S)", "\\1|\\2", sector)) %>% 
  separate_rows(sector, sep = "\\|")


# add max_rep to respondent_info
max_rep <- responses %>% 
  group_by(response_id) %>% 
  summarize(max_rep = max(n_rep))

respondent_info <- respondent_info %>% 
  left_join(max_rep)


# read in and join shapes
shapes <- read_sf("/Users/menzies/shiny/ous-shiny-demo/data/random-california-shapes-clipped-regions.fgb")

# cut n shapes to random response df length
n_sec_resp <- nrow(responses)
n_shapes_total <- nrow(shapes)
start_index <- n_shapes_total - n_sec_resp + 1

shapes <- shapes[start_index:n_shapes_total,]

shapes <- shapes %>% 
  bind_cols(responses)

# add regions to respondent_info and responses
id_regions <- shapes %>% 
  as.data.frame() %>% 
  select(response_id, region)

respondent_info <- respondent_info %>% 
  left_join(id_regions)

responses <- responses %>% 
  left_join(id_regions)

# write temporary files
temp_data_date <- Sys.time()

write_rds(temp_data_date, here("data/temp/temp_data_date.RDS"))
write_rds(responses, here("data/temp/responses.RDS"))
write_rds(respondent_info, here("data/temp/respondent_info.RDS"))
write_sf(shapes, here("data/temp/shapes.fgb"), delete_dsn = TRUE)
 

gc()






