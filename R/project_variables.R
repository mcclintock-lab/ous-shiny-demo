# set project specific variables

project <- "demo"

app_title <- "OUS Shiny Demo"

seasketch_url <- "https://seasketch.org/peter"

sectors <- c(
  "Commercial fishing",
  "Security and defense",
  "Recreational fishing",
  "Marine biotechnology",
  "Research",
  "Energy development",
  "Touristic fishing",
  "Aquaculture",
  "Recreation, sports, and tourism"
)

region <- "region"

region_list <- c(
  "Southern Coast",
  "Central Coast",
  "North Coast",
  "Bay Area"
)

age_groups <- c("<18", "18-30", "30-50", "50+")

genders <- c("Male", "Female", "Non-binary", "Rather not say")

json_columns <- tribble(
  ~column, ~column_values_object,
  "age", "age_groups",
  "gender", "genders"
)

columns_to_remove <- c(
  "account_email",
  "is_logged_in",
  "is_duplicate_ip",
  "survey_id",
  "created_at_utc",
  "updated_at_utc",
  "is_practice"
)

shape_attributes_to_keep <- c(
  "response_id",
  "name",
  "facilitator_name",
  "sector",
  "regions_represented",
  "is_facilitated"
)

# shape-specific attributes you want to keep in the data editing process
shape_specific_attributes <- NULL

