

librarian::shelf(tidyverse, randomNames)

n_resp = 100

# gen response ids
ids = 1:n_resp

# gen dates

days_after <- sample(0:30, n_resp, replace = TRUE)
dates <- sort(Sys.Date() + days_after)

# gen random genders
gender_options <- c("Female", "Male", "Rather not say")

genders <- sample(gender_options, n_resp, prob=c(0.45, 0.45, 0.1), replace=TRUE)
genders_input <- ifelse(genders == "Rather not say", NA, genders)

# gen random names using genders
names <- randomNames(gender = genders_input, name.order = "first.last") %>% 
  str_remove(",")

# gen random age_ranges
age_options <- c("18-27", "28-37", "38-47", "48-57", "58-67", "68-77", "78+")

age_ranges <- sample(age_options, n_resp, prob = c(.05, .15, .175, .25, .175, .15, .05), replace=TRUE)

# gen random sectors with potential for up to three per respondent
sector_options <- c("Aquaculture", "Commercial fishing", "Energy Development",
                    "Marine Biotechnology", "Recreation, sports, and tourism",
                    "Recreational fishing", "Research", "Security and defense",
                    "Touristic fishing")

sec_probs = c(0.05, 0.2, 0.05, 0.05, 0.25, 0.2, 0.05, 0.05, 0.1)

sectors <- vector("character")

for (i in 1:n_resp) {
  
  n_sec <- sample(c(1, 2, 3), 1, replace = TRUE)
    
  sec <- sample(sector_options, n_sec, prob = sec_probs, replace=FALSE)
  sec <- paste(sec, collapse = ",")
  
  sectors <- c(sectors, sec)
}

# gen random number represented
norm_dist <- rnorm(n_resp, 1, 3) %>%
  abs() %>% 
  round()

n_rep <- ifelse(norm_dist == 0, 1, norm_dist)

# gen facilitated
is_facil_options <- c(TRUE, FALSE)

is_facilitated <- sample(is_facil_options, n_resp, prob = c(0.7, 0.3), replace = TRUE)

# gen facilitators
facil_options <- randomNames(30, name.order = "first.last") %>% 
  str_remove(",")

facilitators = sample(facil_options, n_resp, replace = TRUE)


# combine into dataframe
d <- data.frame(response_id = ids, date = dates, name = names,
                facilitator_name = facilitators, gender = genders,
                age_range = age_ranges, sector = sectors, n_rep = n_rep,
                is_facilitated = is_facilitated) %>% 
  mutate(facilitator_name = ifelse(is_facilitated == FALSE, NA, facilitator_name))


# gen harvest license
harv_options <- c(TRUE, FALSE)

harv_licenses <- sample(harv_options, n_resp, prob = c(0.6, 0.4), replace = TRUE)


# gen vessel id
vessel_options <- stringi::stri_rand_strings(70, 6)

vessel_ids <- sample(vessel_options, n_resp, replace = TRUE)


# add to df and make sure they only apply to comm fish sector
random_data <- d %>% 
  mutate(harv_license = harv_licenses,
         vessel_id = vessel_ids) %>% 
  mutate(harv_license = ifelse(str_detect(sector, "Commercial fishing"), harv_license, FALSE),
         vessel_id = ifelse(str_detect(sector, "Commercial fishing"), vessel_id, NA))

write_rds(random_data, "data/temp/random_data.RDS")


# shapes generated randomly with python script and joined with informal regions in qgis
# shapes <- read_sf("/Users/menzies/shiny/ous-shiny-demo/data/random-cali-shapes.fgb")











