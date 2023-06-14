

librarian::shelf(tidyverse, randomNames)

n_resp = 200

# gen response ids
ids = 1:n_resp

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

# gen random sectors
sector_options <- c("Aquaculture", "Commercial fishing", "Energy Development",
                    "Marine Biotechnology", "Recreation, sports, and tourism",
                    "Recreational fishing", "Research", "Security and defense",
                    "Touristic fishing")

sectors <- sample(sector_options, n_resp, replace=TRUE)

# gen random number represented
norm_dist <- rnorm(n_resp, 1, 2) %>%
  abs() %>% 
  round()

n_rep <- ifelse(norm_dist == 0, 1, norm_dist)

# gen facilitated
facil_options <- c(TRUE, FALSE)

is_facilitated <- sample(facil_options, n_resp, prob = c(0.7, 0.3), replace = TRUE)


# combine into dataframe
d <- data.frame(response_id = ids, name = names, gender = genders,
                age_range = age_ranges, sector = sectors, n_rep = n_rep)


# gen harvest license
harv_options <- c(TRUE, FALSE)

harv_licenses <- sample(harv_options, n_resp, prob = c(0.6, 0.4), replace = TRUE)


# gen vessel id
vessel_options <- stringi::stri_rand_strings(70, 6)

vessel_ids <- sample(vessel_options, n_resp, replace = TRUE)


# add to df and make sure they only apply to comm fish sector
d <- d %>% 
  mutate(harv_license = harv_licenses,
         vessel_id = vessel_ids) %>% 
  mutate(harv_license = ifelse(sector == "Commercial fishing", harv_license, FALSE),
         vessel_id = ifelse(sector == "Commercial fishing", vessel_id, NA))








