# ous shiny set up

library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(here)
library(lubridate)
library(mapdeck)
devtools::load_all("packages/shinymanager")
devtools::load_all("packages/shinyBS")
# read in mapbox public key
mb_pk <- read_rds("auth/mb_pk.RDS")

# load status of password protection
source("R/secure_option.R")

# load all project-specific variables
source("R/project_variables.R")

# data update in ymd format for exported file names
data_update_ymd <- read_rds("data/temp/data_date.RDS") |> 
  gsub(pattern = " .*", replacement = "")

# load processed data files
responses <- read_rds(here("data/temp/responses.RDS"))
respondent_info <- read_rds(here("data/temp/respondent_info.RDS"))
shapes <- read_rds(here("data/temp/shapes.RDS"))
change_log <- read_rds("data/change_log.RDS")


# load functions
source("R/make_plots.R")
source("R/make_datatable.R")
source("R/make_target_table.R")
source("R/make_reporting_tables.R")
source("R/make_change_log.R")
