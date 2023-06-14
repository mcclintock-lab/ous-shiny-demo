# ous shiny set up

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(here)
library(janitor)
library(DT)
library(knitr)
library(leaflet)
library(sf)
library(shinycssloaders)
library(ggchicklet)
library(snakecase)
library(lubridate)

project <- "demo"

# identifies date of latest data files and removes old ones
# source(here("R/manage_data.R"))

# read in sector id abbreviation keys
sector_ids <- read.csv(here("data/demo_sector_ids.csv")) 

# date project officially launched
launch_date <- "2022-09-06"

# if there is a regional designation that is of interest to group by, define it here
region <- "region"

temp_data_date <- read_rds(here("data/temp/temp_data_date.RDS"))
data_update <- as_datetime(readLines("data/temp/data_update.txt"), tz = "America/Los_Angeles")


if (temp_data_date >= data_update) {

  responses <- read_rds(here("data/temp/responses.RDS"))
  respondent_info <- read_rds(here("data/temp/respondent_info.RDS"))
  shapes <- read_sf(here("data/temp/shapes.fgb"))

} else {
  
  source("data_prep.R")
}

source("R/make_plots.R")
source("R/make_data_explorer.R")
source("R/make_target_table.R")
source("R/make_reporting_tables.R")






