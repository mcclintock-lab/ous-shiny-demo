# ous shiny set up

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinymanager)
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

secure <- T

project <- "demo"

# read in sector id abbreviation keys
sector_ids <- read.csv(here("data/demo_sector_ids.csv"))

# date project officially launched
launch_date <- "2022-09-06"

# datetime temp data files were generated
temp_data_date <- read_rds(here("data/temp/temp_data_date.RDS"))

# datetime data were last downloaded from seasketch
data_update <-
  as_datetime(readLines("data/temp/data_update.txt"), tz = "America/Los_Angeles")

# data update in ymd format for exported file names
data_update_ymd <- gsub(" .*", "", as.character(temp_data_date))

# shape-specific attributes you want to keep in the data editing process
shape_specific_attributes <- NULL

# load latest processed data files
responses <- read_rds(here("data/temp/responses.RDS"))
respondent_info <- read_rds(here("data/temp/respondent_info.RDS"))
shapes <- read_rds(here("data/temp/shapes.RDS"))
change_log <- read_rds("data/change_log.RDS")


# if there is a regional designation that is of interest to group by, define it here
region <- "region"
region_list <- unique(respondent_info[, region])

# load scripts
source("R/make_plots.R")
source("R/make_datatable.R")
source("R/make_target_table.R")
source("R/make_reporting_tables.R")
source("R/make_change_log.R")
