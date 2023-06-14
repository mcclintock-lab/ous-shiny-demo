# identify date of latest data files and remove old ones

library(stringr)

resp_files = list.files(path = "data/", pattern = "^responses.*", full.names = FALSE)
shp_files = list.files(path = "data/", pattern = "^shapes*", full.names = FALSE)
file_dates = gsub("responses-", "", resp_files)
file_dates = as.Date(gsub(".csv", "", file_dates))

data_date <- as.character(max(file_dates))

old_resp <- resp_files[!str_detect(resp_files, data_date)]
old_shps <- shp_files[!str_detect(shp_files, data_date)]

if (length(old_resp != 0)) {
  for (file in old_resp) {
    file.remove(paste0("data/", file)) 
  }
}

if (length(old_shps != 0)) {
  for (file in old_shps) {
    file.remove(paste0("data/", file)) 
  }
}