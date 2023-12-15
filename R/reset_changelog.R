# reset change_log for testing purposes

change_log <- read_rds("data/change_log.RDS")

change_log <- change_log[0, ]

write_rds(change_log, "data/change_log.RDS")