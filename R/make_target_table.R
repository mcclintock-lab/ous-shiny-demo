
targets <- read_csv("data/demo_survey_targets.csv")

# make target table function to be called in server
make_target_table <- function(responses) {
  
  rec_fishing <- responses |>
    filter(sector == "Recreational fishing") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "recreational_fishing_sector"
    )
  
  comm_fishing <- responses |>
    filter(sector == "Commercial fishing") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "commercial_fishing_sector"
    )
  
  tour_fishing <- responses |>
    filter(sector == "Touristic fishing") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "touristic_fishing_sector"
    )
  
  uw_cultural <- responses |>
    filter(sector == "Underwater cultural heritage") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "underwater_cultural_heritage_sector"
    )
  
  rec_sports <- responses |>
    filter(sector == "Recreation, sports and tourism") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "recreation_sports_and_tourism_sector"
    )
  
  sci_tech <- responses |>
    filter(
      sector == "Scientific Research, Technological Development and Environmental Monitoring"
    ) |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "science_tech_and_monitoring_sector"
    )
  
  aquaculture <- responses |>
    filter(sector == "Aquaculture") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "aquaculture_sector"
    )
  
  sec_def <- responses |>
    filter(sector == "Security and defense") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "security_and_defense_sector"
    )
  
  vessels <- responses |>
    summarize(
      individuals = length(unique(vessel_id)),
      represented = NA,
      metric = "vessels"
    )
  
  
  population <- responses |>
    select(response_id, n_rep) |>
    group_by(response_id) |>
    mutate(n_rep = max(n_rep)) |>
    distinct() |>
    ungroup() |>
    summarize(
      individuals = n(),
      represented = sum(n_rep, na.rm = TRUE),
      metric = "population"
    )
  
  
  progress <- bind_rows(
    list(
      rec_fishing,
      comm_fishing,
      tour_fishing,
      vessels,
      population,
      uw_cultural,
      rec_sports,
      sci_tech,
      aquaculture,
      sec_def
    )
  )
  
  targets_progress <- targets |>
    full_join(progress)
  
  # save separate progress table to global env for use in sector plot
  sector_targets <- targets_progress |>
    filter(str_detect(metric, "sector")) |>
    mutate(
      sector = case_when(
        metric == "commercial_fishing_sector" ~ "Commercial fishing",
        metric == "touristic_fishing_sector" ~ "Touristic fishing",
        metric == "recreational_fishing_sector" ~ "Recreational fishing",
        metric == "rec_sports_sector" ~ "Recreation, sports, and tourism",
        metric == "sci_tech_sector" ~ "Science, tech, and monitoring"
      )
    ) |>
    select(sector, target)
  
  assign("sector_targets", sector_targets, envir = .GlobalEnv)
  
  
  targets_progress <- targets_progress |>
    mutate(
      percent = ifelse(
        str_detect(metric, "sector|population"),
        round((represented / target), 2),
        round((individuals / target), 2)
      ),
      percent = ifelse(percent > 1, 1, percent),
      metric = str_replace_all(metric, "_", " "),
      metric = str_to_title(metric),
      metric = str_replace(metric, " And ", " and ")
    )
  
  # make sure all island/metric combos are represented
  targets_progress <- complete(targets_progress, metric) |>
    arrange("metric") |>
    # replace NAs with 0s
    mutate(
      individuals = ifelse(is.na(individuals), 0, individuals),
      represented = ifelse(is.na(represented) &
                             metric != "Vessels", 0, represented)
    )
  
  assign("targets_progress", targets_progress, envir = .GlobalEnv)
  
  # red-ylw-grn color ramp styling for target progress
  breaks <- seq(0, 1, 0.01)
  colors <-
    colorRampPalette(c("#FFD1C2", "#FCFACE", "#EAFCEC"))(length(breaks) + 1)
  
  target_table <-
    datatable(
      targets_progress,
      colnames = c(
        "Metric",
        "Target",
        "Individual respondents",
        "Individuals represented",
        "Percent achieved"
      ),
      editable = TRUE,
      selection = "none",
      options = list(
        lengthChange = FALSE,
        dom = "t",
        pageLength = nrow(df),
        columnDefs = list(list(
          className = 'dt-center', targets = 2:5
        ))
      )
    ) |>
    # highlight column of metric (rep or indiv) that is associated with "percent achieved"
    formatStyle(
      columns = "individuals",
      valueColumns = "metric",
      backgroundColor = styleEqual(unique(targets_progress$metric[!str_detect(targets_progress$metric, "Sector|Population")]),
                                   "#eefafa"),
      border = styleEqual(unique(targets_progress$metric[!str_detect(targets_progress$metric, "Sector|Population")]),
                          '0.1px solid #b8b8b8'),
      "border-radius" = styleEqual(unique(targets_progress$metric[!str_detect(targets_progress$metric, "Sector|Population")]),
                                   '5px'),
      fontWeight = styleEqual(unique(targets_progress$metric[!str_detect(targets_progress$metric, "Sector|Population")]),
                              "bold")
    ) |>
    formatStyle(
      columns = "represented",
      valueColumns = "metric",
      backgroundColor = styleEqual(unique(targets_progress$metric[str_detect(targets_progress$metric, "Sector|Population")]),
                                   "#eefafa"),
      border = styleEqual(unique(targets_progress$metric[str_detect(targets_progress$metric, "Sector|Population")]),
                          '0.1px solid #b8b8b8'),
      "border-radius" = styleEqual(unique(targets_progress$metric[str_detect(targets_progress$metric, "Sector|Population")]),
                                   '5px'),
      fontWeight = styleEqual(unique(targets_progress$metric[str_detect(targets_progress$metric, "Sector|Population")]),
                              "bold")
    ) |>
    # percent color ramp formatting
    formatStyle(
      columns = "percent",
      backgroundColor = styleInterval(breaks, colors),
      "border-radius" = "5px"
    ) |>
    formatPercentage(columns = "percent",
                     mark = ".",
                     digits = 0)
  
  return(target_table)
}
