

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
  
  research <- responses |>
    filter(sector == "Research") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "research_sector"
    )
  
  rec_sports <- responses |>
    filter(sector == "Recreation, sports, and tourism") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "recreation_sports_and_tourism_sector"
    )
  
  aquaculture <- responses |>
    filter(sector == "Aquaculture") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "aquaculture_sector"
    )
  
  mar_bio <- responses |>
    filter(sector == "Marine biotechnology") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "marine_biotechnology_sector"
    )
  
  sec_def <- responses |>
    filter(sector == "Security and defense") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "security_and_defense_sector"
    )
  
  energy_dev <- responses |>
    filter(sector == "Energy development") |>
    summarize(
      individuals = n(),
      represented = sum(n_rep),
      metric = "energy_development_sector"
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
      population,
      research,
      rec_sports,
      aquaculture,
      sec_def,
      energy_dev,
      mar_bio
    )
  )
  
  targets_progress <- targets |>
    full_join(progress)
  
  # save separate progress table to global env for use in sector plot
  sector_targets <- targets_progress |>
    filter(str_detect(metric, "sector")) |>
    mutate(
      # sector names are formatted to join with responses
      sector = case_when(
        metric == "recreation_sports_and_tourism_sector" ~ "Recreation, sports, and tourism",
        metric == "science_tech_and_monitoring_sector" ~ "Science, tech, and monitoring",
        TRUE ~  
          str_replace_all(metric, "_", " ") |> 
          str_remove(" sector") |>
          to_sentence_case()
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
        pageLength = nrow(targets_progress),
        columnDefs = list(list(
          className = 'dt-center', targets = 2:5
        ))
      )
    ) |>
    # percent color ramp formatting
    formatStyle(
      columns = "percent",
      backgroundColor = styleInterval(breaks, colors),
      "border-radius" = "5px"
    ) |>
    # removes row striping
    formatStyle(
      columns = -1:ncol(targets_progress) + 1,
      "box-shadow" = "inset 0 0 0 9999px rgba(0, 0, 0, 0)"
    ) |> 
    formatPercentage(columns = "percent",
                     mark = ".",
                     digits = 0)
  
  return(target_table)
}
