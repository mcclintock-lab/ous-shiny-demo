

targets <- read_csv("data/survey_targets.csv")

# make target table function to be called in server
make_target_table <- function(responses) {
  progress <- responses |> 
    select(sector, participants) |> 
    group_by(sector) |> 
    summarize(participants = sum(participants))
  
  targets_progress <- targets |>
    full_join(progress)
  
  assign("sector_targets", targets, envir = .GlobalEnv)
  
  
  targets_progress <- targets_progress |>
    mutate(
      participants = ifelse(is.na(participants), 0, participants),
      percent = round((participants / target), 2),
      percent = ifelse(percent > 1, 1, percent)
    )
  
  assign("targets_progress", targets_progress, envir = .GlobalEnv)
  
  # red-ylw-grn color ramp styling for target progress
  breaks <- seq(0, 1, 0.01)
  colors <-
    colorRampPalette(c("white", "#EAFCEC"))(length(breaks) + 1)
  
  target_table <-
    DT::datatable(
      targets_progress,
      colnames = c(
        "Metric",
        "Target",
        "Participants",
        "Percent achieved"
      ),
      editable = TRUE,
      selection = "none",
      options = list(
        lengthChange = FALSE,
        dom = "t",
        pageLength = nrow(targets_progress),
        columnDefs = list(list(
          className = 'dt-center', targets = 2:ncol(targets_progress)
        ))
      )
    ) |>
    # percent color ramp formatting
    DT::formatStyle(
      columns = "percent",
      backgroundColor = DT::styleInterval(breaks, colors),
      "border-radius" = "5px"
    ) |>
    # removes row striping
    DT::formatStyle(
      columns = -1:ncol(targets_progress) + 1,
      "box-shadow" = "inset 0 0 0 9999px rgba(0, 0, 0, 0)"
    ) |> 
    DT::formatPercentage(columns = "percent",
                     mark = ".",
                     digits = 0)
  
  return(target_table)
}
