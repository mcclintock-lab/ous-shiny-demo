

targets <- read_csv("data/survey_targets.csv")

# make target table function to be called in server
make_target_table <- function(responses) {
  progress <- responses |> 
    select(sector, participants) |> 
    group_by(sector) |> 
    summarize(participants = sum(participants))
  
  targets_progress <- targets |>
    full_join(progress)
  
  # used for sector plot in `R/make_plots.R`
  assign("sector_targets", targets, envir = .GlobalEnv)
  
  targets_progress <- targets_progress |>
    mutate(
      participants = ifelse(is.na(participants), 0, participants),
      percent = round((participants / target), 2),
      percent = ifelse(percent > 1, 1, percent)
    )
  
  # used for target editing in `server.R`
  assign("targets_progress", targets_progress, envir = .GlobalEnv)
  
  target_table <-
    DT::datatable(
      targets_progress,
      colnames = c(
        "Metric",
        "Target",
        "Participants",
        "Percent achieved"
      ),
      editable = list(target='cell', disable = list(columns = c(1,3,4))),
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
    # percent progress bar
    DT::formatStyle(
      columns = "percent",
      background = DT::styleColorBar(
        c(targets_progress$percent, 1), # tack on a default 1 (i.e. max possible amount) because `styleColorBar` fills proportionally
        "#BCFCC3",
        angle = -90
      ),
      backgroundSize = '100% 15%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'bottom'
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
