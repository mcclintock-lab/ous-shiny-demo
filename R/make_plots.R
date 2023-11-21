# create functions for making sector and demographics plots

# make responses by sector plot ----
make_sector_plot <- function(metric, responses) {
  
  if (metric == "represented") {
    
    df <- responses |> 
      group_by(sector) |> 
      summarize(n = sum(participants)) |> 
      mutate(sector = as.character(sector)) |> 
      mutate(sector = str_wrap(sector, width = 20),
             sector = as.factor(sector)) |>
      left_join(sector_targets)
    
    ggplot(df, aes(x = reorder(sector, n), y = n)) +
      geom_chicklet(fill = "#2E4052", radius = grid::unit(5, "pt")) +
      geom_chicklet(aes(x = reorder(sector, n), y = target), color = "green", fill = "green",
               alpha = 0.05, radius = grid::unit(5, "pt")) +  
                    #width = 0.05, just = 0) +
      coord_flip() +
      labs(x = "Sector\n", y = "\nCount") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0, 0, 5, 2), units = "mm")
      )
    
  } else {
    
    df <- responses |> 
      group_by(sector) |> 
      count() |> 
      mutate(sector = as.character(sector)) |> 
      mutate(sector = str_wrap(sector, width = 20),
             sector = as.factor(sector))
    
    ggplot(df, aes(x = reorder(sector, n), y = n)) +
      geom_chicklet(fill = "#2E4052", radius = grid::unit(5, "pt")) +
      coord_flip() +
      labs(x = "Sector\n", y = "\nCount") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0, 0, 5, 2), units = "mm"),
      )
  }
}


# make demographics plot ----
make_demo_plot <- function(respondent_info) {
  
  age_counts <- respondent_info |> 
    select(all_of(matches("^age_.*"))) |> 
    summarise_all("sum") |> 
    pivot_longer(everything(), names_to = "age_group", values_to = "count") |> 
    mutate(age_group = str_remove(age_group, "age_"))
  
  age_counts <- age_counts[c(2,3,4,1),]
  
  gender_counts <- respondent_info |> 
    select(all_of(matches("^gender_.*"))) |> 
    summarise_all("sum") |> 
    pivot_longer(everything(), names_to = "gender", values_to = "count") |> 
    mutate(gender = str_remove(gender, "gender_"))
  
  ggplot(age_counts, aes(x = age_group, y = count)) + # need df$n or R sees it as function n()
    geom_chicklet(fill = "#2E4052") + 
    labs(x = "\nAge group", y = "Number of respondents\n") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      plot.margin = unit(c(0, 2, 5.5, 5), "mm")
    )
}


