

# make response by sector df ----
make_sector_df <- function(metric) {
  
  # metric is selected from a drop down menu in the ui
  if (metric == "responses") {
    
    df <- responses %>% 
      group_by(sector) %>% 
      count() %>% 
      mutate(sector = as.character(sector)) %>% 
      mutate(sector = case_when(sector == "Scientific Research, Technological Development and Environmental Monitoring" ~ "Research, tech and monitoring",
                                TRUE ~ sector),
             sector = str_wrap(sector, width = 20),
             sector = as.factor(sector))
    
    return(df)
    
  } else if (metric == "represented") {
    
    df <- responses %>% 
      group_by(sector) %>% 
      summarize(n = sum(n_rep)) %>% 
      mutate(sector = as.character(sector)) %>% 
      mutate(sector = case_when(sector == "Scientific Research, Technological Development and Environmental Monitoring" ~ "Research, tech and monitoring",
                                TRUE ~ sector),
             sector = str_wrap(sector, width = 20),
             sector = as.factor(sector))
    
    
    return(df)
  }
}



# make responses by sector plot ----
make_sector_plot <- function(island_name = NULL, metric, all = FALSE) {
  
  if (metric == "represented") {
    
    targets_df <- sector_progress
    
    df <- make_sector_df(metric = metric) %>%
      left_join(targets_df)
    
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
    
    df <- make_sector_df(metric = metric)
    
    targets_df <- sector_progress
    
    df <- make_sector_df(metric = metric) %>%
      left_join(targets_df)
    
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
make_demo_plot <- function(df) {
    
    age_resp <- round((1 - nrow(respondent_info %>% filter(is.na(age_range))) / nrow(respondent_info)) * 100)
    age_resp_label <- paste0(age_resp, "% reported age")
    gender_resp <- round((1 - nrow(respondent_info %>% filter(is.na(gender))) / nrow(respondent_info)) * 100)
    gender_resp_label <- paste0(gender_resp, "% reported gender") 
    
    df <- respondent_info %>%
      filter(!is.na(age_range),
             !is.na(gender)) %>% 
      group_by(age_range, gender) %>% 
      count()
    
    age_count <- df %>% 
      group_by(age_range) %>% 
      summarize(n = sum(n))
    
    age_count_max <- max(age_count$n)
    
    ggplot(df, aes(x = age_range, y = df$n, fill = gender)) + # need df$n or R sees it as function n()
      geom_chicklet() + 
      labs(x = "\nAge range", y = "Number of respondents\n", fill = "Gender") +
      scale_fill_manual(values=c("#BDD9BF", "#2E4052", "#FFC857"),
                        labels = c("Female", "Male", "Rather\nnot say")) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, angle = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(0, 2, 5.5, 5), "mm")
      ) +
      annotate("text", x = length(unique(respondent_info$age_range)) - 1,
               y = -0.21 * age_count_max, label = age_resp_label, size = 4, hjust = 0) +
      annotate("text", x = length(unique(respondent_info$age_range)) - 1,
               y = -0.25 * age_count_max, label = gender_resp_label, size = 4, hjust = 0) +
      coord_cartesian(ylim = c(0, age_count_max + round(0.1 * age_count_max + 0.5)),
                      xlim = c(1, length(unique(df$age_range))), clip = "off")
  
}


