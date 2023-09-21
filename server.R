# Define server logic

shinyServer(function(input, output, session) {
  # USER AUTH --------------------------------
  
  # read in db passphrase
  passphrase_file <- "auth/passphrase.txt"
  passphrase <- readChar(passphrase_file, file.info(passphrase_file)$size)
  
  # check_credentials() returns a function that takes username and password and returns list of user attributes
  res_auth <-
    secure_server(check_credentials = check_credentials("auth/users.sqlite",
                                                        passphrase = passphrase))
  
  # Create reactive admin status to determine privileges
  admin_status <- reactive({
    reactiveValuesToList(res_auth)$admin
  })
  
  # write privileges if not admin
  write_status <- reactive({
    reactiveValuesToList(res_auth)$write
  })
  
  # current user
  current_user <- reactive({
    reactiveValuesToList(res_auth)$user
  })
  
  # FILE READING ---------------------------------------
  
  # responses
  responses_reader <- reactiveFileReader(
    intervalMillis = 1.8e6,
    # check for updates every 30 min
    session = session,
    filePath = "data/temp/responses.RDS",
    readFunc = readRDS
  )
  
  responses <- reactive(responses_reader())
  
  # respondent_info
  respondent_info_reader <- reactiveFileReader(
    intervalMillis = 1.8e6,
    session = session,
    filePath = "data/temp/respondent_info.RDS",
    readFunc = readRDS
  )
  
  respondent_info <- reactive(respondent_info_reader())
  
  # shapes
  shapes_reader <- reactiveFileReader(
    intervalMillis = 1.8e6,
    session = session,
    filePath = "data/temp/shapes.RDS",
    readFunc = readRDS
  )
  
  shapes <- reactive(shapes_reader())
  
  # data update datetime
  data_update_reader <- reactiveFileReader(
    intervalMillis = 1.8e3,
    session = session,
    filePath = "data/temp/data_update.txt",
    readFunc = readLines
  )
  
  data_update <- reactive(data_update_reader() |>
                            as_datetime(tz = "America/Los_Angeles"))
  
  # REFRESH APP ----------------------------------------------
  observeEvent(input$refresh, {
    shinyjs::js$refresh_page()
  })
  
  # OVERVIEW ------------------------------------------------
  
  ## info boxes ----
  
  # latest data update
  output$latest_data <- renderUI({
    HTML(paste0("Data updated", br(), "<b>", data_update(), "</b>"))
  })
  
  # number respondents box
  output$num_respondents <- renderValueBox({
    valueBox(
      nrow(respondent_info()),
      HTML(paste0("Individual", br(), "Respondents")),
      icon = icon("user", class = "fa-solid fa-user")
    )
  })
  
  # number represented box
  output$num_rep <- renderValueBox({
    valueBox(
      sum(respondent_info()$max_rep),
      HTML(paste0("Individuals", br(), "Represented")),
      icon = icon("users")
    )
  })
  
  # sector responses box
  output$sec_resp <- renderValueBox({
    valueBox(nrow(responses()),
             HTML(paste0("Sector", br(), "Responses")),
             icon = icon("water")
    )
  })
  
  
  ## targets ------------------------------------------------
  
  ### target table ----
  output$target_table <- renderDataTable({
    make_target_table(responses = responses())
  })
  
  ### target editing ----
  observe({
    # admin only - allow writing target changes to local csv
    if (!is.null(write_status()) && write_status() == TRUE) {
      
      # make target_table_reactive object reactive so the save_targets event can access the current changes
      target_table_reactive <- reactiveVal()
      target_table_reactive(targets)
      
      # listen for user edits to table and update target_table_reactive accordingly
      observeEvent(input$target_table_cell_edit, {
        changed_row <- input$target_table_cell_edit$row
        changed_val <- input$target_table_cell_edit$value
        changed_val <- ifelse(changed_val == "", NA, changed_val)
        changed_target <- targets_progress[[changed_row, "metric"]] |>
          snakecase::to_snake_case()
        
        changed_target_table <- target_table_reactive()
        
        changed_target_table[changed_target_table$metric == changed_target, ]$target <-
          changed_val
        
        target_table_reactive(changed_target_table)
        
        # save table state to global object
        assign("changed_target_table", changed_target_table, envir = .GlobalEnv)
        
      })
      
      # save_targets listener - overwrite source targets
      observeEvent(input$save_targets, {
        write_csv(changed_target_table, "data/demo_survey_targets.csv")
        
        assign("targets",
               changed_target_table,
               envir = .GlobalEnv)
        
        # rerender table on save
        output$target_table <- renderDataTable({
          make_target_table(responses = responses())
        })
        
        # rerender sector plot because it has target bars
        output$resp_plot <- renderPlot({
          make_sector_plot(metric = resp_plot_metric(),
                           responses = responses())
          
        })
        
      })
    }
  })
  
  
  ## demographic plot ----
  
  output$demo_plot <- renderPlot({
    make_demo_plot(respondent_info = respondent_info())
    
  })
  
  ## sector plot ------------------------------------
  
  # initially define metric that can be changed with the box dropdown menu
  resp_plot_metric <- reactiveVal()
  resp_plot_metric("represented")
  
  output$resp_plot <- renderPlot({
    make_sector_plot(metric = resp_plot_metric(),
                     responses = responses())
    
  })
  
  # observe dropdown input
  observeEvent(input$represented, {
    if (resp_plot_metric() != "represented") {
      resp_plot_metric("represented")
    }
  })
  
  observeEvent(input$responses, {
    if (resp_plot_metric() != "responses") {
      resp_plot_metric("responses")
    }
  })
  
  
  # DATATABLE ----------------------------------------------
  
  ## main table ----
  output$datatable <-
    renderDataTable(expr = make_datatable(responses = responses()),
                    server = FALSE)
  
  ### view in map ----
  observeEvent(input$dt_view_shapes, {
    if (is.null(input$datatable_rows_selected)) {
      showNotification("Please select a response in the table to view on map",
                       type = "error")
      
    } else {
      updateTabItems(inputId = "tabs", selected = "shapes")
      
      selected_row <- input$datatable_rows_selected
      selected_id <- unique(responses()$response_id[selected_row])
      
      updateTextInput(inputId = "shape_id", value = selected_id)
      
      updateSwitchInput(inputId = "filter_id", value = TRUE)
      
    }
  })
  
  ### data editing ----
  observe({
    # admin only - allow writing target changes to local csv
    if (!is.null(write_status()) && write_status() == TRUE) {
      
      responses_reactive <- reactiveVal()
      responses_reactive(responses())
      
      # listen for user edits to table and update responses accordingly
      observeEvent(input$datatable_cell_edit, {
        
        changed_row <- input$datatable_cell_edit$row
        changed_col <- input$datatable_cell_edit$col
        changed_val <- input$datatable_cell_edit$value
        changed_val <- ifelse(changed_val == "", NA, changed_val)
        
        # make changes to reactive object with static intermediary
        changed_responses <- responses_reactive()
        changed_responses[changed_row, changed_col] <- changed_val
        responses_reactive(changed_responses)
        
        # save table state to global object so changes can be written
        assign("changed_responses", changed_responses, envir = .GlobalEnv)
        
      })
      
      # save_targets listener - overwrite source targets
      observeEvent(input$save_datatable_edits, {
        
        # update change log
        make_change_log(
          responses(),
          responses_reactive(),
          change_log,
          current_user()
        )
        
        write_rds(changed_responses, "data/temp/responses.RDS")
        
        # rerender table on save
        output$datatable <- renderDataTable({
          make_datatable(responses = changed_responses)
        })
        
        output$change_log_table <- renderDataTable({
          datatable(change_log)
        })
      })
    }
  })
  
  ## corrections ----
  
  corrections_data <- read_rds("data/corrections.RDS") |> 
    arrange(by = desc(fixed))
  
  output$corrections_table <-
    renderDataTable(make_corrections_table(corrections_data))
  
  corrections <- reactiveVal()
  corrections(corrections_data)
  
  corrections_proxy <- dataTableProxy("corrections_table")
  
  # submit new correction
  observeEvent(input$submit_correction, {
    
    if (is.na(input$corrections_response_id) | input$corrections_text == "") {
      showNotification("Please enter a valid response ID and comment",
                       type = "error")
    } else {
      new_entry <- data.frame(
        response_id = input$corrections_response_id,
        correction = input$corrections_text,
        user = ifelse(class(current_user()) == "character", current_user(), NA),
        date = now(),
        fixed = "⬜️"
      )
      
      # make sure response_id exists in data - show warning if not
      if (new_entry[["response_id"]] %in% responses()$response_id) {
        new_corrections <- bind_rows(list(corrections(), new_entry))
        
        corrections(new_corrections)
        
        replaceData(corrections_proxy, new_corrections)
        
        write_rds(corrections(), "data/corrections.RDS")
        
        updateNumericInput(inputId = "corrections_response_id",
                           value = numeric(0))
        updateTextAreaInput(inputId = "corrections_text",
                            value = character(0))
      } else {
        showNotification("The submitted response ID doesn't exist in the dataset",
                         type = "error")
      }
    }
    
  })
  
  # mark entry as fixed
  observeEvent(input$mark_fixed, {
    selected_rows <- input$corrections_table_rows_selected
    
    new_corrections <- corrections()
    
    for (i in seq_along(selected_rows)) {
      if (new_corrections$fixed[selected_rows[i]] == "⬜️") {
        new_corrections$fixed[selected_rows[i]] <- "✅"
      } else {
        new_corrections$fixed[selected_rows[i]] <- "⬜️"
      }
    }
    
    corrections(new_corrections)
    
    replaceData(corrections_proxy, new_corrections)
    
    write_rds(corrections(), "data/corrections.RDS")
  })
  
  ## changelog ----
  output$change_log_table <- renderDataTable({
    datatable(change_log)
  })
  
  ## duplicates ----
  output$n_dups <- renderText(nrow("n_dups"))
  outputOptions(output, "n_dups", suspendWhenHidden = FALSE)
  
  output$dup_table <- renderDataTable(make_dups_table())
  
  
  # SHAPE VIEWER -------------------------------------------------
  
  output$map <- renderLeaflet({
    shapes <- shapes()
    
    if (input$filter_id == TRUE) {
      # parse user input for filter by response_id
      shape_id <-
        as.numeric(strsplit(input$shape_id, split = ", |,")[[1]])
      
      shapes <- shapes |>
        filter(response_id %in% shape_id)
      
    } else {
      if ("all" %in% input$map_regions) {
        map_regions <- unique(shapes$region)
        
      } else {
        map_regions <- input$map_regions
        
      }
      
      if (input$map_facil_var == "both") {
        map_facil <- c(TRUE, FALSE)
        
      } else {
        map_facil <- input$map_facil_var
      }
      
      if ("All" %in% input$map_sector) {
        shapes <- shapes |>
          filter(region %in% map_regions,
                 is_facilitated %in% map_facil)
        
      } else {
        shapes <- shapes |>
          filter(
            region %in% map_regions,
            sector %in% input$map_sector,
            is_facilitated %in% map_facil
          )
      }
    }
    
    # save filtered shapes as reactive expression to global env for shape export
    assign("filtered_shapes", reactive(shapes), env = .GlobalEnv)
    
    # number of shapes displayed - added to map with `addControl()`
    shapes_displayed <- HTML({
      paste0("Shapes displayed: ",
             div(id = "shapes-number", nrow(shapes)))
    })
    
    # render map
    leaflet(shapes) |>
      addProviderTiles("Esri.WorldStreetMap") |>
      # addTiles("https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png",
      # options = providerTileOptions(apikey = "")) |>
      addPolygons(
        stroke = TRUE,
        weight = 0.02,
        color = "black",
        fillOpacity = 0.1,
        fillColor = "red",
        highlight = highlightOptions(
          color = 'yellow',
          weight = 5,
          bringToFront = FALSE,
          sendToBack = TRUE,
          stroke = 2,
          opacity = 1
        ),
        popup = paste0(
          "<b>",
          shapes$sector,
          "</b>",
          "<br><b>Response ID:</b> ",
          shapes$response_id,
          "<br><b>Name:</b> ",
          shapes$name,
          "<br><b>Facilitator:</b> ",
          shapes$facilitator_name
        )
      ) |>
      addPolylines(color = "black", weight = 0.5) |>
      addControl(shapes_displayed, position = "topright")
  })
  
  
  # filter by id
  output$filter_id_text <- renderText({
    "Filter by Response ID:"
  })
  
  # clear filter button
  observeEvent(input$clear_shape_filters, {
    updatePickerInput(session = getDefaultReactiveDomain(),
                      inputId = "map_regions",
                      selected = character(0))
    
    updateMaterialSwitch(session = getDefaultReactiveDomain(),
                         inputId = "map_regions_all",
                         value = FALSE)
    
    updatePickerInput(session = getDefaultReactiveDomain(),
                      inputId = "map_sector",
                      selected = character(0))
    
    updateMaterialSwitch(session = getDefaultReactiveDomain(),
                         inputId = "map_sector_all",
                         value = FALSE)
    
    updateSelectInput(inputId = "map_facil_var", selected = "both")
    
    updateSwitchInput(inputId = "filter_id", value = FALSE)
    
    updateTextInput(inputId = "shape_id", value = character(0))
    
  })
  
  # export shapes handler
  output$download_filtered_shapes <- downloadHandler(
    filename = function() {
      paste0(project, "_ous_shapes_", data_update_ymd, ".geojson")
    },
    content = function(file) {
      write_sf(filtered_shapes(), file)
    }
  )
  
  
  # REPORTING -----------------------------
  
  output$reporting_totals_table <- renderDataTable(
    make_reporting_totals_table(
      responses = responses(),
      respondent_info = respondent_info(),
      shapes = shapes()
    ),
    server = FALSE,
    options = list(
      pageLength = 100,
      dom = "t",
      lengthChange = FALSE,
      searching = FALSE
    )
  )
  
  output$reporting_by_sector_table <- renderDataTable(
    make_reporting_sector_table(
      responses = responses(),
      respondent_info = respondent_info(),
      shapes = shapes()
    ),
    server = FALSE,
    options = list(
      pageLength = 100,
      dom = "t",
      lengthChange = FALSE,
      searching = FALSE,
      columnDefs = list(list(width = '250px', targets = "Sector"))
    )
  )
  
  # reporting table titles
  
  output$reporting_totals_title <- renderText("Totals")
  
  output$reporting_by_sector_title <- renderText("By Sector")
  
  ## download CSVs ----
  
  data_report_totals <- reactive(reporting_totals)
  
  output$download_report_totals <- downloadHandler(
    filename = function() {
      paste0(project, "_ous_reporting_totals.csv")
    },
    content = function(file) {
      write_csv(data_report_totals(), file)
    }
  )
  
  data_report_sector <- reactive(reporting_by_sector)
  
  output$download_report_sector <- downloadHandler(
    filename = function() {
      paste0(project, "_ous_reporting_by_sector.csv")
    },
    content = function(file) {
      write_csv(data_report_sector(), file)
    }
  )
})
