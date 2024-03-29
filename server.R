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
  
  # PRIVILEGES --------------------------------
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
  responses_reactive <- reactiveVal()
  
  observe({
    responses_reactive(responses())
  })
  
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
  shapes_reactive <- reactiveVal()
  
  observe({
    shapes_reactive(shapes())
  })
  
  # data update datetime
  data_update_reader <- reactiveFileReader(
    intervalMillis = 1.8e3,
    session = session,
    filePath = "data/temp/data_date.RDS",
    readFunc = read_rds
  )
  
  data_update <- reactive(data_update_reader())
  
  
  # REFRESH APP ----------------------------------------------
  observeEvent(input$refresh, {
    shinyjs::js$refresh_page()
  })
  
  # OVERVIEW ------------------------------------------------
  
  ## info boxes ----
  
  # datetime update box
  output$data_update <- renderValueBox({
    valueBox(
      HTML("Responses</br>Updated:"),
      HTML(data_update() |> str_replace(" ", "</br>")),
      icon = icon("database")
    )
  })
  
  # number respondents box
  output$individual_respondents <- renderValueBox({
    valueBox(
      length(unique(responses_reactive()$response_id)),
      HTML(paste0("Individual", br(), "Responses")),
      icon = icon("user", class = "fa-solid fa-user")
    )
  })
  
  # number represented box
  output$individuals_represented <- renderValueBox({
    valueBox(
      sum(respondent_info()$participants),
      HTML(paste0("Participants", br(), "Represented")),
      icon = icon("users")
    )
  })
  
  # sector responses box
  output$sector_responses <- renderValueBox({
    valueBox(
      nrow(responses_reactive()),
      HTML(paste0("Sector", br(), "Responses")),
      icon = icon("water")
    )
  })
  
  
  ## targets ------------------------------------------------
  
  ### target table ----
  output$target_table <- DT::renderDataTable({
    make_target_table(responses = responses_reactive())
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
        changed_target <- targets_progress[[changed_row, "sector"]]
        
        changed_target_table <- target_table_reactive()
        
        changed_target_table[changed_target_table$sector == changed_target, ]$target <-
          changed_val
        
        target_table_reactive(changed_target_table)
        
        # save table state to global object
        assign("changed_target_table", changed_target_table, envir = .GlobalEnv)
        
      })
      
      # save_targets listener - overwrite source targets
      observeEvent(input$save_targets, {
        write_csv(changed_target_table, "data/survey_targets.csv")
        
        assign("targets",
               changed_target_table,
               envir = .GlobalEnv)
        
        # rerender table on save
        output$target_table <- DT::renderDataTable({
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
  
  # initially define metric that can be changed with the box dropdown menu
  demo_plot_metric <- reactiveVal()
  demo_plot_metric("age")
  
  output$demo_plot <- renderPlot({
    make_demo_plot(respondent_info = respondent_info(),
                   metric = demo_plot_metric())
    
  })
  
  # observe dropdown input
  observeEvent(input$age, {
    if (demo_plot_metric() != "age") {
      demo_plot_metric("age")
    }
  })
  
  observeEvent(input$gender, {
    if (demo_plot_metric() != "gender") {
      demo_plot_metric("gender")
    }
  })
  
  ## sector plot ------------------------------------
  
  # initially define metric that can be changed with the box dropdown menu
  resp_plot_metric <- reactiveVal()
  resp_plot_metric("represented")
  
  output$resp_plot <- renderPlot({
    make_sector_plot(metric = resp_plot_metric(),
                     responses = responses_reactive())
    
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
  
  edit_data_status <- reactiveVal()
  edit_data_status(FALSE)
  
  # conditional styling based on edit status
  datatable_title_style <- reactiveVal()
  edit_data_button <- reactiveVal()
  save_edits_button <- reactiveVal()
  save_data_button <- reactiveVal()
  
  observe({
    if (!is.null(write_status()) && write_status() == FALSE) {
      datatable_title_style("Ocean Use Survey Data")
      edit_data_button("<div style='color:#bababa'>Edit data</div>")
      save_edits_button("<div style='color:#bababa'>Save edits</div>")
      
    } else {
      
      if (edit_data_status() == FALSE) {
        datatable_title_style("Ocean Use Survey Data")
        edit_data_button("✏️ Edit data")
        save_edits_button("<div style='color:#bababa'>◽️ Save edits</div>")
        
      } else {
        datatable_title_style("<div style='color:orange'>Ocean Use Survey Data - Editing</div>")
        edit_data_button("❌ Stop editing")
        save_edits_button("💾 Save edits")
      }
    }
  })
  
  output$datatable_title <- renderText(datatable_title_style())
  output$edit_data_button <- renderText(edit_data_button())
  output$save_edits_button <- renderText(save_edits_button())
  
  # create responses value for updating datatable when columns are selected
  responses_datatable <- reactiveVal()
  
  observe({
    responses_datatable(responses())
  })
  
  ## main table ----
  output$datatable <-
    DT::renderDataTable(expr = make_datatable(responses = responses_datatable(),
                                              edit_data_status = edit_data_status()),
                        server = FALSE)
  
  ### select columns ----
  observeEvent(input$datatable_column_select, {
    
    responses_datatable(responses())
    
    col_names <- names(responses())
    
    selected_columns <- 
      if (is.null(input$datatable_column_select)) {
      col_names
    } else {
      input$datatable_column_select 
    }
    
    responses_datatable_selected <- responses_datatable() |> 
      select(all_of(selected_columns))
    
    responses_datatable(responses_datatable_selected)
    
  },
  ignoreNULL = FALSE
  )
  
  ### view in map ----
  observeEvent(input$dt_view_shapes, {
    if (is.null(input$datatable_rows_selected)) {
      shinyWidgets::show_alert(title = "Please select a response in the table to view in map",
                               type = "warning")
      
    } else {
      updateTabItems(inputId = "tabs", selected = "shapes")
      
      selected_row <- input$datatable_rows_selected
      selected_id <- unique(responses_reactive()$response_id[selected_row])
      
      updateTextInput(inputId = "shape_id", value = selected_id)
      
      shinyWidgets::updateSwitchInput(inputId = "filter_id", value = TRUE)
      
    }
  })
  
  ### data editing ----
  
  # init reactive objects
  latest_save <- reactiveVal()
  responses_edited <- reactiveVal()
  shapes_edited <- reactiveVal()
  
  observe({
    # used to store latest save state for comparison with responses_edited() and downloading
    latest_save(responses())
    # used to store current state of table edits
    responses_edited(responses())
    # used to store current state of shape edits
    shapes_edited(shapes())
  })
  
  # listen for datatable edit status
  observeEvent(input$edit_datatable, {
    
    if (!is.null(write_status()) && write_status() == TRUE) {
      
      edit_data_status(!edit_data_status())
      
      # revert responses_edited to latest save state when edit status changes to FALSE
      if (edit_data_status() == FALSE) {
        
        responses_edited(latest_save())
      }
    }
  })
  
  #### edit mode ----
  
  # listen for user edits to table and update responses accordingly
  observeEvent(input$datatable_cell_edit, {
    
    if (edit_data_status() == TRUE) {
      
      changed_row <- input$datatable_cell_edit$row
      changed_col <- input$datatable_cell_edit$col
      changed_val <- input$datatable_cell_edit$value
      changed_response_id <- responses_edited()[[changed_row, "response_id"]]
      sector_changed <- changed_col == which(names(responses()) == "sector")
      response_id_changed <- changed_col == which(names(responses()) == "response_id")
      
      # ensure logical variables conform
      if (class(responses()[[changed_col]]) == "logical") {
        
        changed_val <- as.logical(changed_val)
        
        if (is.na(changed_val)) {
          
          shinyWidgets::show_alert(title = "This variable requires a value of 'true' or 'false'",
                                   type = "warning")
          
        } else {
          
          # make changes to reactive object with static intermediary
          changed_responses <- responses_edited()
          changed_responses[changed_row, changed_col] <- changed_val
          responses_edited(changed_responses)
        }
      } else {
        
        if (sector_changed == TRUE) {
          
          if (!changed_val %in% sectors) {
            
            shinyWidgets::show_alert(title = "That sector is not recognized",
                                     text = "Please check spelling, punctuation, and capitalization",
                                     type = "warning")
          }
          
          changed_shapes <- shapes_edited()
          original_sector <- latest_save()[[changed_row, changed_col]] 
          shapes_changed_row <- which(changed_shapes$response_id == changed_response_id &
                                        changed_shapes$sector == original_sector)
          changed_shapes[shapes_changed_row, "sector"] <- changed_val
          shapes_edited(changed_shapes)
          
        } else if (response_id_changed == TRUE) {
          
          if (!changed_val %in% responses_edited()$response_id) {
            
            shinyWidgets::show_alert(title = "That response ID doesn't exist in the data",
                                     type = "warning")
          }
          
          changed_shapes <- shapes_edited()
          original_response_id <- latest_save()[[changed_row, changed_col]] 
          shapes_changed_row <- which(changed_shapes$response_id == changed_response_id)
          changed_shapes[shapes_changed_row, "response_id"] <- changed_val
          shapes_edited(changed_shapes)
        }
        
        # make changes to reactive object with static intermediary
        changed_responses <- responses_edited()
        changed_responses[changed_row, changed_col] <- changed_val
        responses_edited(changed_responses)
      }
    }
  })
  
  #### save edits ----
  
  # save_datatable_edits listener - writes changes to responses.RDS and adds to change log
  observeEvent(input$save_datatable_edits, {
    
    # save and update change log if changes have been made
    if (!is.null(input$datatable_cell_edit) && edit_data_status() == TRUE 
        && identical(responses_edited(), latest_save()) == FALSE
    ) {
      
      # update change log
      make_change_log(
        latest_save(),
        responses_edited(),
        change_log,
        current_user()
      )
      
      # update responses
      write_rds(responses_edited(), "data/temp/responses.RDS")
      write_rds(change_log, "data/change_log.RDS")
      
      latest_save(responses_edited())
      responses_reactive(responses_edited())
      
      # update shapes
      changed_shapes <- shapes_edited() |> 
        select(response_id, sector, all_of(shape_specific_attributes)) |> 
        right_join(responses_edited(),
                   by = c("response_id", "sector"))
      
      shapes_edited(changed_shapes)
      shapes_reactive(changed_shapes)
      
      write_rds(changed_shapes, "data/temp/shapes.RDS")
      
      # rerender table
      output$datatable <- DT::renderDataTable({
        make_datatable(responses = responses_edited(),
                       edit_data_status = edit_data_status())
      })
      
      output$change_log_table <- DT::renderDataTable({
        make_change_log_table(change_log = change_log)
      }) 
    }
  })
  
  ### data download ----
  output$download_responses <- downloadHandler(
    filename = function() {
      paste0(project, "_processed_ous_responses_", data_update_ymd, ".csv")
    },
    content = function(file) {
      write_csv(latest_save(), file)
    }
  )
  
  ## corrections ----
  
  corrections_data <- read_rds("data/corrections.RDS") |> 
    arrange(by = desc(fixed))
  
  output$corrections_table <-
    DT::renderDataTable(make_corrections_table(corrections_data, edit_data_status()))
  
  corrections <- reactiveVal()
  corrections(corrections_data)
  
  corrections_proxy <- DT::dataTableProxy("corrections_table")
  
  # submit new correction
  observeEvent(input$submit_correction, {
    
    if (is.na(input$corrections_response_id) | 
        input$corrections_text == "" |
        input$corrections_reason == "") {
      shinyWidgets::show_alert(title = "Please enter a valid response ID, correction, and reason",
                               type = "warning")
    } else {
      new_entry <- data.frame(
        response_id = input$corrections_response_id,
        correction = input$corrections_text,
        reason = input$corrections_reason,
        user = ifelse(class(current_user()) == "character", current_user(), NA),
        date = as.character(now() |> substr(1, 19)),
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
        updateTextAreaInput(inputId = "corrections_reason",
                            value = character(0))
      } else {
        shinyWidgets::show_alert("The submitted response ID doesn't exist in the dataset",
                                 type = "warning")
      }
    }
    
  })
  
  # mark entry as fixed
  observeEvent(input$toggle_fixed, {
    
    if (!is.null(write_status()) && write_status() == TRUE) {
      
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
    }
  })
  
  # edit corrections table
  observeEvent(input$corrections_table_cell_edit, {
    
    changed_row <- input$corrections_table_cell_edit$row
    changed_col <- input$corrections_table_cell_edit$col
    changed_val <- input$corrections_table_cell_edit$value
    
    changed_corrections <- corrections()
    changed_corrections[changed_row, changed_col] <- changed_val
    corrections(changed_corrections)
    
    write_rds(corrections(), "data/corrections.RDS")
  })
  
  # button styling
  observe({
    if (!is.null(write_status()) && write_status() == FALSE) {
      
      output$toggle_fixed_button <- renderText("<div style='color:#bababa'>◽ Toggle fixed</div>")
      
    } else {
      
      output$toggle_fixed_button <- renderText("✅ Toggle fixed")
    }
  })
  
  ## changelog ----
  output$change_log_table <- DT::renderDataTable({
    make_change_log_table(change_log = change_log)
  })
  
  ## duplicates ----
  output$n_dups <- renderText(nrow("n_dups"))
  outputOptions(output, "n_dups", suspendWhenHidden = FALSE)
  
  output$dup_table <- DT::renderDataTable(make_dups_table())
  
  
  # SHAPE VIEWER -------------------------------------------------
  
  output$map <- renderLeaflet({
    library(sf)
    
    shapes <- shapes_reactive()
    
    # save filtered shapes as reactive expression to global env for shape export
    assign("filtered_shapes", reactive(shapes), env = .GlobalEnv)
    
    # number of shapes displayed - added to map with `addControl()`
    shapes_displayed <- HTML({
      paste0("Shapes displayed: ",
             div(id = "shapes-number", nrow(shapes)))
    })
    
    fill_opacity <- 0.5 / log(nrow(shapes))
    fill_opacity <- ifelse(fill_opacity > 0.3, 0.3, fill_opacity)
    
    ## map popup ----
    map_popup <- paste0(
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
    
    eez <- read_sf("data/eez.fgb")
    
    # get bbox for fitBounds()
    bbox <- st_bbox(shapes)
    
    ## render map ----
    map <- leaflet(shapes) |>
      # addProviderTiles("Esri.WorldStreetMap") |>
      addTiles(
        paste0("https://api.mapbox.com/styles/v1/mapbox/streets-v11/tiles/{z}/{x}/{y}?access_token=", mb_pk),
        attribution = "© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> <strong><a href='https://www.mapbox.com/map-feedback/' target='_blank'>Improve this map</a></strong>"
      ) |>
      addPolylines(
        data = eez,
        group = "eez_line",
        color = "red",
        weight = 2,
        opacity = 0.2,
        label = "EEZ"
      ) |> 
      addPolygons(
        group = "polygons_and_lines",
        stroke = TRUE,
        weight = 0.02,
        color = "black",
        fillOpacity = fill_opacity,
        fillColor = "red",
        highlight = highlightOptions(
          color = 'yellow',
          weight = 5,
          bringToFront = FALSE,
          sendToBack = TRUE,
          stroke = 2,
          opacity = 1
        ),
        popup = map_popup
      ) |>
      addPolylines(
        group = "polygons_and_lines",
        color = "black",
        weight = 0.5
      ) |> 
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) |> 
      addControl(shapes_displayed, position = "topright", layerId = "shapes_displayed")
    
    return(map)
  })
  
  # filter by id
  output$filter_id_text <- renderText({
    "Filter by Response ID:"
  })
  
  # clear filter button
  observeEvent(input$clear_shape_filters, {
    shinyWidgets::updatePickerInput(session = getDefaultReactiveDomain(),
                                    inputId = "map_regions",
                                    selected = character(0))
    
    shinyWidgets::updatePickerInput(session = getDefaultReactiveDomain(),
                                    inputId = "map_sectors",
                                    selected = character(0))
    
    shiny::updateSelectInput(inputId = "map_facil_var", selected = "both")
    
    shinyWidgets::updateSwitchInput(inputId = "filter_id", value = FALSE)
    
    shiny::updateTextInput(inputId = "shape_id", value = character(0))
    
  })
  
  ## update map -----
  observeEvent(
    c(
      input$filter_id,
      input$map_sectors,
      input$map_facil_var,
      input$map_regions
    ), {
      
      shapes <- shapes_reactive()
      
      if (input$filter_id == TRUE) {
        # parse user input for filter by response_id
        shape_id <-
          as.numeric(strsplit(input$shape_id, split = ", |,")[[1]])
        
        shapes <- shapes |>
          filter(response_id %in% shape_id)
        
      } else {
        
        map_sectors <- if (is.null(input$map_sectors)) {
          sectors
        } else {
          input$map_sectors
        }
        
        # look for input$map_regions in region_represented column
        region_detected <- 
          if (is.null(input$map_regions)) {
            sapply(shapes$regions_represented, function(x) TRUE) |> as.logical()
          } else {
            sapply(shapes$regions_represented, function(x) any(str_detect(x, input$map_regions))) |> as.logical()
          }
        
        shapes <- shapes[region_detected, ]
        
        if (input$map_facil_var == "both") {
          map_facil <- c(TRUE, FALSE)
          
        } else {
          map_facil <- input$map_facil_var
        }
        
        shapes <- shapes |>
          filter(sector %in% map_sectors &
                   is_facilitated %in% map_facil)
      }
      
      assign("filtered_shapes", reactive(shapes), env = .GlobalEnv)
      
      # number of shapes displayed - added to map with `addControl()`
      shapes_displayed <- HTML({
        paste0("Shapes displayed: ",
               div(id = "shapes-number", nrow(shapes)))
      })
      
      fill_opacity <- 0.5 / log(nrow(shapes))
      fill_opacity <- ifelse(fill_opacity > 0.3, 0.3, fill_opacity)
      
      map_popup <- paste0(
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
      
      # get bbox for fitBounds()
      bbox <- st_bbox(shapes)
      
      leafletProxy("map", data = shapes) |> 
        clearGroup("polygons_and_lines") |> 
        removeControl("shapes_displayed") |> 
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) |> 
        addPolygons(
          group = "polygons_and_lines",
          stroke = TRUE,
          weight = 0.02,
          color = "black",
          fillOpacity = fill_opacity,
          fillColor = "red",
          highlight = highlightOptions(
            color = 'yellow',
            weight = 5,
            bringToFront = FALSE,
            sendToBack = TRUE,
            stroke = 2,
            opacity = 1
          ),
          popup = map_popup
        ) |>
        addPolylines(
          group = "polygons_and_lines",
          color = "black",
          weight = 0.5
          ) |>
        addControl(shapes_displayed, position = "topright", layerId = "shapes_displayed")
    },
    ignoreInit = TRUE
    )
  
  # export shapes handler
  output$download_filtered_shapes <- downloadHandler(
    filename = function() {
      paste0(project, "_ous_shapes_", data_update_ymd, ".geojson")
    },
    content = function(file) {
      write_sf(filtered_shapes(), file)
    }
  )
})
