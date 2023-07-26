# shiny server

print("server")

# Define server logic
shinyServer(function(input, output) {
  # info boxes ------------------------------------------------
  
  # latest data update
  output$latest_data <- renderUI({
    HTML(paste0("Data updated", br(), "<b>", data_update, "</b>"))
  })
  
  # number respondents box
  output$num_respondents <- renderValueBox({
    valueBox(
      nrow(respondent_info),
      HTML(paste0("Individual", br(), "Respondents")),
      icon = icon("user", class = "fa-solid fa-user"),
      color = "blue"
    )
  })
  
  # number represented box
  output$num_rep <- renderValueBox({
    valueBox(
      sum(respondent_info$max_rep),
      HTML(paste0("Individuals", br(), "Represented")),
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # sector responses box
  output$sec_resp <- renderValueBox({
    valueBox(nrow(responses),
             HTML(paste0("Sector", br(), "Responses")),
             icon = icon("water"),
             color = "teal")
  })
  
  
  # targets ------------------------------------------------
  
  # targets table
  
  output$target_table <- renderDataTable({
    make_target_table()
    
  })
  
  # demographic plot ---------------------
  
  output$demo_plot <- renderPlot({
    make_demo_plot(df = respondent_info)
    
  })
  
  # Sector plot ------------------------------------
  
  # initially define metric that can be changed with the box dropdown menu
  resp_plot_metric <- reactiveVal()
  resp_plot_metric("represented")
  
  output$resp_plot <- renderPlot({
    make_sector_plot(metric = resp_plot_metric())
    
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
  
  
  # datatable ----------------------------------------------
  
  output$datatable <- renderDataTable(expr = make_datatable(),
                                      server = FALSE) # needed to use plugins
  
  observeEvent(input$dt_view_shapes, {
    if (is.null(input$datatable_rows_selected)) {
      showNotification("Please select a response in the table to view on map",
                       type = "error")
      
    } else {
      updateTabItems(inputId = "tabs", selected = "shapes")
      
      selected_row <- input$datatable_rows_selected
      selected_id <- unique(responses$response_id[selected_row])
      
      print(selected_row)
      
      updateTextInput(inputId = "shape_id", value = selected_id)
      
      updateSwitchInput(inputId = "filter_id", value = TRUE)
      
    }
    
  })
  
  output$n_dups <- renderText(nrow("n_dups"))
  outputOptions(output, "n_dups", suspendWhenHidden = FALSE)
  
  output$dup_table <- renderDataTable(make_dups_table())
  
  
  
  # Shape viewer -------------------------------------------------
  
  output$map <- renderLeaflet({
    if (input$filter_id == TRUE) {
      # parse user input for filter by response_id
      shape_id <-
        as.numeric(strsplit(input$shape_id, split = ", |,")[[1]])
      
      shapes <- shapes %>%
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
        shapes <- shapes %>%
          filter(region %in% map_regions,
                 is_facilitated %in% map_facil)
        
      } else {
        shapes <- shapes %>%
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
    leaflet(shapes) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      # addTiles("https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png",
      # options = providerTileOptions(apikey = "")) %>%
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
      ) %>%
      addPolylines(color = "black", weight = 0.5) %>%
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
      paste0(project, "_ous_shapes_", data_update_ymd, ".fgb")
    },
    content = function(file) {
      write_sf(filtered_shapes(), file)
    }
  )
  
  
  # reporting -----------------------------
  
  output$reporting_totals_table <- renderDataTable(
    make_reporting_totals_table(),
    server = FALSE,
    options = list(
      pageLength = 100,
      dom = "t",
      lengthChange = FALSE,
      searching = FALSE
    )
  )
  
  output$reporting_by_sector_table <- renderDataTable(
    make_reporting_sector_table(),
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
  
  # download reporting CSVs
  
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
