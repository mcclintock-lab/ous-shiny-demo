# shiny ui

ui <- (dashboardPage(
  dashboardHeader(
    title = app_title,
    tags$li(
      class = "dropdown",
      shinyWidgets::actionBttn(
        inputId = "refresh",
        label = "Refresh App",
        icon = icon("refresh"),
        style = "simple",
        size = "sm"
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("dashboard", verify_fa = F)
      ),
      menuItem(
        "Data Explorer",
        tabName = "explore",
        icon = icon("th", verify_fa = F)
      ),
      menuItem(
        "Shape Viewer",
        tabName = "shapes",
        icon = icon("map", verify_fa = F)
      )
    )
  ),
  
  dashboardBody(
    # javascript
    shinyjs::useShinyjs(),
    # refresh button function
    shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
    
    # STYLING ----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # main menu items
    tabItems(
      # OVERVIEW -------------------------------------------------------------
      tabItem(
        tabName = "overview",
        
        div(
          id = "overview-tab",
          ## value boxes ----
            width = "100%",
            
            # top row of boxes
            div(
              class = "col-sm-12 col-md-12 col-lg-12",
              id = "top-row",
              
              # latest data and island option stack
              div(
                id = "data-island-stack",
                class = "col-sm-6 col-md-6 col-lg-3",
                
                # latest data text
                div(
                  id = "latest-data",
                  
                  div(id = "latest-data-box",
                      box(
                        tags$a(
                          href = seasketch_url,
                          target = "_blank",
                          img(src = "images/seasketch-logo.png",
                              style = "height: 35px; margin-bottom: 17px; margin-top: 10px;"),
                          width = "100%"
                        ),
                        
                        uiOutput("latest_data"),
                        width = "100%"
                      ))
                ),
              ),
              
              # value box row
              div(
                id = "value-box-row",
                class = "col-sm-12 col-md-12 col-lg-12",
                
                # value boxes with response and representation figures
                div(class = "col-sm-4 col-md-4 col-lg-4",
                    valueBoxOutput("individual_respondents", width = "33%")),
                
                div(class = "col-sm-4 col-md-4 col-lg-4",
                    valueBoxOutput("individuals_represented", width = "33%")),
                
                div(class = "col-sm-4 col-md-4 col-lg-4",
                    valueBoxOutput("sector_responses", width = "33%"))
              )
            ),
            
            ## target table ----
            div(
              id = "target-box",
              class = "col-sm-12 col-md-12 col-lg-12",
              
              shinydashboardPlus::box(
                title = "Targets",
                width = 12,
                align = "center",
                collapsible = TRUE,
                dropdownMenu = shinydashboardPlus::boxDropdown(
                  shinydashboardPlus::boxDropdownItem("Save target changes", id = "save_targets")
                ),
                
                DT::dataTableOutput("target_table") |>
                  shinycssloaders::withSpinner(type = 8)
              )
          ),
          
          
          
          
          ## plots ----
          div(id = "plots-row",
              class = "col-sm-12 col-md-12 col-lg-12",
              fluidRow(
                # sector responses
                div(
                  class = "col-sm-12 col-md-12 col-lg-6",
                  id = "resp-plot-box",
                  
                  shinydashboardPlus::box(
                    title = "Responses by Sector",
                    width = 12,
                    collapsible = TRUE,
                    
                    dropdownMenu = shinydashboardPlus::boxDropdown(
                      shinydashboardPlus::boxDropdownItem("Represented", id = "represented"),
                      shinydashboardPlus::boxDropdownItem("Responses", id = "responses")
                      
                    ),
                    
                    plotOutput("resp_plot") |>
                      shinycssloaders::withSpinner(type = 8)
                  )
                ),
                
                
                # demographics
                div(
                  class = "col-sm-12 col-md-12 col-lg-6",
                  id = "demo-plot-box",
                  
                  shinydashboardPlus::box(
                    title = "Demographics",
                    width = 12,
                    collapsible = TRUE,
                    
                    dropdownMenu = shinydashboardPlus::boxDropdown(
                      shinydashboardPlus::boxDropdownItem("Age", id = "age"),
                      shinydashboardPlus::boxDropdownItem("Gender", id = "gender")
                      
                    ),
                    
                    plotOutput("demo_plot") |>
                      shinycssloaders::withSpinner(type = 8)
                  )
                )
              )
          )
        )
      ),
      
      # DATA TABLE ----------------------------------------------------------
      tabItem(tabName = "explore",
              
              ## main tab ----
              div(id = "dt-box",
                  shinydashboardPlus::box(
                    title = htmlOutput("datatable_title"),
                    width = 12,
                    headerBorder = FALSE,
                    dropdownMenu = shinydashboardPlus::boxDropdown(
                      shinydashboardPlus::boxDropdownItem(htmlOutput("edit_data_button"),
                                                          id = "edit_datatable"),
                      shinydashboardPlus::boxDropdownItem(htmlOutput("save_edits_button"),
                                                          id = "save_datatable_edits")
                    ),
                    tabBox(
                      width = "100%",
                      tabPanel(
                        title = "All Data",
                        
                        # view in map button
                        shinyWidgets::actionBttn(
                          "dt_view_shapes",
                          "View in Map",
                          style = "simple",
                          icon = icon("map")
                        ),
                        # download shapes
                        shinyWidgets::downloadBttn(
                          "download_responses",
                          "Download",
                          style = "simple",
                        ),
                        width = "100%",
                        DT::dataTableOutput("datatable")
                      ),
                      
                      ## change log ----
                      tabPanel(
                        title = "Change Log",
                        box(
                          width = 12,
                          DT::dataTableOutput("change_log_table")
                        )
                      ),
                      
                      ## corrections ----
                      tabPanel(
                        title = "Corrections",
                        
                        tags$br(),
                        
                        box(
                          width = 12,
                          numericInput(
                            "corrections_response_id",
                            "Response ID:",
                            NA,
                            width = "25%"
                          ),
                          div(
                            id = "corrections-text-input",
                            textAreaInput(
                              "corrections_text",
                              "Corrections to be made:",
                              resize = "vertical"
                            )
                          ),
                          div(
                            id = "corrections-reason-input",
                            textAreaInput(
                              "corrections_reason",
                              "Reason for correction:",
                              resize = "vertical"
                            )
                          )
                        ),
                        
                        tags$br(),
                        
                        shinydashboardPlus::box(
                          id = "corrections_box",
                          dropdownMenu = shinydashboardPlus::boxDropdown(
                            shinydashboardPlus::boxDropdownItem(
                              htmlOutput("toggle_fixed_button"),
                              id = "toggle_fixed")),
                          title = p(div(
                            id = "corrections_title",
                            shinyWidgets::actionBttn(
                              "submit_correction",
                              "Submit new ",
                              style = "simple",
                              size = "sm",
                              icon = icon("plus")
                            )
                          )),
                          width = 12,
                          
                          DT::dataTableOutput("corrections_table")
                        )
                      )
                    )
                  )
              )
      ),
      
      # SHAPE VIEWER --------------------------------------------------------
      tabItem(tabName = "shapes",
              
              fluidRow(
                div(
                  id = "shape-viewer-box",
                  class = "col-sm-12 col-md-12 col-lg-4",
                  
                  shinydashboardPlus::box(
                    title = "Shape Viewer",
                    width = 12,
                    
                    # clear filters
                    shinyWidgets::actionBttn(
                      "clear_shape_filters",
                      "Clear Map & Filters",
                      style = "simple",
                      # size = "sm",
                      icon = icon("refresh")
                    ),
                    
                    # download shapes
                    shinyWidgets::downloadBttn(
                      "download_filtered_shapes",
                      "Export Current Shapes",
                      style = "simple",
                      # size = "sm",
                    ),
                    
                    # region filter dropdown
                    shinyWidgets::pickerInput(
                      inputId = "map_regions",
                      label = "Regions: ",
                      choices = region_list,
                      selected = region_list,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE)
                    ),
                    
                    # sector filter dropdown
                    shinyWidgets::pickerInput(
                      inputId = "map_sector",
                      label = "Sectors: ",
                      choices = sectors,
                      selected = sectors,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE)
                    ),
                    
                    # filter by facilitation
                    div(
                      id = "filter-facil",
                      selectInput(
                        "map_facil_var",
                        label = "Filter by Facilitation",
                        choices = list(
                          "Both" = "both",
                          "Facilitated" = TRUE,
                          "Unfacilitated" = FALSE
                        )
                      )
                    ),
                    
                    # filter by id text, button, and input
                    div(id = "filter-id-text",
                        htmlOutput("filter_id_text")),
                    
                    div(
                      id = "filter-id-toggle",
                      shinyWidgets::switchInput(
                        "filter_id",
                        label = NULL,
                        value = FALSE,
                        size = "small"
                      )
                    ),
                    
                    div(
                      id = "shape-id",
                      textInput("shape_id", label = NULL,
                                placeholder = "Response IDs separated by commas")
                    )
                    
                  )
                ),
                
                div(class = "col-sm-12 col-md-12 col-lg-8",
                    
                    box(
                      width = "50%",
                      leaflet::leafletOutput("map") |>
                        shinycssloaders::withSpinner(type = 8)
                    ))
              ))
    )
  )
))

# secure app with shinymanager
if (secure == TRUE) {
  ui <- secure_app(ui, enable_admin = TRUE)
} else {
  ui <- ui
}
