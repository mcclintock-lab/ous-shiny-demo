# shiny ui

ui <- (dashboardPage(
  dashboardHeader(
    title = "Ocean Use Survey",
    tags$li(
      class = "dropdown",
      id = "disclaimer",
      # HTML(
      #   "*This app contains <a id=generate-data-link target=_blank href='https://github.com/mcclintock-lab/ous-shiny-demo/blob/main/R/generate_data.R'>randomly generated data</a> solely for demonstration purposes"
      # )
      "*This app contains fabricated data purely for demonstration purposes"
    ),
    tags$li(
      class = "dropdown",
      actionBttn(
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
      ),
      menuItem(
        "Reporting",
        tabName = "reporting",
        icon = icon("file-lines", verify_fa = F)
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
        
        ## value boxes ----
        fluidRow(
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
                class = "col-sm-12 col-md-12 col-lg-12",
                div(id = "latest-data-box",
                    box(
                      tags$a(
                        href = "https://seasketch.org",
                        target = "_blank",
                        img(src = "images/seasketch-logo.png",
                            style = "height: 35px; margin-bottom: 17px; margin-top: 10px;"),
                        width = "100%"
                        # height = "62px"
                      ),
                      
                      uiOutput("latest_data"),
                      width = "100%"
                    ))
              ),
            ),
            
            # value box row
            div(
              id = "value-box-row",
              class = "col-sm-12 col-md-12 col-lg-9",
              
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
              
              dataTableOutput("target_table") |>
                withSpinner(type = 8)
            )
          )
        ),
        
        
        
        
        ## plots ----
        
        div(id = "plots-row",
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
                    withSpinner(type = 8)
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
                  
                  plotOutput("demo_plot") |>
                    withSpinner(type = 8)
                )
              )
            ))
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
                        actionBttn(
                          "dt_view_shapes",
                          "View in Map",
                          style = "simple",
                          # size = "sm",
                          icon = icon("map")
                        ),
                        # download shapes
                        downloadBttn(
                          "download_responses",
                          "Download",
                          style = "simple",
                          # size = "sm",
                        ),
                        
                        dataTableOutput("datatable")
                      ),
                      
                      ## change log ----
                      tabPanel(
                        title = "Change Log",
                        box(
                          width = 12,
                          dataTableOutput("change_log_table")
                        )
                      ),
                      
                      ## corrections ----
                      tabPanel(
                        title = "Corrections",
                        
                        tags$br(),
                        
                        box(
                          width = 12,
                          # width = 12,
                          # headerBorder = FALSE,
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
                          title = p(div(
                            id = "corrections_title",
                            actionBttn(
                              "mark_fixed",
                              "Mark fixed",
                              style = "simple",
                              size = "sm",
                              icon = icon("check")
                            ),
                            actionBttn(
                              "submit_correction",
                              "Submit new ",
                              style = "simple",
                              size = "sm",
                              icon = icon("plus")
                            )
                          )),
                          width = 12,
                          
                          dataTableOutput("corrections_table")
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
                    actionBttn(
                      "clear_shape_filters",
                      "Clear Map & Filters",
                      style = "simple",
                      # size = "sm",
                      icon = icon("refresh")
                    ),
                    
                    # download shapes
                    downloadBttn(
                      "download_filtered_shapes",
                      "Export Current Shapes",
                      style = "simple",
                      # size = "sm",
                    ),
                    
                    # region filter dropdown
                    pickerInput(
                      inputId = "map_regions",
                      label = "Regions: ",
                      choices = c(unique(responses$region)),
                      selected = unique(responses$region),
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE)
                    ),
                    
                    # sector filter dropdown
                    pickerInput(
                      inputId = "map_sector",
                      label = "Sectors: ",
                      choices = c(unique(responses$sector)),
                      selected = unique(responses$sector),
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
                      switchInput(
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
                      leafletOutput("map") |>
                        withSpinner(type = 8)
                    ))
              )),
      
      # REPORTING -------------------------------------
      tabItem(
        tabName = "reporting",
        
        div(
          id = "reporting-total-box",
          
          box(
            width = "100%",
            
            textOutput("reporting_totals_title"),
            
            downloadBttn(
              "download_report_totals",
              "Download",
              size = "sm",
              style = "simple"
            ),
            
            dataTableOutput("reporting_totals_table") |>
              withSpinner(type = 8)
          )
        ),
        
        div(
          id = "reporting-sector-box",
          box(
            width = "100%",
            
            textOutput("reporting_by_sector_title"),
            
            downloadBttn(
              "download_report_sector",
              "Download",
              size = "sm",
              style = "simple"
            ),
            
            dataTableOutput("reporting_by_sector_table") |>
              withSpinner(type = 8)
          )
        )
      )
    )
  )
))

# secure app with shinymanager
if (secure == TRUE) {
  ui <- secure_app(ui, enable_admin = TRUE)
} else {
  ui <- ui
}
