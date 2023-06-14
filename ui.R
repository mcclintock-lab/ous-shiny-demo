# shiny ui


dashboardPage(
  dashboardHeader(title = "Brazil Ocean Use"),
  dashboardSidebar(
    sidebarMenu(
      
      id = "tabs",
      menuItem("Analytics", tabName = "analytics", icon = icon("dashboard", verify_fa = F)),
      menuItem("Data Explorer", tabName = "explore", icon = icon("th", verify_fa = F)),
      menuItem("Shape Viewer", tabName = "shapes", icon = icon("map", verify_fa = F)),
      menuItem("Reporting", tabName = "reporting", icon = icon("file-lines", verify_fa = F))
    )
  ),
  
  dashboardBody(
    
    # STYLING ----
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # main menu items
    tabItems(
      
      # ANALYTICS -------------------------------------------------------------
      tabItem(tabName = "analytics",
              
              # totals and responses by sector --------------------------------
              fluidRow(
                
                width = "100%",
                
                # top row of boxes
                div(class = "col-sm-12 col-md-12 col-lg-12",
                    id = "top-row",
                    
                    # latest data and island option stack
                    div(id = "data-island-stack",
                        class = "col-sm-6 col-md-6 col-lg-3",
                        
                        # latest data text
                        div(id = "latest-data",
                            class = "col-sm-12 col-md-12 col-lg-12",
                            div(id = "latest-data-box",
                                box(
                                  
                                  tags$a(
                                    href = "https://next.seasket.ch/blueazores/app",
                                    target="_blank",
                                    img(src = "images/seasketch-logo.png",
                                        style = "height: 35px; margin-bottom: 17px; margin-top: 10px;"),
                                    width = "100%"
                                    # height = "62px"
                                  ),
                                  
                                  uiOutput("latest_data"),
                                  width = "100%"
                                )
                            )
                        ),
                    ),
                    
                    # value box row
                    div(id = "value-box-row",
                        class = "col-sm-12 col-md-12 col-lg-9",
                        
                        # value boxes with response and representation figures
                        div(class = "col-sm-4 col-md-4 col-lg-4",
                            valueBoxOutput("num_respondents", width = "33%")
                        ),
                        
                        div(class = "col-sm-4 col-md-4 col-lg-4",
                            valueBoxOutput("num_rep", width = "33%")
                        ),
                        
                        div(class = "col-sm-4 col-md-4 col-lg-4",
                            valueBoxOutput("sec_resp", width = "33%")
                        )
                    )
                ),
                
                # target table 
                div(id = "target-box",
                    class = "col-sm-12 col-md-12 col-lg-12",
                    
                    shinydashboardPlus::box(
                      title = "Targets",
                      width = 12,
                      align = "center",
                      collapsible = TRUE,
                      
                      dataTableOutput("target_table") %>% 
                        withSpinner(type = 8)
                    )
                )
              ),
              
              
              
              
              # plots --------------------------------------------------------
              
              div(id = "plots-row",
                  fluidRow(
                    # sector responses
                    div(class = "col-sm-12 col-md-12 col-lg-6",
                        id = "resp-plot-box",
                        
                        shinydashboardPlus::box(
                          title = "Responses by Sector",
                          width = 12,
                          collapsible = TRUE,
                          
                          dropdownMenu = shinydashboardPlus::boxDropdown(
                            
                            shinydashboardPlus::boxDropdownItem("Represented", id = "represented"),
                            shinydashboardPlus::boxDropdownItem("Responses", id = "responses")
                            
                          ),
                          
                          plotOutput("resp_plot") %>% 
                            withSpinner(type = 8)
                        )
                    ),
                    
                    
                    # demographics
                    div(class = "col-sm-12 col-md-12 col-lg-6", 
                        id = "demo-plot-box",
                        
                        shinydashboardPlus::box(
                          title = "Demographics",
                          width = 12,
                          collapsible = TRUE,
                          
                          plotOutput("demo_plot") %>% 
                            withSpinner(type = 8)
                        )
                    )
                  )
              )
      ),
      
      # DATA TABLE ----------------------------------------------------------
      tabItem(tabName = "explore",
              
              div(id = "dt-box",
                  box(width = "100%",
                      tabBox(width = "100%",
                             tabPanel(title = "All Data",
                                      
                                      # view in map button
                                      actionBttn("dt_view_shapes", "View in Map",
                                                 style = "pill", size = "sm",
                                                 icon = icon("map")),
                                      
                                      dataTableOutput("datatable") %>% 
                                        withSpinner(type = 8)
                                      
                             ),
                             # duplicates tab - only displays table if dups exist
                             tabPanel(title = "Duplicates",
                                      conditionalPanel(condition = "output.n_dups!='0'",
                                                       dataTableOutput("dup_table", width = "50%")
                                      ),
                                      conditionalPanel(condition = "output.n_dups=='0'",
                                                       HTML(paste0(br(),"No duplicate responses found"))
                                      )
                                      
                                      
                             )
                      )
                  )
              )
      ),
      
      # SHAPE VIEWER --------------------------------------------------------
      tabItem(tabName = "shapes",
              
              fluidRow(
                
                div(class = "col-sm-12 col-md-12 col-lg-4",
                    
                    box(
                      
                      width = "100%",
                      
                      # clear filters
                      actionBttn("clear_shape_filters", "Clear Map & Filters",
                                 style = "pill", size = "sm", icon = icon("refresh")),
                      
                      # choose region?
                      checkboxGroupButtons("map_regions", "Regions: ",
                                           width = "100%",
                                           selected = "terceira",
                                           choices = list("All" = "all",
                                                          "North" = "north",
                                                          "Northeast" = "northeast",
                                                          "Southeast" = "southeast",
                                                          "South" = "south")),
                      
                      # choose sectors
                      multiInput("map_sector", "Sectors: ",
                                 width = "100%",
                                 choices = c("All", sector_ids$sector),
                                 selected = "All",
                                 options = list(enable_search = FALSE)),
                      
                      
                      # filter by facilitation
                      div(id = "filter-facil",
                          selectInput("map_facil_var", label = "Filter by Facilitation",
                                      choices = list("Both" = "both",
                                                     "Facilitated" = "true",
                                                     "Unfacilitated" = "false"))
                      ),
                      
                      # filter by id text, button, and input
                      div(id = "filter-id-text",
                          htmlOutput("filter_id_text")
                      ),
                      
                      div(id = "filter-id-toggle",
                          switchInput("filter_id", label = NULL, value = FALSE,
                                      size = "small")
                      ),
                      
                      div(id = "shape-id",
                          textInput("shape_id", label = NULL,
                                    placeholder = "Response IDs separated by commas")
                      )
                      
                    )
                ),
                
                div(class = "col-sm-12 col-md-12 col-lg-8",
                    
                    box(
                      
                      width = "50%",
                      leafletOutput("map") %>% 
                        withSpinner(type = 8)
                    )
                )
              )
      ),
      
      tabItem(tabName = "reporting",
              
              div(id = "reporting-total-box",
                  
                  box(
                    width = "100%",
                    
                    textOutput("reporting_totals_title"),
                    
                    downloadBttn("download_report_totals", "Download",
                                 size = "sm",
                                 style = "jelly"),
                    
                    dataTableOutput("reporting_totals_table") %>% 
                      withSpinner(type = 8)
                  )
              ),
              
              div(id = "reporting-sector-box",
                  box(
                    width = "100%",
                    
                    textOutput("reporting_by_sector_title"),
                    
                    downloadBttn("download_report_sector", "Download",
                                 size = "sm",
                                 style = "jelly"),
                    
                    dataTableOutput("reporting_by_sector_table") %>% 
                      withSpinner(type = 8)
                  )
              )
      )
    )
  )
)










