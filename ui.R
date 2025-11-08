# UI

# R package for interactive maps
library(shinydashboard)
library(leaflet)

# Dashboard header
header <- dashboardHeader(title = "Healthcare Dashboard")

# Dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarItemSelected",
    
    # Tab 1: Global Cancer Mortality
    menuItem("Global Cancer Mortality", 
             tabName = "mortality", 
             icon = icon("globe")),
    
    # Tab 2: Additional tab
    menuItem("Tab 2", 
             tabName = "tab2", 
             icon = icon("chart-line")),
    
    # Tab 3: Additional tab
    menuItem("Tab 3", 
             tabName = "tab3", 
             icon = icon("flask")),
    
    # Tab 4: Additional tab
    menuItem("Tab 4", 
             tabName = "tab4", 
             icon = icon("search"))
  ),
  
  # Filters for Tab 1
  conditionalPanel(
    condition = "input.sidebarItemSelected == 'mortality'",
    
    sliderInput(inputId = "year",
                label = "Years included",
                min = 2003,
                max = 2015,
                value = c(2003, 2015),
                sep = "",
                step = 1
    ),
    
    selectInput("region", "Select Region",   
                choices = c("World", "North America", "South America", "Asia", "Europe", "Oceania", "Africa"),
                selected = "World"),
    
    selectInput("cancer", "Select Cancer Type",   
                choices = c("All Cancers", "Breast", "Lung", "Prostate", "Colon", "Pancreas"),
                selected = "All Cancers"),
    
    uiOutput("yearSelectorUI")
  )
)

# Dashboard Body
body <- dashboardBody(
  tabItems(
    
    # ---- Tab 1: Global Cancer Mortality ----
    tabItem(tabName = "mortality",
            fluidRow(
              # Info Box 1
              infoBoxOutput(width = 3,
                            "infoYears" 
              ),
              # Info Box 2
              infoBoxOutput(width = 3,
                            "infoDRFirst"
              ),
              # Info Box 3
              infoBoxOutput(width = 3,
                            "infoDRLast"
              ),
              # Info Box 4
              infoBoxOutput(width = 3,
                            "change"
              )
            ),
            
            
            fluidRow(
              # Plot 1 (First row, First col)
              column(width = 6,
                     box(title = "Regional Trends in Cancer Mortality Rate", width = 12,
                         plotOutput("regionPlot", height = "300px"))
              ),
              # Plot 2 (Second row, Second col)
              column(width = 6,
                     box(title = "Average Cancer Mortality Rate by Cancer Type", width = 12,
                         plotOutput("cancerTypePlot", height = "300px"))
              )
            ),
            
            fluidRow(
              # Plot 3 (Second row, First col)
              column(width = 6,
                     box(
                       title = tags$div(
                            "Average Mortality Rate per Age Group by Region, Cancer Type and Year",
                           style = "font-size:15px;"
                         ), 
                       width = 12,
                       plotOutput("ageHistPlot", height = "300px"))
              ),
              # Plot 4 (Second row, Second col)
              column(width = 6,
                     box(
                       title = tags$div(
                         "Countries with Largest Change in Mortality Rates by Cancer Type",
                         style = "font-size:15px;"
                       ),
                       width = 12,
                       plotOutput("divergingPlot", height = "300px")
                     )
              )
            ),
            
            fluidRow(
              # Plot 5 (Spans entire third row)
              column(width = 12,
                     box(title = "Worldwide Change in Mortality Rates by Cancer Type", width = 12,
                         leafletOutput("mapPlot", height = "425px"))
              )
            )
    ),
    
    # Tab 2
    tabItem(tabName = "tab2",
            h3("Project 2")
    ),
    
    # Tab 3
    tabItem(tabName = "tab3",
            h3("Project 3")
    ),
    
    # Tab 4
    tabItem(tabName = "tab4",
            h3("Project 4")
    )
  )
)

# Dashboard Output
dashboardPage(
  header,
  sidebar,
  body
)