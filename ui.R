# UI

# R package for interactive apps
library(shinydashboard)
library(leaflet)

# Dashboard header
header <- dashboardHeader(title = "Healthcare Dashboard")

# Dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarItemSelected",
    
    # Tab 1: (now mental health map / indicator-based)
    menuItem("Mental Health Care", 
             tabName = "mental", 
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
  
  # Filters for Tab 1 – now just an Indicator slicer
  # Filters for Tab 1 – Indicator + Date dropdown
  conditionalPanel(
    condition = "input.sidebarItemSelected == 'mental'",
    
    selectInput(
      inputId = "indicator",
      label = "Select Indicator",
      choices = NULL,   # populated in server
      selected = NULL
    ),
    
    selectInput(
      inputId = "end_date",
      label = "Select Time Period End Date",
      choices = NULL,   # populated in server
      selected = NULL
    )
  )
  
)

# Dashboard Body
body <- dashboardBody(
  tabItems(
    
    # ---- Tab 1: Mental Health Map by Indicator (using part2 visuals) ----
    tabItem(
      tabName = "mental",
      
      # Row 1: Map
      fluidRow(
        column(
          width = 12,
          box(
            title = tags$div(
              "Mental Health Care in the Last 4 Weeks by State",
              style = "font-size:16px; font-weight:bold;"
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("mh_map", height = "350px")
          )
        )
      ),
      
      # Row 2: Bar charts (Sex and Race/Ethnicity, United States)
      fluidRow(
        column(
          width = 6,
          box(
            title = tags$div(
              "Differences by Sex (United States)",
              style = "font-size:14px; font-weight:bold; white-space:normal;"
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("mh_sex_bar", height = "300px")
          )
        ),
        column(
          width = 6,
          box(
            title = tags$div(
              "Differences by Race/Ethnicity (United States)",
              style = "font-size:14px; font-weight:bold; white-space:normal;"
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("mh_race_bar", height = "300px")
          )
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
