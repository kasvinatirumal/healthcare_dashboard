# UI

# R package for interactive maps
library(shinydashboard)
library(leaflet)

# this is for tab 3
state_choices <- sort(c(state.name, "District of Columbia"))

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
    
    # Tab 2: Heart Disease Indicators
    menuItem("Heart Disease Indicators", 
             tabName = "heart", 
             icon = icon("heart", class = "fas")),
    
    # Tab 3: Additional tab
    menuItem("Obesity Risk Factors", 
             tabName = "obesity", 
             icon = icon("running")),
    
    # Tab 4: Additional tab
    menuItem("Mental Heatlh Care", 
             tabName = "mental", 
             icon = icon("brain"))
  ),
  
  # ---- Tab 1: Global Cancer Mortality ----
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
                choices = c("World", "North America", "South America", "Asia", 
                            "Europe", "Oceania", "Africa"),
                selected = "World"),
    
    selectInput("cancer", "Select Cancer Type",   
                choices = c("All", "Breast", "Lung", "Prostate", "Colon", "Pancreas"),
                selected = "All"),
    
    uiOutput("yearSelectorUI")
  ),
  
  # ---- Tab 2: Heart Disease Indicators ----
  conditionalPanel(
    condition = "input.sidebarItemSelected == 'heart'",
    
    checkboxGroupInput(
      inputId = "race",
      label = "Race",
      choices = c("Asian", "Hispanic", "Black", "White", "Native American", "Other"),
      selected = c("Asian", "Hispanic", "Black", "White", "Native American", "Other")
    ),
    
    checkboxGroupInput(
      inputId = "sex",
      label = "Sex",
      choices = c("Male", "Female"),
      selected = c("Male", "Female")
    ),
    
    checkboxGroupInput(
      inputId = "drinking_smoking",
      label = "Drinking/Smoking Status",
      choices = c("Neither", "Drinker Only", "Smoker Only", "Both"),
      selected = c("Neither", "Drinker Only", "Smoker Only", "Both")
    ),
    
    sliderInput(inputId = "bmi",
                label = "BMI",
                min = 10,
                max = 50,
                value = c(10, 50),
                sep = "",
                step = 5
    )
  ),
  
  # ---- Tab 3: Obesity Risk Factors (state selectors) ----
  conditionalPanel(
    condition = "input.sidebarItemSelected == 'obesity'",
    
    selectInput(
      inputId = "obesity_state1",
      label = "Select State 1",
      choices = state_choices,
      selected = "Washington"
    ),
    
    selectInput(
      inputId = "obesity_state2",
      label = "Select State 2",
      choices = state_choices,
      selected = "California"
    ),
    
    sliderInput(
      inputId = "obesity_years",
      label   = "Years included",
      min     = 2011,
      max     = 2023,
      value   = c(2011, 2023),
      step    = 1,
      sep     = ""
    ),
    
    checkboxGroupInput(
      inputId = "obesity_behaviors",
      label   = "Behavioral measures to display",
      choices = c(
        "150+ min Activity",
        "Activity + Strengthening",
        ">300 min Activity",
        "Strengthening 2+ Days",
        "No Physical Activity"
      ),
      selected = c(
        "150+ min Activity",
        "Activity + Strengthening",
        ">300 min Activity",
        "Strengthening 2+ Days",
        "No Physical Activity"
      )
    )
  ),
  conditionalPanel(
    condition = "input.sidebarItemSelected == 'mental'",
    
    selectInput(
      inputId = "end_date",
      label = "Select Time Period End Date",
      choices = NULL,   # populated in server
      selected = NULL
    ),
    
    selectInput(
      inputId = "indicator",
      label = "Select Indicator",
      choices = NULL,   # populated in server
      selected = NULL
    ),
    
    selectInput(
      inputId = "indicator2",
      label = "Select Second Indicator",
      choices = NULL,   # populated in server
      selected = NULL
    )
  )
)

# Dashboard Body
body <- dashboardBody(
  tabItems(
    
    # ---- Tab 1: Global Cancer Mortality ----
    tabItem(tabName = "mortality",
            div(
              h4("Global cancer mortality patterns by type, region, and age group, and how they change over time"),
              style = "text-align: center; margin-bottom: 15px;"
            ),
            fluidRow(
              # Info Box 1
              infoBoxOutput(width = 3, "infoYears"),
              
              # Info Box 2
              infoBoxOutput(width = 3, "infoDRFirst"),
              
              # Info Box 3
              infoBoxOutput(width = 3, "infoDRLast"),
              
              # Info Box 4
              infoBoxOutput(width = 3, "change")
            ),
            
            
            fluidRow(
              # Plot 1 (First row, First col)
              column(width = 6,
                     box(title = tags$div(textOutput("title1"),
                                          style = "font-size:14.5px;"), width = 12,
                         plotOutput("cancerTypePlot", height = "300px"))
              ),
              # Plot 2 (First row, Second col)
              column(width = 6,
                     box(title = tags$div(textOutput("title2"),
                                          style = "font-size:14.5px;"), width = 12,
                         plotOutput("regionPlot", height = "300px"))
              )
            ),
            
            fluidRow(
              # Plot 3 (Second row, First col)
              column(width = 6,
                     box(
                       title = tags$div(textOutput("title3"),
                                        style = "font-size:14px;"), width = 12,
                       plotOutput("ageHistPlot", height = "300px"))
              ),
              # Plot 4 (Second row, Second col)
              column(width = 6,
                     box(
                       title = tags$div(textOutput("title4"),
                                        style = "font-size:14px;"), width = 12,
                       plotOutput("divergingPlot", height = "300px")
                     )
              )
            ),
            
            fluidRow(
              # Plot 5 (Spans entire third row)
              column(width = 12,
                     box(title = tags$div(textOutput("title5"),
                                          style = "font-size:14.5px;"), width = 12,
                         leafletOutput("mapPlot", height = "425px"))
              )
            )
    ),
    
    # ---- Tab 2: Heart Disease Indicators ----
    tabItem(tabName = "heart",
            div(
              style = "text-align: center; margin-bottom: 25px;",
              HTML("
                <h4 style='font-size: 20px; margin-bottom: 10px;'>
                  <strong>I am evaluating how demographic factors as well as lifestyle choices correlate with heart disease risk.</strong>
                </h4>
            
                <ul style='display: inline-block; text-align: left; font-size: 15px; line-height: 1.5; margin-top: 5px;'>
                  <li>
                    A <strong><u><em>smoker</em></u> </strong> is classified as someone who smoked at least 100 cigarettes in their lifetime 
                    <strong>and</strong> currently smokes either every day or some days
                  </li>
                  <br>
                  <li>
                    A  <strong><u><em>drinker</em></u></strong> is classified as someone who has more than 7 drinks per week
                  </li>
                </ul>
                ")
            ),
            
            fluidRow(
              # Plot 1 (First row, First col)
              column(width = 6,
                     box(width = 12,
                         plotOutput("heart_race_sex_plot", height = "300px"))
              ),
              # Plot 2 (Second row, Second col)
              column(width = 6,
                     box(width = 12,
                         plotOutput("heart_smoking_drinking_plot", height = "300px"))
              )
            ),
            
            fluidRow(
              # Plot 3 (Second row, First col)
              column(width = 12,
                     box(width = 12,
                         plotOutput("heart_bmi_plot", height = "300px"))
              )
            ),
    ),
    
    # ---- Tab 3: Obesity Risk Factors ----
    tabItem(tabName = "obesity",
            div(
              h4("Tracking Obesity and Health Behaviors in the United States"),
              style = "text-align: center; margin-bottom: 20px;"
            ),
            
            # Info boxes showing state comparison summary
            fluidRow(
              infoBoxOutput(width = 4, "obesity_info_state1"),
              infoBoxOutput(width = 4, "obesity_info_state2"),
              infoBoxOutput(width = 4, "obesity_info_diff")
            ),
            
            # First row: US Map
            fluidRow(
              column(width = 12,
                     box(title = "Adult Obesity Rates by U.S. State", 
                         width = 12,
                         leafletOutput("obesity_map", height = "400px"))
              )
            ),
            
            # Second row: State comparison plots
            fluidRow(
              column(width = 6,
                     box(title = textOutput("obesity_state1_title"), 
                         width = 12,
                         plotOutput("obesity_state1_plot", height = "350px"))
              ),
              column(width = 6,
                     box(title = textOutput("obesity_state2_title"), 
                         width = 12,
                         plotOutput("obesity_state2_plot", height = "350px"))
              )
            ),
            
            # Third row: Obesity and overweight trends comparison
            fluidRow(
              column(width = 12,
                     box(title = "Obesity and Overweight Trends Comparison", 
                         width = 12,
                         plotOutput("obesity_weight_comparison", height = "350px"))
              )
            )
    ),
    
    tabItem(
      tabName = "mental",
      fluidRow(
        column(
          width = 12,
          tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 10px;"),
          tags$h3("Single Indicator:", style = "font-weight: bold; margin-left: 5px;")
        )
      ),
      
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
      
      fluidRow(
        column(
          width = 12,
          box(
            title = tags$div(
              "Indicator Trend Over Time (United States)",
              style = "font-size:15px; font-weight:bold;"
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("mh_indicator_trend", height = "350px")
          )
        )
      ),
      
      
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
      ),
      
      fluidRow(
        column(
          width = 12,
          tags$hr(style = "border-top: 2px solid #ccc; margin-top: 20px; margin-bottom: 10px;"),
          tags$h3("Comparison with Second Indicator:", style = "font-weight: bold; margin-left: 5px;")
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          box(
            title = tags$div(
              "Indicator Trend Comparison Over Time (United States)",
              style = "font-size:15px; font-weight:bold;"
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("mh_indicator_trend_compare", height = "350px")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          box(
            title = tags$div(
              "Differences by Sex – Comparison of Indicators (United States)",
              style = "font-size:14px; font-weight:bold; white-space:normal;"
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("mh_sex_compare", height = "320px")
          )
        ),
        column(
          width = 6,
          box(
            title = tags$div(
              "Differences by Race/Ethnicity – Comparison of Indicators (United States)",
              style = "font-size:14px; font-weight:bold; white-space:normal;"
            ),
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("mh_race_compare", height = "320px")
          )
        )
      )
      
      
    )
  )
)

# Dashboard Output
dashboardPage(
  header,
  sidebar,
  body
)