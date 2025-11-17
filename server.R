library(tidyverse)
library(maps)
library(scales)    # percent_format
library(stringr)   # str_wrap

data <- readr::read_csv("Mental_Health_Care_in_the_Last_4_Weeks.csv")
data$Value <- data$Value / 100

# Convert end date to Date class
data <- data %>%
  mutate(TimeEnd = as.Date(`Time Period End Date`, format = "%m/%d/%Y"))

# Base state-level data for map (no date filter yet)
states_base <- data %>%
  filter(
    State != "United States",
    Group == "By State"
  ) %>%
  mutate(lower_state = tolower(State))

states_map <- map_data("state")

server <- function(input, output, session) {
  
  # ---- Populate indicator dropdown ----
  observe({
    inds <- sort(unique(states_base$Indicator))
    
    updateSelectInput(
      session,
      "indicator",
      choices  = inds,
      selected = if (length(inds) > 0) inds[1] else NULL
    )
  })
  
  # ---- Populate end_date dropdown from data ----
  observe({
    dates <- sort(unique(data$TimeEnd))
    
    updateSelectInput(
      session,
      "end_date",
      choices  = as.character(dates),           # values returned as character
      selected = as.character(max(dates))
    )
  })
  
  # Helper: selected date as Date object
  selected_date <- reactive({
    req(input$end_date)
    as.Date(input$end_date)
  })
  
  # -------- Map: By State, filtered by indicator + date --------
  states_filtered <- reactive({
    req(input$indicator, selected_date())
    
    states_base %>%
      filter(
        Indicator == input$indicator,
        TimeEnd   == selected_date()
      )
  })
  
  output$mh_map <- renderPlot({
    df <- states_filtered()
    
    merged <- states_map %>%
      dplyr::left_join(df, by = c("region" = "lower_state"))
    
    wrapped_title <- str_wrap(input$indicator, width = 45)
    
    ggplot(merged, aes(long, lat, group = group, fill = Value)) +
      geom_polygon(color = "white") +
      coord_fixed(1.3) +
      scale_fill_gradient(
        low = "lightblue",
        high = "darkblue",
        na.value = "grey90"
      ) +
      labs(
        fill  = "Decimal Percent",
        title = wrapped_title
      ) +
      theme_void()
  })
  
  # -------- Bar chart: By Sex (United States), filtered by date --------
  us_by_sex <- reactive({
    req(input$indicator, selected_date())
    
    data %>%
      filter(
        State   == "United States",
        Group   == "By Sex",
        TimeEnd == selected_date(),
        Indicator == input$indicator
      )
  })
  
  output$mh_sex_bar <- renderPlot({
    df <- us_by_sex()
    
    validate(
      need(nrow(df) > 0, "No data available for this indicator (By Sex).")
    )
    
    ggplot(df, aes(x = Subgroup, y = Value, fill = Subgroup)) +
      geom_col() +
      scale_fill_manual(values = c('Male' = 'lightblue', 'Female' = 'pink')) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        x = NULL,
        y = "Percent"
      ) +
      guides(fill = "none") +
      theme_minimal(base_size = 12)
  })
  
  # -------- Bar chart: By Race/Ethnicity (United States), filtered by date --------
  us_by_race <- reactive({
    req(input$indicator, selected_date())
    
    data %>%
      filter(
        State   == "United States",
        Group   == "By Race/Hispanic ethnicity",
        TimeEnd == selected_date(),
        Indicator == input$indicator
      ) %>%
      group_by(Subgroup) %>%
      summarise(
        Value = mean(Value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::drop_na(Value)
  })
  
  output$mh_race_bar <- renderPlot({
    df <- us_by_race()
    
    validate(
      need(nrow(df) > 0, "No data available for this indicator (By Race/Ethnicity).")
    )
    
    ggplot(df, aes(x = reorder(Subgroup, Value), y = Value)) +
      geom_col(fill = 'steelblue') +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        x = NULL,
        y = "Percent"
      ) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 10)
      )
  })
}
