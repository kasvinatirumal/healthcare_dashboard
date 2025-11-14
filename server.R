# Server

library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(ggrepel)
library(leaflet)

# Load the data
load("cancer_all_df.RData")

function(input, output) {
  theData = reactive({
    cancer_all_df %>%
      # We use the output of the year slider to filter the years
      filter(year >= input$year[1], year <= input$year[2])
  })
  
  # Produce the reactive widget
  output$yearSelectorUI = renderUI({
    # Years available in the filtered dataset
    selectedYears <- sort(unique(theData()$year))
    
    # Selector widget with default as first year
    selectInput(
      inputId = "yearSelector",
      label = "Select Year",
      choices = selectedYears,
      selected = input$year[1]
    )
  })
  
  # Produce the first info box
  output$infoYears = renderInfoBox({
    infoBox(
      "Years", # Title
      input$year[2] - input$year[1], # Value
      icon = icon("calendar", lib = "font-awesome"), # Icon
      color = "blue", # Color
      # Condition for filling
      fill = ifelse(input$year[2] - input$year[1] < 10,
                    TRUE, FALSE)
    )
  })
  
  # Filter dataset for second and third info box
  infoData = reactive({
    theData() %>%
      filter(
        age_group_broad == "all",
        region == input$region,
        cancer_type == input$cancer
      )
  })
  
  # Get average value for second info box
  infoDRFirstValue <- reactive({
    infoData() %>%
      filter(year == input$year[1]) %>%
      summarise(mean_rate = mean(mean_death_rate, na.rm = TRUE)) %>%
      pull(mean_rate) %>%
      round(1)
  })
  
  # Get average value for third info box
  infoDRLastValue <- reactive({
    infoData() %>%
      filter(year == input$year[2]) %>%
      summarise(mean_rate = mean(mean_death_rate, na.rm = TRUE)) %>%
      pull(mean_rate) %>%
      round(1)
  })
  
  
  # Produce the second info box
  output$infoDRFirst <- renderInfoBox({
    value <- infoDRFirstValue()
    infoBox(
      paste0("Avg D. Rate (", input$year[1], ")"),
      value,
      icon = icon("flag"),
      color = "purple"
    )
  })
  
  # Produce the third info box
  output$infoDRLast <- renderInfoBox({
    value <- infoDRLastValue()
    infoBox(
      paste0("Avg D. Rate (", input$year[2], ")"),
      value,
      icon = icon("flag-checkered"),
      color = "purple"
    )
  })
  
  # Produce the fourth info box
  output$change <- renderInfoBox({
    change <- round(infoDRLastValue() - infoDRFirstValue(),1)
    infoBox(
      paste0("Change in D. Rate"),
      ifelse(change > 0, paste0("+", round(change, 2)), round(change, 2)),
      icon = icon(ifelse(change >= 0, "arrow-up", "arrow-down")),
      color = ifelse(change >= 0, "red", "green")
    )
  })
  
  # Produce the first reactive title 
  output$title1 <- renderText({
    paste0("Average Cancer Mortality Rate by Cancer Type (", input$region, ", ", 
           input$year[1], "-", input$year[2],")")
  })
  
  # Produce the second reactive title
  output$title2 <- renderText({
    paste0("Regional Trends in Cancer Mortality Rate (", input$cancer, " Cancer, ",
           input$year[1], "-", input$year[2],")")
  })
  
  # Produce the third reactive title
  output$title3 <- renderText({
    paste0("Average Mortality Rate by Age Group (", input$region, ", ", input$cancer,
           " Cancer, ", input$yearSelector, ")")
  })
  
  # Produce the fourth reactive title
  output$title4 <- renderText({
    paste0("Countries with Largest Change in Mortality Rates (", input$cancer, " Cancer, ",
           input$year[1], "-", input$year[2],")")
  })
  
  # Produce the fifth reactive title
  output$title5 <- renderText({
    paste0("Worldwide Change in Mortality Rates (", input$cancer, " Cancer, ",
           input$year[1], "-", input$year[2],")")
  })
  
  # Line plot showing cancer mortality by cancer type
  output$cancerTypePlot <- renderPlot({
    plot_data <- theData()
    
    plot_data_trend <- plot_data %>%
      filter(age_group_broad == "all", region == input$region) %>%
      group_by(year, cancer_type) %>%
      summarise(
        death_rate = mean(mean_death_rate, na.rm = TRUE),
        .groups = "drop"
      )
    
    label_points_trend <- plot_data_trend %>%
      group_by(cancer_type) %>%
      filter(year == max(year)) %>%
      ungroup()
    
    max_rate <- max(plot_data_trend$death_rate, na.rm = TRUE)
    palette <- brewer.pal(n = 5, name = "Set2")
    cancer_colors <- c(
      "Pancreas"    = palette[1],
      "Colon"      = palette[2],
      "Lung"       = palette[3],
      "Prostate"   = palette[4],
      "Breast"     = "#B8860B",
      "All" = "#777777"
    )
    
    p1 <- ggplot(plot_data_trend, aes(x = year, y = death_rate, color = cancer_type)) +
      geom_line(linewidth = 1, aes(linetype = cancer_type)) +
      geom_text_repel(
        data = label_points_trend,
        aes(label = cancer_type),
        nudge_x = 0.6,
        hjust = 0,
        direction = "y",
        segment.color = NA
      ) +
      scale_color_manual(values = cancer_colors) +
      scale_linetype_manual(values = c("dashed", rep("solid", 5))) +
      scale_x_continuous(breaks = seq(input$year[1], input$year[2], by = 1)) +
      scale_y_continuous(limits = c(0, max_rate)) +
      labs(
        x = "Year",
        y = "Average Death Rate (per 100,000 population)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(element_blank()),
        legend.position = "none",
        axis.text.x = element_text(size=10)
      )
    print(p1)
  })
  
  # Get colors for regions
  continent_colors <- c(
    "Asia" = "#ff7f0e",
    "Europe" = "#1f77b4",
    "Africa" = "#2ca02c",
    "North America" = "#d62728",
    "South America" = "#9467bd",
    "Oceania" = "#e377c2",
    "World" = "#777777"
  )
  
  # Line plot showing cancer mortality by region
  output$regionPlot <- renderPlot({
    cancer_data <- theData()
    
    plot_trend_region <- cancer_data %>%
      filter(age_group_broad == "all", cancer_type == input$cancer) %>%
      group_by(year, region) %>%
      summarise(
        death_rate = mean(mean_death_rate, na.rm = TRUE),
        .groups = "drop"
      )
    
    label_trend_region <- plot_trend_region %>%
      group_by(region) %>%
      filter(year == max(year)) %>%
      ungroup()
    
    plot_trend_region$region <- factor(plot_trend_region$region)
    max_rate <- max(plot_trend_region$death_rate, na.rm = TRUE)
    
    p2 <- ggplot(plot_trend_region, aes(x = year, y = death_rate, color = region)) +
      geom_line(linewidth = 1, aes(linetype = region)) +
      geom_text_repel(
        data = label_trend_region,
        aes(label = region),
        nudge_y = -0.75,
        nudge_x = 0.6,
        hjust = 0,
        direction = "y",
        segment.color = NA
      ) +
      scale_color_manual(values = continent_colors) +
      scale_linetype_manual(values = c(rep("solid", 6), "dashed")) +
      scale_x_continuous(breaks = seq(input$year[1], input$year[2], by = 1)) +
      scale_y_continuous(limits = c(0, max_rate)) +
      labs(
        x = "Year",
        y = "Average Death Rate (per 100,000 population)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size=10)
      )
    print(p2)
  })
  
  
  # Histogram showing average mortality rate by age group
  output$ageHistPlot <- renderPlot({
    req(input$yearSelector)
    
    hist_plot_data <- theData() %>%
      filter(region == input$region,
             cancer_type == input$cancer,
             year == input$yearSelector,
             !age_group_broad %in% c("all", "unknown")
      ) %>%
      group_by(age_group_broad) %>%
      summarise(death_rate = mean(mean_death_rate), .groups = "drop")
    
    
    p3 <- ggplot(hist_plot_data, aes(x = age_group_broad, y = death_rate)) +
      geom_col(color="black",fill = continent_colors[[input$region]]) +
      geom_text(aes(label = round(death_rate, 1)),
                vjust = -0.5,
                size = 4) +
      labs(
        x = "Age group",
        y = "Death rate per 100,000 population"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(size=10, angle = 45, hjust = 1)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  
    
    print(p3)
  })
  
  
  # Diverging bar plot showing change in mortality rates
  output$divergingPlot <- renderPlot({
    cancer_change <- theData() %>%
      filter(cancer_type == input$cancer,
             age_group_broad == "all") %>%
      select(region, country, year, mean_death_rate)
    
    cancer_change_wide <- cancer_change %>%
      pivot_wider(names_from = year, values_from = mean_death_rate, names_prefix = "yr_") %>%
      mutate(change = .data[[paste0("yr_", input$year[2])]] - .data[[paste0("yr_", input$year[1])]])
    
    top_increases <- cancer_change_wide %>%
      arrange(desc(change)) %>%
      slice_head(n = 10)
    
    top_decreases <- cancer_change_wide %>%
      arrange(change) %>%
      slice_head(n = 10) %>%
      arrange(desc(change))
    
    top_changes <- bind_rows(
      top_increases %>% mutate(direction = "Increase"),
      top_decreases %>% mutate(direction = "Decrease")
    ) %>%
      mutate(
        country = factor(country, levels = unique(country)),
        region = factor(region, levels = unique(region))
      )
    
    max_change <- max(top_changes$change, na.rm = TRUE)
    min_change <- min(top_changes$change, na.rm = TRUE)
    
    # Makes nice breaks for y-axis automatically
    breaks_seq <- pretty(c(min_change, max_change), n = 5)
    
    # Ensure zero is included if it’s in the data range
    if(min_change < 0 & !0 %in% breaks_seq) {
      breaks_seq <- sort(c(breaks_seq, 0))
    }
    
    p4 <- ggplot(
      top_changes %>% mutate(country = str_wrap(country, width = 20)),
      aes(x = reorder(country, change), y = change, fill = region)
    ) +
      geom_col(color="black", width = 0.85) +
      geom_text(
        aes(
          label = ifelse(change > 0,
                         sprintf("+%.1f", change),
                         sprintf("%.1f", change)),
          hjust = ifelse(change > 0, -0.2, 1.2)
        ),
        size = 3
      ) +
      coord_flip() +
      scale_fill_manual(values = continent_colors) +
      labs(
        y = paste(
          "Change in Death Rate (per 100,000 population)"
        ),
        fill = "Region"
      ) +
      scale_y_continuous(
        breaks = breaks_seq,
        limits = c(min(breaks_seq)-2, max(breaks_seq)+1),
        expand = expansion(mult = c(0, 0))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),  
        axis.title.x = element_text(size = 10),  
        axis.title.y = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = 9),
        legend.text  = element_text(size = 9),
        legend.key.size = unit(0.9, "lines")
      )
    print(p4)
  })
  
  # Map plot showing worldwide change in cancer mortality rates
  output$mapPlot <- renderLeaflet({
    cancer_change <- theData() %>%
      filter(region != "World",
             cancer_type == input$cancer,
             age_group_broad == "all") %>%
      select(region, country, year, mean_death_rate, longitude, latitude)
    
    cancer_change_wide <- cancer_change %>%
      pivot_wider(names_from = year, values_from = mean_death_rate, names_prefix = "yr_") %>%
      mutate(change = .data[[paste0("yr_", input$year[2])]] - .data[[paste0("yr_", input$year[1])]])
    
    cancer_change_wide %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addCircles(
        lng = ~ longitude,
        lat = ~ latitude,
        weight = 1,
        radius = ~ abs(change) * 75000,
        color = ~ifelse(change > 0, "red", "green"),   
        fillColor = ~ifelse(change > 0, "red", "green"),  
        fillOpacity = 0.4,
        popup = ~ paste(
          country,
          "<br>Change:",
          ifelse(change > 0, paste0("+", round(change, 2)), round(change, 2)),
          "<br>Type:", input$cancer
        )
      )
  })
}