# Server

library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(ggrepel)
library(leaflet)
library(sf)
library(tigris)

# Load the cancer data
load("cancer_all_df.RData")

# heart disease data preprocessing
source("heart_preprocess_data.R")

# -------- processing obesity data to make it cleaner --------
# loading obesity data and change column name
obesity_df <- read.csv("NationalObesity.csv") %>%
  rename(State = NAME, ObesityRate = Obesity)

obesity_risk_raw <- read.csv("BehavioralRiskForObesity.csv")
obesity_risk <- obesity_risk_raw[
  obesity_risk_raw$StratificationCategory1 == "Total" &
    obesity_risk_raw$Stratification1 == "Total",
]

# renaming for shorter labels 
obesity_risk <- obesity_risk %>%
  mutate(
    ShortQuestion = dplyr::case_when(
      Question == "Percent of adults aged 18 years and older who have obesity" ~ "Adults with Obesity",
      Question == "Percent of adults aged 18 years and older who have an overweight classification" ~ "Adults Overweight",
      Question == "Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)" ~ "150+ min Activity",
      Question == "Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic physical activity (or an equivalent combination) and engage in muscle-strengthening activities on 2 or more days a week" ~ "Activity + Strengthening",
      Question == "Percent of adults who achieve more than 300 minutes a week of moderate-intensity aerobic physical activity or 150 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)" ~ ">300 min Activity",
      Question == "Percent of adults who engage in muscle-strengthening activities on 2 or more days a week" ~ "Strengthening 2+ Days",
      Question == "Percent of adults who engage in no leisure-time physical activity" ~ "No Physical Activity",
      Question == "Percent of adults who report consuming fruit less than one time daily" ~ "Low Fruit Intake",
      Question == "Percent of adults who report consuming vegetables less than one time daily" ~ "Low Vegetable Intake",
      TRUE ~ Question
    )
  )

options(tigris_use_cache = TRUE, tigris_class = "sf")
states_sf <- tigris::states(cb = TRUE, year = 2023)

# dropping territories
states_sf <- states_sf[!(states_sf$STATEFP %in% c("60", "66", "69", "78")), ]
states_sf <- states_sf[, c("STATEFP", "NAME", "geometry")]
states_sf <- sf::st_transform(states_sf, 4326)

# join obesity rates
obesity_map_sf <- merge(
  x   = states_sf,
  y   = obesity_df,
  by.x = "NAME",
  by.y = "State",
  all.x = TRUE,
  sort = FALSE
)

# Getting a single obesity percentage per state
state_obesity_rate <- function(state_name) {
  obesity_df %>%
    filter(State == state_name) %>%
    summarise(rate = mean(ObesityRate, na.rm = TRUE)) %>%
    pull(rate)
}

# Getting behavior data per state, dropping some vals that are not accurate
get_state_behavior_data <- function(state_name) {
  obesity_risk %>%
    filter(
      LocationDesc == state_name,
      !ShortQuestion %in% c("Low Fruit Intake", "Low Vegetable Intake")
    ) %>%
    group_by(YearStart, ShortQuestion) %>%
    summarise(
      mean_value = mean(Data_Value, na.rm = TRUE),
      .groups = "drop"
    )
}

# ------------------------------------------------------------------- #
server <- function(input, output) {
  # ---- Tab 1: Global Cancer Mortality ----
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
  
  # ---- Tab 2: Heart Disease Indicators ----
  # race/sex plot
  output$heart_race_sex_plot <- renderPlot({
    df <- heart_temp %>%
      filter(
        Race %in% input$race,
        Sex %in% input$sex,
      )
    
    race_order <- df %>%
      group_by(Race) %>%
      summarize(avg_rate = mean(HeartDisease == "Yes")) %>%
      arrange(avg_rate) %>%
      pull(Race)
    
    heart_summary <- df %>%
      mutate(Race = factor(Race, levels = race_order)) %>%
      group_by(Race, Sex) %>%
      summarize(
        HeartDiseaseRate = mean(HeartDisease == "Yes") * 100,
        .groups = "drop"
      )
    
    ggplot(heart_summary, aes(x = Race, y = HeartDiseaseRate, fill = Sex)) +
      geom_col(position = "dodge") +
      labs(
        title = "Heart Disease Rate by Race and Sex",
        subtitle = "Looking Across Demographic Groups",
        x = "Race",
        y = "Heart Disease Rate (%)",
        fill = "Sex"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(b=20)),
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20), size=15),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 20), size = 10),
        axis.title = element_text(face = "bold", size = 15)
      ) +
      scale_fill_manual(values = c("Male" = "blue", "Female" = "red"))
  })
  
  # smoking/drinking plot
  output$heart_smoking_drinking_plot <- renderPlot({
    df <- heart_temp %>%
      filter(
        BehaviorGroup %in% input$drinking_smoking
      )
    
    heart_behav <- df %>%
      group_by(BehaviorGroup) %>%
      summarize(HeartDiseaseRate = mean(HeartDisease == "Yes") * 100) %>%
      arrange(HeartDiseaseRate) %>%  
      mutate(BehaviorGroup = factor(BehaviorGroup, levels = BehaviorGroup))
    
    ggplot(heart_behav, aes(x = BehaviorGroup, y = HeartDiseaseRate, fill = BehaviorGroup)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", HeartDiseaseRate)), vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Heart Disease Rate by Smoking/Drinking Status",
        x = "Lifestyle Group",
        y = "Heart Disease Rate (%)",
        fill = "Status"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(b = 30)),
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20), size = 15),
        axis.title = element_text(face = "bold", size = 15)
      ) +
      scale_fill_manual(values = c(
        "Neither" = "green",
        "Smoker Only" = "blue",
        "Drinker Only" = "purple",
        "Both" = "red"
      ))
  })
  
  # bmi plot
  output$heart_bmi_plot <- renderPlot({
    
    df <- heart_temp %>%
      filter(
        BMI >= input$bmi[1],
        BMI <= input$bmi[2]
      )
    
    df <- df %>%
      mutate(
        BMI_bin = cut(
          BMI,
          breaks = seq(floor(input$bmi[1]), ceiling(input$bmi[2]), by = 5),
          include.lowest = TRUE,
          right = FALSE
        )
      )
    
    bmi_summary <- df %>%
      group_by(BMI_bin) %>%
      summarize(
        HeartDiseaseRate = mean(HeartDisease == "Yes") * 100,
        .groups = "drop") %>%
      filter(!is.na(BMI_bin))  
    
    ggplot(bmi_summary, aes(x = BMI_bin, y = HeartDiseaseRate, fill = BMI_bin)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = sprintf("%.1f%%", HeartDiseaseRate)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Heart Disease Rate by BMI",
        subtitle = "< 18.5 BMI is underweight, > 25 is overweight",
        x = "BMI Range",
        y = "Heart Disease Rate (%)",
        fill = "BMI Bin"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(b = 30)),
        plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 20), size = 15),
        plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 20), size = 10),
        axis.title = element_text(face = "bold", size = 15)
      ) +
      scale_fill_brewer(palette = "Oranges")
  })
  
  # ---- Tab 3: Obesity Risk Factors ----
  
  # obesity summary state 1
  output$obesity_info_state1 <- renderInfoBox({
    req(input$obesity_state1)
    rate1 <- state_obesity_rate(input$obesity_state1)
    infoBox(
      title = input$obesity_state1,
      value = ifelse(is.na(rate1), "No data",
                     paste0(round(rate1, 1), "% obese")),
      subtitle = "Adult obesity prevalence",
      icon = icon("user"),
      color = "navy"
    )
  })
  
  # obesity summary state 2
  output$obesity_info_state2 <- renderInfoBox({
    req(input$obesity_state2)
    rate2 <- state_obesity_rate(input$obesity_state2)
    infoBox(
      title = input$obesity_state2,
      value = ifelse(is.na(rate2), "No data",
                     paste0(round(rate2, 1), "% obese")),
      subtitle = "Adult obesity prevalence",
      icon = icon("user"),
      color = "teal"
    )
  })
  
  # obesity summary percentage point difference
  output$obesity_info_diff <- renderInfoBox({
    req(input$obesity_state1, input$obesity_state2)
    rate1 <- state_obesity_rate(input$obesity_state1)
    rate2 <- state_obesity_rate(input$obesity_state2)
    diff_val <- rate2 - rate1
    
    infoBox(
      title = "Difference (State 2 - State 1)",
      value = ifelse(any(is.na(c(rate1, rate2))), "No data",
                     paste0(ifelse(diff_val >= 0, "+", ""), round(diff_val, 1), " pp")),
      subtitle = "Percentage point difference in obesity rate",
      icon = icon(ifelse(diff_val >= 0, "arrow-up", "arrow-down")),
      color = ifelse(diff_val > 0, "red", "green")
    )
  })
  
  # U.S. obesity map
  output$obesity_map <- renderLeaflet({
    map_data <- obesity_map_sf %>%
      filter(NAME != "Puerto Rico")
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_data$ObesityRate,
      na.color = "grey90"
    )
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(ObesityRate),
        weight = 0.5,
        color = "white",
        fillOpacity = 0.8,
        smoothFactor = 0.2,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(NAME, ": ", round(ObesityRate, 1), "%"),
        popup = ~paste0(
          "<strong>", NAME, "</strong><br>",
          "Adult Obesity: ", round(ObesityRate, 1), "%"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~ObesityRate,
        title = "Adult Obesity (%)",
        opacity = 0.8
      ) %>%
      setView(
        lng = -120,
        lat = 42,
        zoom = 3.4
      )
  })
  
  state1_behavior <- reactive({
    req(input$obesity_state1)
    df <- get_state_behavior_data(input$obesity_state1)
    
    if (!is.null(input$obesity_years)) {
      df <- df[df$YearStart >= input$obesity_years[1] &
                 df$YearStart <= input$obesity_years[2], ]
    }
    df
  })
  
  state2_behavior <- reactive({
    req(input$obesity_state2)
    df <- get_state_behavior_data(input$obesity_state2)
    
    if (!is.null(input$obesity_years)) {
      df <- df[df$YearStart >= input$obesity_years[1] &
                 df$YearStart <= input$obesity_years[2], ]
    }
    df
  })
  
  # titles for state plots
  output$obesity_state1_title <- renderText({
    paste0("Behavioral Trends in ", input$obesity_state1)
  })
  
  output$obesity_state2_title <- renderText({
    paste0("Behavioral Trends in ", input$obesity_state2)
  })
  
  # behavioral trends plot for State 1
  output$obesity_state1_plot <- renderPlot({
    df <- state1_behavior()
    
    df <- df[!(df$ShortQuestion %in% c("Adults with Obesity", "Adults Overweight")), ]
    
    validate(
      need(length(input$obesity_behaviors) > 0,
           "Please select at least one behavior to display.")
    )
    
    df <- df[df$ShortQuestion %in% input$obesity_behaviors, ]
    
    validate(need(nrow(df) > 0, "No data available for this combination."))
    
    ggplot(df, aes(x = YearStart, y = mean_value, color = ShortQuestion)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 1.3) +
      labs(
        x = "Year",
        y = "Percent of Adults",
        color = "Behavior"
      ) +
      expand_limits(y = 0) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right")
  })
  
  # behavior trends plot for State 2
  output$obesity_state2_plot <- renderPlot({
    df <- state2_behavior()
    
    df <- df[!(df$ShortQuestion %in% c("Adults with Obesity", "Adults Overweight")), ]
    
    validate(
      need(length(input$obesity_behaviors) > 0,
           "Please select at least one behavior to display.")
    )
    
    df <- df[df$ShortQuestion %in% input$obesity_behaviors, ]
    
    validate(need(nrow(df) > 0, "No data available for this combination."))
    
    ggplot(df, aes(x = YearStart, y = mean_value, color = ShortQuestion)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 1.3) +
      labs(
        x = "Year",
        y = "Percent of Adults",
        color = "Behavior"
      ) +
      expand_limits(y = 0) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right")
  })
  
  # obesity and overweight trends comparison (for both states)
  output$obesity_weight_comparison <- renderPlot({
    df1 <- state1_behavior() %>%
      filter(ShortQuestion %in% c("Adults with Obesity", "Adults Overweight")) %>%
      mutate(State = input$obesity_state1)
    
    df2 <- state2_behavior() %>%
      filter(ShortQuestion %in% c("Adults with Obesity", "Adults Overweight")) %>%
      mutate(State = input$obesity_state2)
    
    df_all <- bind_rows(df1, df2)
    validate(need(nrow(df_all) > 0, "No obesity/overweight data available for selected states."))
    
    ggplot(df_all, aes(
      x = YearStart,
      y = mean_value,
      color = ShortQuestion,
      linetype = State,
      shape = State
    )) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.2, fill = "white") +
      scale_linetype_manual(values = setNames(
        c("solid", "twodash"),
        c(input$obesity_state1, input$obesity_state2)
      )) +
      scale_shape_manual(values = setNames(
        c(16, 23),
        c(input$obesity_state1, input$obesity_state2)
      )) +
      labs(
        x = "Year",
        y = "Percent of Adults",
        color = "Measure",
        linetype = "State",
        shape = "State"
      ) +
      expand_limits(y = 0) +
      theme_minimal(base_size = 13)
  })
}