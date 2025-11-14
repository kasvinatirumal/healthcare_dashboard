# Server

library(tidyverse)
library(dplyr)

# Load the data
load("cancer_all_df.RData")

# heart disease data preprocessing
source("heart_preprocess_data.R")

server <- function(input, output) {
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
}