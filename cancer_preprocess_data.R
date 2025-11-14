library(tidyverse)
library(maps)

# Function to read and clean cancer datasets
read_and_fix <- function(file, cancer_name) {
  read_csv(file, skip = 6, na = c("", "NA"), show_col_types = FALSE) %>%
    mutate(
      `Death rate per 100 000 population` = as.numeric(gsub(",", "", `Death rate per 100 000 population`)),
      Cancer = cancer_name
    )
}

# Load datasets for each cancer type
bc_data   <- read_and_fix("breast_cancer.csv", "Breast")
pros_data <- read_and_fix("prostate_cancer.csv", "Prostate")
panc_data <- read_and_fix("pancreas_cancer.csv", "Pancreas")
lung_data <- read_and_fix("lung_cancer.csv", "Lung")
colon_data<- read_and_fix("colon_cancer.csv", "Colon")

# Combine all cancer datasets into one dataset
combined_data <- bind_rows(bc_data, pros_data, panc_data, lung_data, colon_data)
combined_data <- combined_data %>%
  filter(Year >= 2003 & Year <= 2015)

num_years <- 2015 - 2003 + 1

# Get countries with full data for all cancers across all years
countries_with_complete_cancer <- combined_data %>%
  group_by(`Country Name`, Cancer) %>%
  summarise(n_years = n_distinct(Year), .groups = "drop") %>%
  filter(n_years == num_years)

# Keep countries that have data foe all cancer types across all years
complete_countries <- countries_with_complete_cancer %>%
  group_by(`Country Name`) %>%
  filter(n_distinct(Cancer) == n_distinct(combined_data$Cancer)) %>%
  pull(`Country Name`) %>%
  unique()

# Filter original dataset to only keep complete countries
balanced_data <- combined_data %>%
  filter(`Country Name` %in% complete_countries)

# Aggregate cancer death rates by country, cancer, year, and age group
fin_data <- balanced_data %>%
  filter(Sex == "All") %>% 
  group_by(`Region Name`, Sex, `Country Name`, Cancer, Year, `Age group code`) %>%
  summarise(
    `Death rate per 100 000 population` = sum(`Death rate per 100 000 population`, na.rm = TRUE),
    .groups = "drop"
    )

# Rename columns
colnames(fin_data) <- c("region", "sex", "country", "cancer_type", "year", "age_group", "death_rate")

# Rename inaccurate or inconsistent country and region names
fin_data <- fin_data %>%
  mutate(
    country = country %>%
      gsub("T\\?rkiye", "Turkey", .) %>%
      gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", .) %>%
      gsub("R\\?union", "Réunion", .) %>%
      gsub("China, Hong Kong SAR", "Hong Kong", .),
    
    region = region %>%
      gsub("North America and the Caribbean", "North America", .) %>%
      gsub("Central and South America", "South America", .)
  )

# Simplify age group labels
fin_data$age_group <- gsub("Age", "", fin_data$age_group)
fin_data$age_group <- gsub("_", "-", fin_data$age_group)
fin_data <- fin_data %>%
  mutate(age_group = gsub("00", "0", age_group))

# Group age groups into broader age categories
fin_data_age <- fin_data %>%
  mutate(age_group_broad = case_when(
    age_group %in% c("0") ~ "0",
    age_group %in% c("01-04", "05-09") ~ "1-9",
    age_group %in% c("10-14", "15-19") ~ "10-19",
    age_group %in% c("20-24", "25-29") ~ "20-29",
    age_group %in% c("30-34", "35-39") ~ "30-39",
    age_group %in% c("40-44", "45-49") ~ "40-49",
    age_group %in% c("50-54", "55-59") ~ "50-59",
    age_group %in% c("60-64", "65-69") ~ "60-69",
    age_group %in% c("70-74", "75-79") ~ "70-79",
    age_group %in% c("80-84", "85-over") ~ "80+",
    age_group %in% c("-unknown") ~ "unknown",
    age_group %in% c("-all") ~ "all",
    TRUE ~ NA_character_  # for any unmatched cases
  ))

# Compute mean death rate by the broader age groups
cancer_df <- fin_data_age %>%
  group_by(region, sex, country, cancer_type, year, age_group_broad) %>%
  summarise(
    mean_death_rate = mean(death_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(age_group_broad = as.factor(age_group_broad))

# Compute world-level averages and append to dataset
cancer_world_df <- cancer_df %>%
  group_by(sex, country = "World", cancer_type, year, age_group_broad) %>%
  summarise(
    mean_death_rate = mean(mean_death_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(region = "World") %>%
  bind_rows(cancer_df)

# Compute averages across all cancer types and append to dataset
cancer_all <- cancer_world_df %>%
  group_by(region, sex, country, year, age_group_broad) %>%
  summarise(
    mean_death_rate = mean(mean_death_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cancer_type = "All") %>%
  bind_rows(cancer_world_df)

# Rename countries to ensure consistency with map data
rename_map <- c(
  "Réunion" = "Reunion",
  "Czechia" = "Czech Republic",
  "United Kingdom" = "UK",
  "Antigua and Barbuda" = "Antigua",
  "Saint Kitts and Nevis" = "Saint Kitts",
  "Saint Vincent and the Grenadines" = "Saint Vincent",
  "Trinidad and Tobago" = "Trinidad",
  "United States of America" = "USA",
  "Brunei Darussalam" = "Brunei",
  "Republic of Korea" = "South Korea",
  "Republic of Moldova" = "Moldova",
  "Russian Federation" = "Russia",
  "Venezuela (Bolivarian Republic of)" = "Venezuela"
)

cancer_all$country <- recode(cancer_all$country, !!!rename_map)

# Get approximate coordinates for countries
world_coords <- map_data("world") %>%
  group_by(region) %>%
  summarise(
    longitude = mean(range(long), na.rm = TRUE),
    latitude = mean(range(lat), na.rm = TRUE)
  )

# Merge dataset with country coordinates
data_with_coords <- cancer_all %>%
  left_join(world_coords, by = c("country" = "region"))

# Manually adjust coordinates for certain countries
cancer_all_df <- data_with_coords %>%
  mutate(
    longitude = ifelse(country == "Hong Kong", 114.1694,
                ifelse(country == "USA", -98.35,
                ifelse(country == "South Africa", 22.9375,
                ifelse(country == "New Zealand", 174.8860,
                ifelse(country == "Chile", -71.5429,
                ifelse(country == "Ecuador", -78.1834,
                ifelse(country == "Malaysia", 101.9758,
                ifelse(country == "Norway", 8.4689,
                ifelse(country == "Sweden", 18.6435,
                longitude))))))))),
    latitude  = ifelse(country == "Hong Kong", 22.3193,
                ifelse(country == "USA", 39.50,
                ifelse(country == "South Africa", -30.5595,
                ifelse(country == "New Zealand", -40.9006,
                ifelse(country == "Chile", -35.6751,
                ifelse(country == "Ecuador", -1.8312,
                ifelse(country == "Malaysia", 4.2105,
                ifelse(country == "Norway", 60.4720,
                ifelse(country == "Sweden", 60.1282,
                latitude)))))))))
  )

# Save final processed dataset
save(cancer_all_df, file = "cancer_all_df.RData")