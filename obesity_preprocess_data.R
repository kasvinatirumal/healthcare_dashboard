# loading obesity data and cleaning it
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