# loading obesity data and cleaning it
obesity_df <- read.csv("NationalObesity.csv") %>%
  rename(State = NAME, ObesityRate = Obesity)

obesity_risk_raw <- read.csv("BehavioralRiskForObesity.csv")
obesity_risk <- obesity_risk_raw[
  obesity_risk_raw$StratificationCategory1 == "Total" &
    obesity_risk_raw$Stratification1 == "Total",
]

# renaming for shorter labels 
long_questions <- c(
  "Percent of adults aged 18 years and older who have obesity",
  "Percent of adults aged 18 years and older who have an overweight classification",
  "Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)",
  "Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic physical activity (or an equivalent combination) and engage in muscle-strengthening activities on 2 or more days a week",
  "Percent of adults who achieve more than 300 minutes a week of moderate-intensity aerobic physical activity or 150 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)",
  "Percent of adults who engage in muscle-strengthening activities on 2 or more days a week",
  "Percent of adults who engage in no leisure-time physical activity",
  "Percent of adults who report consuming fruit less than one time daily",
  "Percent of adults who report consuming vegetables less than one time daily"
)

short_labels <- c(
  "Adults with Obesity",
  "Adults Overweight",
  "150+ min Activity",
  "Activity + Strengthening",
  ">300 min Activity",
  "Strengthening 2+ Days",
  "No Physical Activity",
  "Low Fruit Intake",
  "Low Vegetable Intake"
)

m <- match(obesity_risk$Question, long_questions)

obesity_risk$ShortQuestion <- ifelse(
  is.na(m),
  obesity_risk$Question,
  short_labels[m]
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