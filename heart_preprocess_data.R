library(tidyverse)

heart_2022 <- read_csv("heart_2022.csv")

heart_temp <- heart_2022 %>%
  mutate(Race = recode(Race,
                       "American Indian/Alaskan Native" = "Native American"))

heart_temp <- heart_temp %>%
  mutate(
    BehaviorGroup = case_when(
      Smoking == "No"  & AlcoholDrinking == "No"  ~ "Neither",
      Smoking == "Yes" & AlcoholDrinking == "No"  ~ "Smoker Only",
      Smoking == "No"  & AlcoholDrinking == "Yes" ~ "Drinker Only",
      TRUE ~ "Both"
    ),
    BehaviorGroup = factor(BehaviorGroup,
                           levels = c("Neither", "Smoker Only", "Drinker Only", "Both"))
  )