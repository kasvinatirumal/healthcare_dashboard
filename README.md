<h1> Our Group Decided to Focus on Healthcare </h1>

- We are approaching healthcare through different lenses
- The dashboard shows cancer mortality, heart disease, obesity, and mental health

<h3> Looking more closely, here are the guiding questions for each section </h3>

<h2> 1.&nbsp; Global Cancer Mortality (Kasvina) </h2>

  **The questions I want to answer are:**
  - How did mortality rates change between 2003 and 2015 by cancer type and region?
  - How are mortality rates distributed across age groups by cancer type in a given year?
  - Which countries experienced the smallest and largest changes in mortality rates from 2003 to 2015, by region and cancer type?

  **Dataset**
  - Sourced from the WHO Mortality Database, with separate datasets downloaded for five cancer types. The links to each dataset are as follows: [Colon Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/colon-and-rectum-cancers), [Pancreas Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/pancreas-cancer), [Lung Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/trachea-bronchus-lung-cancers), [Breast Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/breast-cancer), [Prostate Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/prostate-cancer)
  - To download any of the five datasets directly from the WHO website, click on the corresponding link, click “Export Data”, select “Full Data”, then hit the "Export" button
  - All five datasets are included in this repository under `cancer_datasets.zip`
  - Each dataset contains 12 variables per observation, including (but not limited to) region, country, year, gender, age group, number of cases, and mortality rate per 100,000 population
  - All five datasets were cleaned and merged into `cancer_all_df.Rdata` using the script `cancer_preprocess_data.R`
  - The resulting cancer_all_df.Rdata file is used for all visualizations within the "Global Cancer Mortality" Tab
    
<h2> 2.&nbsp; Heart Disease (Arnav) </h2>

  **The questions I want to answer are:**
  - Whether there is a difference in heart disease rate between different sexes, as well as different races.
  - Also, do a high rate of smokers/drinkers deal with heart disease?

 
  **Dataset**:
  - It comes from the Centers for Disease Control (CDC) and, more specifically, the Behavioral Risk Factor Surveillance System (BRFSS)
  - The data is gathered by interviewing over 400,000 adults about their health
  - You can find the cleaned-up version on [Kaggle](https://www.kaggle.com/datasets/kamilpytlak/personal-key-indicators-of-heart-disease)
  - The original data can be found on the [CDC website](https://www.cdc.gov/brfss/annual_data/annual_2022.html)
  - It contains 18 variables (columns) for each observation (interviewee), ranging from if they have heart disease to more descriptive characteristics such as if they drink alcohol, physical activity rate, mental health, as well as general health

<h2> 3.&nbsp; Obesity (Nicole) </h2>

  **The questions I want to answer are:**
  - How do obesity and health behavior rates differ across U.S. states?
  - What patterns emerge in obesity rates across the United States?
  - How have these behaviors changed over the last decade, and how do they relate to obesity levels?
 
  **Dataset**:
  - Both datasets used in the application comes from the CDC Behavioral Risk Factor Surveillance System. 
  - There are two datasets used:
  - One dataset maps out geographical obesity rates by state in 2015 [(NationalObesity)](https://catalog.data.gov/dataset/national-obesity-by-state-d765a)
    - It's mainly used to show a map of the states and their obesity rates in relation to the rest of the states
  - The other tracks adults and their physical activities by state through a period of time [(BehavioralRiskForObesity)](https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system/resource/0280bb9c-4de8-4b95-9642-93f727c4d305)
    - This is used to compare states throughout the years, and reports on various activities that people have done (2012-2023)
  - Download the datasets as CSV files and rename to the link names


<h2> 4.&nbsp; Mental Health (Maxx) </h2>

  **The questions I want to answer are:**
  - Broad:
    - How do different demographics experience mental health care differently?
  - Specific:
    - Do specific demographics (region, sex, or race/ethnicity) lack mental health treament when needed?
    - Do people receive prescription mediaction for mental health more or less than therapy or counseling?
   
  **Dataset**
  - The dataset used is from the U.S. Department of Health & Human Services
  - The dataset is an aggregated dataset broken down by several factors with the percentage value of the selected population that qualifies for that indicator.
    - Contains by age, sex, race/ethnicity, and state.
  - The dataset tracks whether an individual has received treatment for mental health.
    - Broken down by prescription medication and counseling/therapy.
