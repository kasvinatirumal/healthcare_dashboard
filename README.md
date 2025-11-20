<h1> Our Group Decided to Focus on Healthcare </h1>

- We are approaching healthcare through different lenses
- The dashboard shows cancer rates, heart disease, obesity, and mental health data

<h3> Looking more closely, here are the guiding questions for each section </h3>

<h2> 1.&nbsp; Global Cancer Mortality (Kasvina) </h2>

  **The questions I want to answer are:**
  
  For the period 2003 - 2015
  - How did cancer mortality rates change over time by cancer type and region?
  - How did mortality rates vary by cancer type and age group within a given year?
  - Which countries experienced the smallest and largest changes in mortality rates, by region and cancer type?

  **Dataset**
  - Sourced from the WHO Mortality Database, with separate datasets downloaded for five cancer types. The links to each dataset are as follows: [Colon Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/colon-and-rectum-cancers), [Pancreas Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/pancreas-cancer), [Lung Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/trachea-bronchus-lung-cancers), [Breast Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/breast-cancer), [Prostate Cancer](https://platform.who.int/mortality/themes/theme-details/topics/indicator-groups/indicator-group-details/MDB/prostate-cancer)
  - To download any of the five datasets directly from the WHO website, open the corresponding link, click “Export Data”, select “Full Data”, then hit the "Export" button. Alternatively, all five datasets are included in the repository under `cancer_datasets.zip`.
  - Each dataset contains 12 variables per observation, including (but not limited to) region, country, year, gender, age group, number of cases, and mortality rate per 100,000 population.
  - All five datasets were cleaned and merged into `cancer_all_df.Rdata` using the script `cancer_preprocess_data.R`.
  - The resulting cancer_all_df.Rdata file is used for all visualizations in Tab 1 of the dashboard.
    
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

<h2> 3.&nbsp; Obesity </h2>

<h2> 4.&nbsp; Mental Health </h2>
