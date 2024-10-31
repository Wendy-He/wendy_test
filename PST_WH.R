
# Load libraries
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)
library(viridis)
library(lubridate)

setwd("/Users/wendyyy/Documents/CHAMPS_Epi Work/Original Code")
AnalyticData <- read.csv("vw_lk_Analytics_Dataset_2024-09-18_12-12-47.csv", stringsAsFactors=FALSE, na.strings=c("",".","NA"))
TacData <- read.csv("vw_lk_TacLaboratoryResult_Pivot_2024-07-30_10-58-07.csv", stringsAsFactors=FALSE, na.strings=c("",".","NA"))


AnalyticData <-AnalyticData %>%
  filter(MITS_flag==1) %>%
  filter(publication_suspended!=1 & !is.na(publication_suspended)) %>%
  filter(sensitive_case!=1 & !is.na(sensitive_case)) %>%
  filter(M00060==1) %>%
  filter(site_name != "Nigeria")



# To split the infant age group, here is the steps:
# Filter the data for the CH00718(infant) age group.
# Calculate the age of each infant in days from calc_dob to calc_dod.
# Split the filtered data into two subgroups: infants aged 28 days to less than 6 months and those aged 6 months to less than 12 months.

# Convert calc_dob and calc_dod to Date format
AnalyticData$calc_dob <- as.Date(AnalyticData$calc_dob, format="%m/%d/%y")
AnalyticData$calc_dod <- as.Date(AnalyticData$calc_dod, format="%m/%d/%y")

head(AnalyticData$calc_dob)
head(AnalyticData$calc_dod) 



AnalyticData <- AnalyticData %>%
  mutate(age_in_days = as.numeric(difftime(calc_dod, calc_dob, units = "days")),
         
         # Create infant subgroup for age group CH00718
         infant_subgroup = case_when(
           age_group == "CH00718" & age_in_days >= 28 & age_in_days < 182 ~ "28 days to <6 months",
           age_group == "CH00718" & age_in_days >= 182 & age_in_days < 365 ~ "6 months to <12 months",
           TRUE ~ NA_character_
         ),
         
         # Create early neonate subgroup for age group CH01405
         early_neonate_subgroup = case_when(
           age_group == "CH01405" & age_in_days >= 1 & age_in_days < 3 ~ "1 day to <3 days",
           age_group == "CH01405" & age_in_days >= 3 & age_in_days < 6 ~ "3 days to <6 days",
           TRUE ~ NA_character_
         ))

table(AnalyticData$infant_subgroup)
table(AnalyticData$early_neonate_subgroup)

```




```{r}
##### Task 2 #####

###### Then look at Staphylococcus aureus in different age group ######

vars_eti<-c("Underlying_Cause_Factor_etiol1","Underlying_Cause_Factor_etiol1_othr","Underlying_Cause_Factor_etiol2","Underlying_Cause_Factor_etiol2_othr",
            "Underlying_Cause_Factor_etiol3","Underlying_Cause_Factor_etiol3_othr","Immediate_Cause_of_Death_etiol1","Immediate_Cause_of_Death_etiol1_othr",
            "Immediate_Cause_of_Death_etiol2","Immediate_Cause_of_Death_etiol2_othr","Immediate_Cause_of_Death_etiol3","Immediate_Cause_of_Death_etiol3_othr",
            "Morbid_Condition_01_etiol1","Morbid_Condition_01_etiol1_othr","Morbid_Condition_01_etiol2","Morbid_Condition_01_etiol2_othr",
            "Morbid_Condition_01_etiol3","Morbid_Condition_01_etiol3_othr","Morbid_Condition_02_etiol1","Morbid_Condition_02_etiol1_othr",     
            "Morbid_Condition_02_etiol2","Morbid_Condition_02_etiol2_othr","Morbid_Condition_02_etiol3","Morbid_Condition_02_etiol3_othr",
            "Morbid_Condition_03_etiol1","Morbid_Condition_03_etiol1_othr","Morbid_Condition_03_etiol2","Morbid_Condition_03_etiol2_othr",
            "Morbid_Condition_03_etiol3","Morbid_Condition_03_etiol3_othr","Morbid_Condition_04_etiol1","Morbid_Condition_04_etiol1_othr",
            "Morbid_Condition_04_etiol2","Morbid_Condition_04_etiol2_othr","Morbid_Condition_04_etiol3","Morbid_Condition_04_etiol3_othr",
            "Morbid_Condition_05_etiol1","Morbid_Condition_05_etiol1_othr","Morbid_Condition_05_etiol2","Morbid_Condition_05_etiol2_othr",
            "Morbid_Condition_05_etiol3","Morbid_Condition_05_etiol3_othr","Morbid_Condition_06_etiol1","Morbid_Condition_06_etiol1_othr",
            "Morbid_Condition_06_etiol2","Morbid_Condition_06_etiol2_othr","Morbid_Condition_06_etiol3","Morbid_Condition_06_etiol3_othr",
            "Morbid_Condition_07_etiol1","Morbid_Condition_07_etiol1_othr","Morbid_Condition_07_etiol2","Morbid_Condition_07_etiol2_othr",
            "Morbid_Condition_07_etiol3","Morbid_Condition_07_etiol3_othr","Morbid_Condition_08_etiol1","Morbid_Condition_08_etiol1_othr",
            "Morbid_Condition_08_etiol2","Morbid_Condition_08_etiol2_othr","Morbid_Condition_08_etiol3","Morbid_Condition_08_etiol3_othr") 

countries_of_interest <- c("Bangladesh", "Ethiopia", "Kenya", "Mali", "Mozambique", "Sierra Leone", "South Africa")

staph_cases <- AnalyticData %>%
  filter(apply(AnalyticData[vars_eti], 1, function(row) any(grepl("Staphylococcus aureus", row, ignore.case = TRUE))))


# Calculate the total number of deaths for each age group from the original dataset
total_deaths_by_age <- AnalyticData %>%
  group_by(age_group) %>%
  summarise(Total_Deaths = n())  # Total number of deaths in each age group

print(total_deaths_by_age)

# Calculate the number of Staphylococcus aureus cases for each age group from the filtered staph_cases
staph_cases_by_age <- staph_cases %>%
  group_by(age_group) %>%
  summarise(Staph_Cases = n())  # Count of Staphylococcus aureus cases in each age group

print(staph_cases_by_age)

# Merge both summaries to calculate the percentage of Staphylococcus aureus deaths
age_group_summary <- left_join(total_deaths_by_age, staph_cases_by_age, by = "age_group") %>%
  mutate(
    Staph_Cases = replace_na(Staph_Cases, 0),  # Replace NA with 0
    Percentage_Staph = (Staph_Cases / Total_Deaths) * 100  # Calculate the percentage
  )

# Print the final summary table
print(age_group_summary) 


# CH00716 Stillbirth
# CH00718 Infant (28 days to less than 12 months)
# CH00719 Child (12 months to less than 60 Months)
# CH01404	Death in the first 24 hours
# CH01405	Early Neonate (1 to 6 days)
# CH01406	Late Neonate (7 to 27 days) 



#### Task 3 #####

# Filter Staphylococcus aureus cases in the causal chain 
staph_cases_ch <- AnalyticData %>%
  filter(apply(AnalyticData[vars_eti], 1, function(row) any(grepl("Staphylococcus aureus", row, ignore.case = TRUE))))

# Calculate the total number of deaths for each age group 
total_deaths_by_age <- AnalyticData %>%
  group_by(age_group) %>%
  summarise(Total_Deaths = n())  # Total number of deaths in each age group

# Step 3: Calculate the number of Staphylococcus aureus cases (causal chain) for each age group
staph_cases_by_age <- staph_cases_ch %>%
  group_by(age_group) %>%
  summarise(Staph_Cases_ch = n())  # Count of Staphylococcus aureus cases in each age group

# Pairing TacData and merging with AnalyticData to get age_group

TacData <- TacData %>%
  rename(Champsid = ChampsID)

# Filter TacData for relevant cases (S. aureus detected) and merge age_group from AnalyticData
Tac_filtered <- TacData %>%
  filter(Champsid %in% AnalyticData$Champsid) %>%
  left_join(select(AnalyticData, Champsid, age_group), by = "Champsid") %>%  
  mutate(S_aureus_detected = ifelse(bld_STAU_2 == "Positive" | CSF_STAU_2 == "Positive" | lung_STAU_2 == "Positive", 1, 0))

# Step 5: Summarise deaths with S. aureus detected by age group
s_aureus_detected <- Tac_filtered %>%
  group_by(age_group) %>%
  summarise(S_aureus_Detected_Deaths = sum(S_aureus_detected, na.rm = TRUE))

# Step 6: Merge summaries to calculate the percentage of Staphylococcus aureus deaths
age_group_summary <- left_join(total_deaths_by_age, staph_cases_by_age, by = "age_group") %>%
  left_join(s_aureus_detected, by = "age_group") %>%
  mutate(
    Staph_Cases_ch = replace_na(Staph_Cases_ch, 0),  # Replace NA with 0 for causal chain cases
    S_aureus_Detected_Deaths = replace_na(S_aureus_Detected_Deaths, 0),  # Replace NA with 0 for detected deaths
    Percentage_Staph = (Staph_Cases_ch / Total_Deaths) * 100  # Calculate percentage for Staph Cases in causal chain
  )

# Step 7: Print the final summary table
print(age_group_summary)




### Updated Task 4 ###

# Calculate total number of people in each age group
total_people_age_group <- AnalyticData %>%
  group_by(age_group) %>%
  summarise(total_people_age_group = n(), .groups = 'drop')

print(total_people_age_group)

# Calculate total number of people at each site
total_people_site <- AnalyticData %>%
  group_by(site_name) %>%
  summarise(total_people_site = n(), .groups = 'drop')

print(total_people_site)

# Filter Staph Cases
staph_cases <- AnalyticData %>%
  select(site_name, age_group) %>%
  filter(apply(AnalyticData[vars_eti], 1, function(row) any(grepl("Staphylococcus aureus", row, ignore.case = TRUE)))) %>%
  group_by(site_name, age_group) %>%
  summarise(Staph_Cases = n(), .groups = 'drop')

# Reshape the data so that sites become columns
staph_table <- staph_cases %>%
  pivot_wider(names_from = site_name, values_from = Staph_Cases, values_fill = list(Staph_Cases = 0)) 

# Merge age group totals with staph_table
staph_table <- left_join(staph_table, total_people_age_group, by = "age_group")

print(staph_table)

# Calculate total Staph cases at each site (column-wise totals)
site_staph_totals <- staph_table %>%
  summarise(across(-c(age_group, total_people_age_group), ~ sum(.x, na.rm = TRUE)))

print(site_staph_totals) 

# Reshape total people per site so they align as columns
total_people_site_vector <- total_people_site %>%
  pivot_wider(names_from = site_name, values_from = total_people_site, values_fill = list(total_people_site = 0))

print(total_people_site_vector)

# Add both Staph case totals and total people per site as rows
staph_table <- staph_table %>%
  bind_rows(
    tibble(age_group = "Total Staph Cases_loca", total_people_age_group = NA, site_staph_totals),
    tibble(age_group = "Total People_loca", total_people_age_group = NA, total_people_site_vector)
  )

# Calculate row-wise totals (percentage of Staph Cases in each age group)
staph_table <- staph_table %>%
  rowwise() %>%
  mutate(
    Total_case_age = sum(c_across(-c(age_group, total_people_age_group)), na.rm = TRUE),
    Row_Total_Percentage = paste0(Total_case_age, " (", round((Total_case_age / total_people_age_group) * 100, 1), "%)")
  ) %>%
  ungroup()

# Rearrange columns
staph_table <- staph_table %>%
  select(age_group, 
         all_of(country_columns),
         Row_Total_Percentage,      
         total_people_age_group,    
         everything())              


## calculate the column percentage ##

# Ensure we are focusing only on the country columns (first 9 columns)
country_columns <- c("Bangladesh", "Ethiopia", "Kenya", "Mali", "Mozambique", "Sierra Leone", "South Africa")

# Calculate total cases for each country
total_cases_loca <- staph_table %>%
  filter(age_group == "Total Staph Cases_loca") %>%
  select(all_of(country_columns))

# Calculate total people for each country
total_people_loca <- staph_table %>%
  filter(age_group == "Total People_loca") %>%
  select(all_of(country_columns))

# Initialize the new row for column percentage
column_percentage <- tibble(age_group = "Column Percentage")

# Iterate through each country column to calculate the percentage and format it with paste0
for (country in country_columns) {
  cases <- total_cases_loca[[country]]  # Get total cases for this country
  people <- total_people_loca[[country]]  # Get total people for this country
  
  # Calculate percentage and format it as "cases (percentage%)"
  percentage <- round((cases / people) * 100, 1)
  column_percentage[[country]] <- paste0(cases, " (", percentage, "%)")
}

# Convert the numeric columns of staph_table to character 
staph_table <- staph_table %>%
  mutate(across(all_of(country_columns), as.character))

# Add the 'column_percentage' row to the table
staph_table <- staph_table %>%
  filter(age_group != "Column Percentage") %>%  # Ensure no duplicate rows
  bind_rows(column_percentage)



# Print the final table
print(staph_table)

view(staph_table)









