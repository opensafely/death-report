# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")


## Create output directory
output_dir <- here("output", "report")
fs::dir_create(output_dir)

# Import processed data ----
dataset0 <- read_csv("output/dataset_death_date_diff.csv.gz") %>% 
  mutate(
    TPP_death_date = as.Date(TPP_death_date),
    ons_death_date = as.Date(ons_death_date),
    date_of_birth = as.Date(date_of_birth),
    age_band = as.factor(age_band),
    practice = as.factor(practice),
    ons_death_place = as.factor(ons_death_place),
    region = as.factor(region),
    IMD_q10 = as.factor(IMD_q10),
    ethnicity = as.factor(ethnicity)
    )
  

# Create variables
DoD_diff_dataset <- dataset0 %>% 
  mutate(
    DoD_min = pmin(TPP_death_date, ons_death_date, na.rm = TRUE),
    diff_DoD = TPP_death_date - ons_death_date,
    TPP_death = case_when(
      !is.na(TPP_death_date) ~ "yes",
      TRUE ~ NA_character_
    ),
    ONS_death = case_when(
      !is.na(ons_death_date) ~ "yes",
      TRUE ~ NA_character_
    ),
    rural_urb_recode = case_when(
      rural_urban < 5 ~ "urban",
      rural_urban >= 5 ~ "rural",
      TRUE ~ NA_character_
    ),
    ONS_year = year(ons_death_date)
  ) %>%
  mutate(
    ONS_or_TPP = case_when(
      !is.na(ONS_death) & !is.na(TPP_death) ~ "ONS & TPP",
      !is.na(ONS_death) & is.na(TPP_death) ~ "ONS",
      !is.na(TPP_death) & is.na(ONS_death) ~ "TPP",
      is.na(TPP_death) & is.na(ONS_death) ~ NA_character_
    )
  )


##### Diff DoD

# Table key indicators by year
table_DoD_detailed <- DoD_diff_dataset %>%
  filter(ONS_or_TPP == "ONS & TPP") %>%
  group_by(
    ONS_year
  ) %>% 
  mutate(
    total = n()
  ) %>% 
  ungroup() %>% 
  group_by(ONS_year, diff_DoD, total) %>%
  summarise(count_by_DoD = n())


#Date diff by group
summarise_DoD_by_group <- function(data, group_var) {
  data %>%
    filter(ONS_or_TPP == "ONS & TPP") %>%
    group_by(ONS_year, {{ group_var }}) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    group_by(ONS_year, {{ group_var }}, diff_DoD, total) %>%
    summarise(count_by_DoD = n(), .groups = "drop")
}

#tables by group

DoD_by_age <- summarise_DoD_by_group(DoD_diff_dataset, age_band)

DoD_by_rural_practice <- summarise_DoD_by_group(DoD_diff_dataset, practice)

DoD_by_ons_death_place <- summarise_DoD_by_group(DoD_diff_dataset, ons_death_place)

DoD_by_region <- summarise_DoD_by_group(DoD_diff_dataset, region)

DoD_by_rural_urban <- summarise_DoD_by_group(DoD_diff_dataset, rural_urban)

DoD_by_IMD_q10 <- summarise_DoD_by_group(DoD_diff_dataset, IMD_q10)

DoD_by_ethnicity <- summarise_DoD_by_group(DoD_diff_dataset, ethnicity)


