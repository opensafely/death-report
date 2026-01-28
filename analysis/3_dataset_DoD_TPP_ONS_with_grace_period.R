# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")


## Create output directory
output_dir <- here("output", "highly_sensitive")
# fs::dir_create(output_dir)

# Import processed data ----
dataset_death_TPP_ONS <- read_csv("output/highly_sensitive/dataset_death_TPP_ONS.csv.gz") %>% 
  mutate(
    TPP_death_date = as.Date(TPP_death_date),
    ons_death_date = as.Date(ons_death_date),
    date_of_birth = as.Date(date_of_birth),
    age_band = as.factor(age_band),
    practice = as.factor(practice),
    ons_death_place = as.factor(ons_death_place),
    region = as.factor(region),
    IMD_q10 = as.factor(IMD_q10),
    ethnicity = as.factor(ethnicity),
    sex = as.factor(sex),
    last_registration_start_date = as.Date(last_registration_start_date),
    last_registration_end_date = as.Date(last_registration_end_date)
  ) %>% 
  filter(
    has_registration == TRUE & # was registered at the beginning of the year the person died
    any_death_during_study == TRUE # died between "2009-01-01" - "2025-06-06" + (deregistration date + 30 days) is after one date of death
    )


# -----------------------
# Create variables
DoD_TPP_ONS_with_grace_period <- dataset_death_TPP_ONS %>%
  mutate(
    death_dereg_diff_TPP = case_when(
      !is.na(last_registration_end_date) ~ as.Date(last_registration_end_date) - TPP_death_date,
      TRUE ~ as.difftime(NA_real_, units = "days"
      )),
    death_dereg_diff_ONS = case_when(
      !is.na(last_registration_end_date) ~ as.Date(last_registration_end_date) - ons_death_date,
      TRUE ~ as.difftime(NA_real_, units = "days"
      )),
    DoD_min = pmin(TPP_death_date, ons_death_date, na.rm = TRUE),
    diff_DoD = TPP_death_date - ons_death_date,
    TPP_death = case_when(!is.na(TPP_death_date) ~ "yes",
                          TRUE ~ NA_character_),
    ONS_death = case_when(!is.na(ons_death_date) ~ "yes",
                          TRUE ~ NA_character_),
    rural_urb_recode = case_when(
      rural_urban < 5 ~ "urban",
      rural_urban >= 5 ~ "rural",
      TRUE ~ NA_character_
    ),
    year_pref_ONS = if_else(!is.na(ons_death_date), year(ons_death_date), year(TPP_death_date)),
    year_month_pref_ONS = if_else(!is.na(ons_death_date), format(ons_death_date, "%Y-%m"), format(TPP_death_date, "%Y-%m")
    )
  ) %>%
  mutate(
    ONS_or_TPP = case_when(
      !is.na(ONS_death) & !is.na(TPP_death) ~ "ONS & TPP",
      !is.na(ONS_death) & is.na(TPP_death) ~ "ONS",
      !is.na(TPP_death) & is.na(ONS_death) ~ "TPP",
      is.na(TPP_death) & is.na(ONS_death) ~ NA_character_
    ),
    DoD_groups = case_when(
      diff_DoD == 0 ~ "0",
      
      diff_DoD >= 1 & diff_DoD <= 7 ~ "1-7",
      diff_DoD >= 8 & diff_DoD <= 31 ~ "8-31",
      diff_DoD >= 32 ~ "32+",
      
      diff_DoD <= -1 & diff_DoD >= -7 ~ "-1 to -7",
      diff_DoD <= -8 & diff_DoD >= -31 ~ "-8 to -31",
      diff_DoD <= -32 ~ "-32+",      
      
      TRUE ~ NA_character_
    ),
    DoD_dereg_tpp = case_when(
      death_dereg_diff_TPP == 0 ~ "0",
      
      death_dereg_diff_TPP >= 1 & death_dereg_diff_TPP <= 7 ~ "1-7",
      death_dereg_diff_TPP >= 8 & death_dereg_diff_TPP <= 31 ~ "8-31",
      death_dereg_diff_TPP >= 32 ~ "32+",
      
      death_dereg_diff_TPP <= -1 & death_dereg_diff_TPP >= -7 ~ "-1 to -7",
      death_dereg_diff_TPP <= -8 & death_dereg_diff_TPP >= -31 ~ "-8 to -31",
      death_dereg_diff_TPP <= -32 ~ "-32+",      
      
      TRUE ~ NA_character_
    ),
    DoD_dereg_TPP_group = case_when(
      death_dereg_diff_TPP == 0 ~ "0",
      
      death_dereg_diff_TPP >= 1 & death_dereg_diff_TPP <= 7 ~ "1-7",
      death_dereg_diff_TPP >= 8 & death_dereg_diff_TPP <= 31 ~ "8-31",
      death_dereg_diff_TPP >= 32 ~ "32+",
      
      death_dereg_diff_TPP <= -1 & death_dereg_diff_TPP >= -7 ~ "-1 to -7",
      death_dereg_diff_TPP <= -8 & death_dereg_diff_TPP >= -31 ~ "-8 to -31",
      death_dereg_diff_TPP <= -32 ~ "-32+",      
      
      TRUE ~ NA_character_
    ),  
    DoD_dereg_ONS_group = case_when(
      death_dereg_diff_ONS == 0 ~ "0",
      
      death_dereg_diff_ONS >= 1 & death_dereg_diff_ONS <= 7 ~ "1-7",
      death_dereg_diff_ONS >= 8 & death_dereg_diff_ONS <= 31 ~ "8-31",
      death_dereg_diff_ONS >= 32 ~ "32+",
      
      death_dereg_diff_ONS <= -1 & death_dereg_diff_ONS >= -7 ~ "-1 to -7",
      death_dereg_diff_ONS <= -8 & death_dereg_diff_ONS >= -31 ~ "-8 to -31",
      death_dereg_diff_ONS <= -32 ~ "-32+",      
      
      TRUE ~ NA_character_
    )  
  ) # %>%
  #filter(
   # death_dereg_diff_ONS >= 0 | death_dereg_diff_TPP >= 0
  #)
  
write.csv(DoD_TPP_ONS_with_grace_period, here::here("output", "highly_sensitive", "DoD_TPP_ONS_with_grace_period.csv"))

