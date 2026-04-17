###################################################
# Author: Martina Pesce / Andrea Schaffer
#   Bennett Institute for Applied Data Science
#   University of Oxford, 2025
#
# This script describe the relationship between death date and GP registration history
#
# Steps:
# 1) classify deaths by source: ONS only / TPP only / both
# 2) define whether death occurred during registration
#    - main analysis: includes 30-day grace period after registration end
#    - sensitivity analysis: no grace period
# 3) among registered deaths, describe timing between:
#    - registration start and death
#    - death and registration end
#
# Outputs:
###################################################

# Libraries ----

library(tidyverse)
library(lubridate)
library(here)
library(fs)

# Create output directories ----

output_dir_hs <- here("output", "highly_sensitive")
dir_create(output_dir_hs)

output_dir_analysis_tables <- here("output", "analysis_tables")
dir_create(output_dir_analysis_tables)

# import custom functions ----
source(here("analysis", "0_utility_functions.R"))

# Import data ----

dataset_death_raw <- read_csv(
  here("output", "highly_sensitive", "dataset_death_TPP_ONS.csv.gz")
) |>
  mutate(
    tpp_death_date = as.Date(tpp_death_date),
    ons_death_date = as.Date(ons_death_date),
    date_of_birth = as.Date(date_of_birth),
    last_registration_start_date = as.Date(last_registration_start_date),
    last_registration_end_date = as.Date(last_registration_end_date),
    region = as.factor(region),
    ethnicity = as.factor(ethnicity),
    sex = as.factor(sex)
  )


# Derive core variables ----
death_registration_processed <- dataset_death_raw |>
  mutate(
    # ONS or TPP death date
    has_ons_date_death = !is.na(ons_death_date),
    has_tpp_death_date = !is.na(tpp_death_date),
    flag_any_date_death = has_ons_date_death | has_tpp_death_date, 
    
    # Plausibility of death dates
    cat_ons_death_date = cat_implausible_death_date(ons_death_date, date_of_birth),
    cat_tpp_death_date = cat_implausible_death_date(tpp_death_date, date_of_birth),
    flag_any_date_death_implausible = 
      !(cat_ons_death_date %in% c("ok", "missing")) |
      !(cat_tpp_death_date %in% c("ok", "missing")),
    
    # Death source
    death_source = cat_death_source(ons_death_date, tpp_death_date),
    
    # Use ONS death date where available, otherwise TPP
    death_date_ref = as.Date(if_else(!is.na(ons_death_date), ons_death_date, tpp_death_date)),
    death_date_ref_year = year(death_date_ref),
    
    # Registration timing
    death_minus_reg_start = as.numeric(death_date_ref - last_registration_start_date),
    reg_end_minus_death = as.numeric(last_registration_end_date - death_date_ref),
    
    registration_status = cat_registration_status(
      death_date = death_date_ref,
      reg_start = last_registration_start_date,
      reg_end = last_registration_end_date,
      grace_days = 30
    ),
    
    # Registered flags
    flag_is_registered = registration_status %in% c( 
      "death_during_registration",
      "death_during_registration_open_end"
    ),
    is_registered_within_grace= registration_status %in% c(
      "death_during_registration",
      "death_during_registration_open_end",
      "death_after_deregistration_within_grace"
    ),
    
    # Timing groups
    reg_start_timing_group = cat_last_reg_start_to_death(death_minus_reg_start),
    reg_end_timing_group = cat_last_reg_end_minus_death(reg_end_minus_death)
  ) |>
  add_tpp_date_or_coded_vars() |>
  add_dod_diff_vars() |>
  add_demographic_vars()
  

write_csv(death_registration_processed, here(output_dir_hs, "death_registration_processed.csv.gz"))


# print details about dataset
capture.output(
  skimr::skim_without_charts(death_registration_processed),
  file = fs::path(output_dir_analysis_tables, "death_registration_processed_skim.txt"),
  split = FALSE
)

# raw table source by year (no filter)
table_source_raw <- death_registration_processed |>
  
  # Keep individuals with any recorded death date 
  # (exclude patients with death only recorded in TPP codes)
  filter(flag_any_date_death == TRUE) |>
  
  group_by(death_date_ref_year, death_source) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  )|>
  group_by(death_date_ref_year) |>
  mutate(
    total_year = sum(total, na.rm = TRUE),    
    perc = total / total_year * 100          
  ) |>
  ungroup()

write_csv(table_source_raw, here(output_dir_analysis_tables, "table_source_raw.csv"))

#  end------

