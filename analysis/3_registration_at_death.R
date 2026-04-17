###################################################
# Author: Martina Pesce / Andrea Schaffer
# Bennett Institute for Applied Data Science
# University of Oxford, 2025
#
# 2) Registration at time of death
#
# Describe registration status at the reference date of death
# and the timing of registration start/end relative to death,
# by death source (ONS only / TPP only / Both) and year.
#
# Inclusion criteria:
# - valid age
# - non-disclosive sex
# - death recorded in ONS and/or TPP
#
# Exclusion criteria:
# - implausible death date in either source
#
# Reference date of death: ONS death date, where available; otherwise TPP death date
###################################################

# Libraries ----
library(tidyverse)
library(here)
library(fs)

# Create output directory ----
output_dir_analysis_tables <- here("output", "analysis_tables")
dir_create(output_dir_analysis_tables)

# Import utility functions ----
source(here("analysis", "0_utility_functions.R"))

# Import data ----
death_registration_processed <- read_csv(
  here("output", "highly_sensitive", "death_registration_processed.csv.gz")
)

# Restrict to patients with any death date and no implausible death dates ----
death_registration_clean <- death_registration_processed |>
  filter(
    flag_any_date_death == TRUE,
    flag_any_date_death_implausible == FALSE
  )

# Registration status at death, by year and death source ----
registration_status_source <- death_registration_clean |>
  group_by(death_date_ref_year, death_source, registration_status) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  ) |>
  arrange(death_date_ref_year, death_source, registration_status)

write_csv(
  registration_status_source,
  here(output_dir_analysis_tables, "registration_status_source.csv")
)

# Timing of last registration start relative to death, by year and death source ----
# "death_before_registration_start" indicates last registration started after death
reg_start_timing_source <- death_registration_clean |>
  group_by(death_date_ref_year, death_source, reg_start_timing_group) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  ) |>
  arrange(death_date_ref_year, death_source, reg_start_timing_group)

write_csv(
  reg_start_timing_source,
  here(output_dir_analysis_tables, "reg_start_timing_source.csv")
)

# Timing of last registration end relative to death, by year and death source ----
# Exclude people whose registration started after death
reg_end_timing_source <- death_registration_clean |>
  filter(
    reg_start_timing_group %in% c(
      "same_day_as_registration_start",
      "death_after_registration_start"
    )
  ) |>
  group_by(death_date_ref_year, death_source, reg_end_timing_group) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  ) |>
  arrange(death_date_ref_year, death_source, reg_end_timing_group)

write_csv(
  reg_end_timing_source,
  here(output_dir_analysis_tables, "reg_end_timing_source.csv")
)