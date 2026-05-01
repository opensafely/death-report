###################################################
# Author: Martina Pesce / Andrea Schaffer
# Bennett Institute for Applied Data Science
# University of Oxford, 2025
#
# 4) Agreement between ONS and TPP death dates
# Assess agreement between date of death recorded in ONS and TPP
# among individuals with a death date available in both sources.
#
# Inclusion criteria:
# - valid age
# - non-disclosive sex
# - registered at death
# - death date recorded in both ONS and TPP
#
# Exclusion criteria:
# - implausible death date in either source
###################################################

# Libraries ----
library(tidyverse)
library(here)
library(fs)
library(lubridate)

# Create output directory ----
output_dir_analysis_tables <- here("output", "analysis_tables")
dir_create(output_dir_analysis_tables)

# Import utility functions ----
source(here("analysis", "0_utility_functions.R"))

# Import data ----
death_registration_processed <- read_csv(
  here("output", "highly_sensitive", "death_registration_processed.csv.gz")
)

# ==================================================
# Analysis
# ==================================================

# Restrict to the main analysis population ----
ons_tpp_dates_diff_analysis <- death_registration_processed |>
  filter(
    has_ons_date_death == TRUE,
    has_tpp_death_date == TRUE,
    flag_any_date_death_implausible == FALSE,
    flag_is_registered == TRUE
  )

# table dates difference bw ons - tpp
table_ons_tpp_dates_diff_overall  <- ons_tpp_dates_diff_analysis |>
  group_by(death_date_ref_year, dod_diff_groups) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  ) |>
  mutate(
    subgroup = "overall",
    subgroup_value = "All"
  )

# Counts by subgroup ----
table_ons_tpp_dates_diff_subgroups <- ons_tpp_dates_diff_analysis |>
  select(
    death_date_ref_year,
    dod_diff_groups,
    age_band,
    sex,
    ethnicity,
    imd_quintile,
    rural_urban,
    region
  ) |>
  pivot_longer(
    cols = c(age_band, sex, ethnicity, imd_quintile, rural_urban, region),
    names_to = "subgroup",
    values_to = "subgroup_value"
  ) |>
  group_by(death_date_ref_year, dod_diff_groups, subgroup, subgroup_value) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  )

# Combine overall and subgroup counts ----
table_ons_tpp_dates_diff <- bind_rows(
  table_ons_tpp_dates_diff_overall,
  table_ons_tpp_dates_diff_subgroups
) |>
  mutate(
    subgroup = factor(
      subgroup,
      levels = c(
        "overall",
        "age_band",
        "sex",
        "ethnicity",
        "imd_quintile",
        "rural_urban",
        "region"
      )
    )
  ) |>
  group_by(death_date_ref_year, subgroup, subgroup_value) |>
  mutate(
    total_subgroup_value = sum(total, na.rm = TRUE),             # denominator
    perc = total / total_subgroup_value * 100           # proportion
  ) |>
  ungroup() |>
  arrange(death_date_ref_year, subgroup, subgroup_value, dod_diff_groups) |>
  select(
    death_date_ref_year,
    subgroup,
    subgroup_value,
    dod_diff_groups,
    total,
    total_subgroup_value,
    perc
  )

# Export main analysis table ----
write_csv(
  table_ons_tpp_dates_diff,
  here(output_dir_analysis_tables, "table_ons_tpp_dates_diff.csv")
)
