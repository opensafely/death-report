###################################################
# Author: Martina Pesce / Andrea Schaffer
# Bennett Institute for Applied Data Science
# University of Oxford, 2025
#
# 3) Death source comparison
#
#
# Create descriptive tables of death source classifications:
# - by reference year and death source
# - overall and by subgroup
# - for deaths occurring in 2025 onwards, by month
#
# Analysis:
# 1. Counts by reference year and death source
#    (overall and by subgroup: age band, sex, ethnicity,
#    IMD quintile, rural/urban classification, and region)
# 2. Monthly counts by death source for deaths in 2025 onwards
# 3. Descriptive summaries including TPP-coded deaths
#########################################################################

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
# Main analysis: dated deaths only
# ==================================================

# Restrict to the main analysis population ----
death_registration_analysis <- death_registration_processed |>
  filter(
    flag_any_date_death == TRUE,
    flag_any_date_death_implausible == FALSE,
    flag_is_registered == TRUE
  )

# Overall counts by year and death source ----
table_death_source_overall <- death_registration_analysis |>
  group_by(death_date_ref_year, death_source) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  ) |>
  mutate(
    subgroup = "overall",
    subgroup_value = "All"
  )

# Counts by subgroup ----
table_death_source_subgroups <- death_registration_analysis |>
  select(
    death_date_ref_year,
    death_source,
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
  group_by(death_date_ref_year, death_source, subgroup, subgroup_value) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  )

# Combine overall and subgroup counts ----
table_death_source <- bind_rows(
  table_death_source_overall,
  table_death_source_subgroups
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
  arrange(death_date_ref_year, subgroup, subgroup_value, death_source) |>
  select(
    death_date_ref_year,
    subgroup,
    subgroup_value,
    death_source,
    total,
    total_subgroup_value,
    perc
  )

# Export main analysis table ----
write_csv(
  table_death_source,
  here(output_dir_analysis_tables, "table_death_source.csv")
)

# ==================================================
# 2025-2026 sources by month
# ==================================================
table_death_source_25_26 <- death_registration_analysis |>
  filter(death_date_ref_year > 2024) |>
  group_by(month = floor_date(death_date_ref, unit = "month"), death_source) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  ) 

# Export lasts months analysis table ----
write_csv(
  table_death_source_25_26,
  here(output_dir_analysis_tables, "table_death_source_25_26.csv")
)

# ==================================================
# Sensitivity analysis: include TPP-coded deaths
# ==================================================

# Descriptive table:
# classify TPP evidence as date, code, or neither
# across all records with a reference year including TPP-coded deaths
# Note: tpp_date_or_coded = NA indicates no TPP code / date , only ONS
tpp_death_code_or_date <- death_registration_processed |>
  group_by(death_date_ref_year_w_tpp_codes, tpp_date_or_coded) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  ) |>
  arrange(death_date_ref_year_w_tpp_codes, tpp_date_or_coded)

write_csv(
  tpp_death_code_or_date,
  here(output_dir_analysis_tables, "tpp_death_code_or_date.csv")
)

# Restrict to the main analysis population ----

# Overall counts by year and death source, including TPP-coded deaths ----
table_death_source_overall_any_tpp <- death_registration_analysis |>
  group_by(death_date_ref_year_w_tpp_codes, death_source_tpp_date_or_coded) |>
  summarise(
    total = rounding(n()),  # SDC rounding
    .groups = "drop"
  )|>
  group_by(death_date_ref_year_w_tpp_codes) |>
  mutate(
    total_year = sum(total, na.rm = TRUE),            
    perc = total / total_year * 100          
  ) |>
  ungroup() |>
  arrange(death_date_ref_year_w_tpp_codes, death_source_tpp_date_or_coded) |>
  select(
    death_date_ref_year_w_tpp_codes,
    death_source_tpp_date_or_coded,
    total,
    total_year,
    perc
  )

# Export sensitivity analysis table ----
write_csv(
  table_death_source_overall_any_tpp,
  here(output_dir_analysis_tables, "table_death_source_overall_any_tpp.csv")
)

