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
    total = n(), 
    .groups = "drop"
  )|>
  group_by(death_date_ref_year, death_source) |>
  mutate(
    total_year = rounding(sum(total, na.rm = TRUE)),    
    total = rounding(total),
    perc = round(total / total_year * 100, 1)          
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
    total = n(), 
    .groups = "drop"
  )|>
  group_by(death_date_ref_year, death_source) |>
  mutate(
    total_year = rounding(sum(total, na.rm = TRUE)),    
    total = rounding(total),
    perc = round(total / total_year * 100, 1)          
  ) |>
  arrange(death_date_ref_year, death_source, reg_start_timing_group)

write_csv(
  reg_start_timing_source,
  here(output_dir_analysis_tables, "reg_start_timing_source.csv")
)

# Timing of last registration end relative to death, by year and death source ----
# Exclude people whose registration started after death
# reg_end_timing_source <- death_registration_clean |>
#   filter(
#     reg_start_timing_group %in% c(
#       "same_day_as_registration_start",
#       "death_after_registration_start"
#     )
#   ) |>
#   group_by(death_date_ref_year, death_source, reg_end_timing_group) |>
#   summarise(
#     total = n(), 
#     .groups = "drop"
#   )|>
#   group_by(death_date_ref_year, death_source) |>
#   mutate(
#     total_year = rounding(sum(total, na.rm = TRUE)),    
#     total = rounding(total),
#     perc = round(total / total_year * 100, 1)          
#   ) |>
#   arrange(death_date_ref_year, death_source, reg_end_timing_group)

# write_csv(
#   reg_end_timing_source,
#   here(output_dir_analysis_tables, "reg_end_timing_source.csv")
# )


# Timing of last registration end relative to death,
# overall and by subgroup and year ----
# Exclude people whose registration started after death.
# Exclude people whose registration ended >28 days before death.
# Combine registration ending 1–28 days before death into one category
# before counting and applying SDC.

# Overall
reg_end_timing_overall <- death_registration_clean |>
  filter(
    death_date_ref_year >= 2020,
    reg_start_timing_group %in% c(
      "same_day_as_registration_start",
      "death_after_registration_start"
    ),
    reg_end_timing_group != "-29+"
  ) |>
  mutate(
    reg_end_timing_group = case_when(
      reg_end_timing_group %in% c(
        "-28 to -8",
        "-7 to -1"
      ) ~ "-28 to -1",
      TRUE ~ reg_end_timing_group
    )
  ) |>
  group_by(
    death_date_ref_year,
    death_source,
    reg_end_timing_group
  ) |>
  summarise(
    total = n(),
    .groups = "drop"
  ) |>
  mutate(
    subgroup = "overall",
    subgroup_value = "All"
  )


# Subgroups
reg_end_timing_subgroups <- death_registration_clean |>
  filter(
    death_date_ref_year >= 2020,
    reg_start_timing_group %in% c(
      "same_day_as_registration_start",
      "death_after_registration_start"
    ),
    reg_end_timing_group != "-29+"
  ) |>
  mutate(
    reg_end_timing_group = case_when(
      reg_end_timing_group %in% c(
        "-28 to -8",
        "-7 to -1"
      ) ~ "-28 to -1",
      TRUE ~ reg_end_timing_group
    )
  ) |>
  select(
    death_date_ref_year,
    death_source,
    reg_end_timing_group,
    age_band,
    sex,
    ethnicity,
    imd_quintile,
    rural_urban,
    region
  ) |>
  pivot_longer(
    cols = c(
      age_band,
      sex,
      ethnicity,
      imd_quintile,
      rural_urban,
      region
    ),
    names_to = "subgroup",
    values_to = "subgroup_value"
  ) |>
  group_by(
    death_date_ref_year,
    death_source,
    subgroup,
    subgroup_value,
    reg_end_timing_group
  ) |>
  summarise(
    total = n(),
    .groups = "drop"
  )


# Combine overall and subgroups
reg_end_timing_source_subgroups <- bind_rows(
  reg_end_timing_overall,
  reg_end_timing_subgroups
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
    ),
    death_source = factor(
      death_source,
      levels = c(
        "ONS_only",
        "TPP_only",
        "Both"
      )
    )
  ) |>

  # One column per original death source
  pivot_wider(
    names_from = death_source,
    values_from = total,
    values_fill = 0
  ) |>

  # Counts within each registration-end category
  mutate(
    total_deaths = ONS_only + TPP_only + Both,
    ONS = ONS_only + Both,
    TPP = TPP_only + Both
  ) |>

  # Denominators across all included registration-end categories
  group_by(
    death_date_ref_year,
    subgroup,
    subgroup_value
  ) |>
  mutate(
    total_deaths_subgroup = sum(
      total_deaths,
      na.rm = TRUE
    ),
    ONS_subgroup = sum(
      ONS,
      na.rm = TRUE
    ),
    TPP_subgroup = sum(
      TPP,
      na.rm = TRUE
    ),
    Both_subgroup = sum(
      Both,
      na.rm = TRUE
    ),
    ONS_only_subgroup = sum(
      ONS_only,
      na.rm = TRUE
    ),
    TPP_only_subgroup = sum(
      TPP_only,
      na.rm = TRUE
    )
  ) |>
  ungroup() |>

  # Apply SDC after calculating all counts and denominators
  mutate(
    across(
      c(
        total_deaths,
        total_deaths_subgroup,
        ONS,
        ONS_subgroup,
        TPP,
        TPP_subgroup,
        Both,
        Both_subgroup,
        ONS_only,
        ONS_only_subgroup,
        TPP_only,
        TPP_only_subgroup
      ),
      rounding
    )
  ) |>

  # Order columns
  select(
    death_date_ref_year,
    subgroup,
    subgroup_value,
    reg_end_timing_group,

    total_deaths,
    total_deaths_subgroup,

    ONS,
    ONS_subgroup,

    TPP,
    TPP_subgroup,

    Both,
    Both_subgroup,

    ONS_only,
    ONS_only_subgroup,

    TPP_only,
    TPP_only_subgroup
  ) |>

  arrange(
    death_date_ref_year,
    subgroup,
    subgroup_value,
    reg_end_timing_group
  )


# Export
write_csv(
  reg_end_timing_source_subgroups,
  here(
    output_dir_analysis_tables,
    "reg_end_timing_source_subgroups.csv"
  )
)


# Clean environment
rm(
  reg_end_timing_overall,
  reg_end_timing_subgroups
)