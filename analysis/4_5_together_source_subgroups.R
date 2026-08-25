###################################################
# Author: Martina Pesce / Andrea Schaffer
# Bennett Institute for Applied Data Science
# University of Oxford, 2025
#
# Date agreement and missing death dates by source
#
# Describe agreement between ONS and TPP dates of death,
# including deaths recorded in only one source.
#
# Categories:
# - original date-difference groups for deaths with dates in both sources
# - missing_tpp_date_with_snomed for ONS-only deaths with a
#   primary care SNOMED death code
# - missing_tpp_date_without_snomed for ONS-only deaths without a
#   primary care SNOMED death code
# - missing_tpp_date: reconstructed as the sum of the two categories above
# - missing_ons_date for TPP-only deaths
#
# Results are presented overall and by population subgroup and year.
#
# Inclusion criteria:
# - valid age
# - non-disclosive sex
# - registered at death
# - death recorded in ONS and/or TPP
#
# Exclusion criteria:
# - implausible death date in either source
###################################################


# ==================================================
# Libraries
# ==================================================

library(tidyverse)
library(here)
library(fs)
library(lubridate)


# ==================================================
# Create output directory
# ==================================================

output_dir_analysis_tables <- here(
  "output",
  "analysis_tables"
)

dir_create(
  output_dir_analysis_tables
)


# ==================================================
# Import utility functions
# ==================================================

source(
  here(
    "analysis",
    "0_utility_functions.R"
  )
)


# ==================================================
# Import data
# ==================================================

death_registration_processed <- read_csv(
  here(
    "output",
    "highly_sensitive",
    "death_registration_processed.csv.gz"
  )
)


# ==================================================
# Analysis population
# ==================================================

date_agreement_source_analysis <- death_registration_processed |>
  filter(
    death_date_ref_year >= 2020,
    flag_any_date_death == TRUE,
    flag_any_date_death_implausible == FALSE,
    flag_is_registered == TRUE
  ) |>
  mutate(
    date_agreement_group = case_when(
      death_source == "ONS_only" &
        !is.na(tpp_coded_death_date) ~
        "missing_tpp_date_with_snomed",
      
      death_source == "ONS_only" &
        is.na(tpp_coded_death_date) ~
        "missing_tpp_date_without_snomed",
      
      death_source == "TPP_only" ~
        "missing_ons_date",
      
      death_source == "Both" ~
        dod_diff_groups,
      
      TRUE ~
        NA_character_
    )
  )


# ==================================================
# Overall
# ==================================================

date_agreement_source_overall <- date_agreement_source_analysis |>
  group_by(
    death_date_ref_year,
    death_source,
    date_agreement_group
  ) |>
  summarise(
    total = n(),
    .groups = "drop"
  ) |>
  mutate(
    subgroup = "overall",
    subgroup_value = "All"
  )


# ==================================================
# Subgroups
# ==================================================

date_agreement_source_subgroups <- date_agreement_source_analysis |>
  select(
    death_date_ref_year,
    death_source,
    date_agreement_group,
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
    date_agreement_group
  ) |>
  summarise(
    total = n(),
    .groups = "drop"
  )


# ==================================================
# Combine overall and subgroups
# ==================================================

date_agreement_source_combined <- bind_rows(
  date_agreement_source_overall,
  date_agreement_source_subgroups
)


# ==================================================
# Reconstruct all ONS-only deaths
# ==================================================
#
# missing_tpp_date is the sum of:
# - missing_tpp_date_with_snomed
# - missing_tpp_date_without_snomed

missing_tpp_date_total <- date_agreement_source_combined |>
  filter(
    date_agreement_group %in% c(
      "missing_tpp_date_with_snomed",
      "missing_tpp_date_without_snomed"
    )
  ) |>
  group_by(
    death_date_ref_year,
    death_source,
    subgroup,
    subgroup_value
  ) |>
  summarise(
    total = sum(
      total,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    date_agreement_group =
      "missing_tpp_date"
  )


# Add reconstructed ONS-only total
date_agreement_source_combined <- bind_rows(
  date_agreement_source_combined,
  missing_tpp_date_total
)


# ==================================================
# Final annual table
# ==================================================

table_date_agreement_source_subgroups <-
  date_agreement_source_combined |>
  
  # Keep date_agreement_group as character.
  # Do not convert it to factor here, because factor levels
  # that do not match the raw dod_diff_groups would become NA
  # and would be dropped from denominator calculations.
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
  
  # Counts within each date-agreement category
  mutate(
    total_deaths =
      ONS_only +
      TPP_only +
      Both,
    
    ONS =
      ONS_only +
      Both,
    
    TPP =
      TPP_only +
      Both
  ) |>
  
  # Denominators across mutually exclusive categories.
  #
  # missing_tpp_date is excluded because it is a reconstructed
  # total of:
  # - missing_tpp_date_with_snomed
  # - missing_tpp_date_without_snomed
  #
  # All denominator calculations happen before SDC.
  group_by(
    death_date_ref_year,
    subgroup,
    subgroup_value
  ) |>
  mutate(
    total_deaths_subgroup = sum(
      total_deaths[
        date_agreement_group !=
          "missing_tpp_date"
      ],
      na.rm = TRUE
    ),
    
    ONS_subgroup = sum(
      ONS[
        date_agreement_group !=
          "missing_tpp_date"
      ],
      na.rm = TRUE
    ),
    
    TPP_subgroup = sum(
      TPP[
        date_agreement_group !=
          "missing_tpp_date"
      ],
      na.rm = TRUE
    ),
    
    Both_subgroup = sum(
      Both[
        date_agreement_group !=
          "missing_tpp_date"
      ],
      na.rm = TRUE
    ),
    
    ONS_only_subgroup = sum(
      ONS_only[
        date_agreement_group !=
          "missing_tpp_date"
      ],
      na.rm = TRUE
    ),
    
    TPP_only_subgroup = sum(
      TPP_only[
        date_agreement_group !=
          "missing_tpp_date"
      ],
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
    date_agreement_group,
    
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
    date_agreement_group
  )


# ==================================================
# Export annual table
# ==================================================

write_csv(
  table_date_agreement_source_subgroups,
  here(
    output_dir_analysis_tables,
    "table_date_agreement_source_subgroups.csv"
  )
)


# ==================================================
# Monthly death source, 2025-2026
# ==================================================
#
# Describe death source by month for deaths occurring
# from 2025 onwards.
#
# Denominators:
# - ONS = Both + ONS_only
# - TPP = Both + TPP_only
# - total_deaths = Both + ONS_only + TPP_only
#
# Counts and denominators are calculated before applying SDC.

table_death_source_25_26 <- death_registration_processed |>
  filter(
    death_date_ref_year > 2024,
    flag_any_date_death == TRUE,
    flag_any_date_death_implausible == FALSE,
    flag_is_registered == TRUE
  ) |>
  mutate(
    month = floor_date(
      death_date_ref,
      unit = "month"
    )
  ) |>
  count(
    month,
    death_source,
    name = "total"
  ) |>
  complete(
    month,
    death_source = c(
      "ONS_only",
      "TPP_only",
      "Both"
    ),
    fill = list(
      total = 0
    )
  ) |>
  pivot_wider(
    names_from = death_source,
    values_from = total,
    values_fill = 0
  ) |>
  
  # Counts and denominators before SDC
  mutate(
    total_deaths =
      ONS_only +
      TPP_only +
      Both,
    
    ONS =
      ONS_only +
      Both,
    
    TPP =
      TPP_only +
      Both
  ) |>
  
  # Apply SDC after calculating all counts and denominators
  mutate(
    across(
      c(
        total_deaths,
        ONS,
        TPP,
        Both,
        ONS_only,
        TPP_only
      ),
      rounding
    )
  ) |>
  
  # Order columns
  select(
    month,
    
    total_deaths,
    
    ONS,
    TPP,
    
    Both,
    ONS_only,
    TPP_only
  ) |>
  
  arrange(month)


# ==================================================
# Export monthly table
# ==================================================

write_csv(
  table_death_source_25_26,
  here(
    output_dir_analysis_tables,
    "table_death_source_25_26.csv"
  )
)

# ==================================================
# Clean environment
# ==================================================

rm(
  date_agreement_source_analysis,
  date_agreement_source_overall,
  date_agreement_source_subgroups,
  date_agreement_source_combined,
  missing_tpp_date_total
)

# ==================================================
# TPP coded death date relative to ONS death date
# ==================================================
#
# Compare the TPP coded death date with the ONS date of death,
# by calendar year.
#
# Population:
# - ONS-recorded deaths
# - registered at death
# - no implausible death date
#
# Categories:
# - -29+
# - -28 to -8
# - -7 to -1
# - 0
# - 1 to 7
# - 8 to 28
# - 29+
# - no_coded_death
#
# Denominator:
# All ONS-recorded deaths in each calendar year.
#
# Counts and denominators are calculated before applying SDC.

table_tpp_coded_ons_dates_diff <- death_registration_processed |>
  filter(
    death_date_ref_year >= 2020,
    death_date_ref_year <= 2025,
    !is.na(ons_death_date),
    flag_any_date_death_implausible == FALSE,
    flag_is_registered == TRUE
  ) |>
  mutate(
    tpp_coded_ons_diff = as.numeric(
      tpp_coded_death_date -
        ons_death_date
    ),
    
    tpp_coded_ons_diff_group = case_when(
      is.na(tpp_coded_death_date) ~
        "no_coded_death",
      
      tpp_coded_ons_diff <= -29 ~
        "-29+",
      
      tpp_coded_ons_diff >= -28 &
        tpp_coded_ons_diff <= -8 ~
        "-28 to -8",
      
      tpp_coded_ons_diff >= -7 &
        tpp_coded_ons_diff <= -1 ~
        "-7 to -1",
      
      tpp_coded_ons_diff == 0 ~
        "0",
      
      tpp_coded_ons_diff >= 1 &
        tpp_coded_ons_diff <= 7 ~
        "1 to 7",
      
      tpp_coded_ons_diff >= 8 &
        tpp_coded_ons_diff <= 28 ~
        "8 to 28",
      
      tpp_coded_ons_diff >= 29 ~
        "29+",
      
      TRUE ~
        NA_character_
    )
  ) |>
  filter(
    !is.na(tpp_coded_ons_diff_group)
  ) |>
  count(
    death_date_ref_year,
    tpp_coded_ons_diff_group,
    name = "n"
  ) |>
  group_by(
    death_date_ref_year
  ) |>
  mutate(
    denominator = sum(
      n,
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  
  # Apply SDC after calculating counts and denominators
  mutate(
    n = rounding(n),
    denominator = rounding(denominator)
  ) |>
  
  select(
    death_date_ref_year,
    tpp_coded_ons_diff_group,
    n,
    denominator
  ) |>
  
  arrange(
    death_date_ref_year,
    match(
      tpp_coded_ons_diff_group,
      c(
        "-29+",
        "-28 to -8",
        "-7 to -1",
        "0",
        "1 to 7",
        "8 to 28",
        "29+",
        "no_coded_death"
      )
    )
  )


# ==================================================
# Export
# ==================================================

write_csv(
  table_tpp_coded_ons_dates_diff,
  here(
    output_dir_analysis_tables,
    "table_tpp_coded_ons_dates_diff.csv"
  )
)
