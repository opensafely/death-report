###################################################
# Author: Martina Pesce / Andrea Schaffer
# Bennett Institute for Applied Data Science
# University of Oxford, 2025
#
# 5) 
#
# Variation across practices in the proportion 
# of deaths recorded only in ONS using yearly percentiles
#
#########################################################################

# Libraries 
library(tidyverse)
library(here)
library(fs)
library(lubridate)

# Create output directory 
output_dir_analysis_tables <- here("output", "analysis_tables")
dir_create(output_dir_analysis_tables)

# Import utility functions 
source(here("analysis", "0_utility_functions.R"))

# Import data 
death_registration_processed <- read_csv(
  here("output", "highly_sensitive", "death_registration_processed.csv.gz")
)


# Main analysis: dated deaths only -------


# Restrict to the main analysis population:
# - has at least one recorded death date
# - does not have an implausible death date
# - was registered with a practice
death_registration_analysis <- death_registration_processed |>
  filter(
    death_date_ref_year >= 2020,
    flag_any_date_death == TRUE,
    flag_any_date_death_implausible == FALSE,
    flag_is_registered == TRUE
  )

# ==================================================
# Practice-level % of source-only deaths
# ==================================================

# For each practice-year:
# - TPP_only is expressed as a percentage of all deaths recorded in TPP
#   (TPP_only + Both)
# - ONS_only is expressed as a percentage of all deaths recorded in ONS
#   (ONS_only + Both)
# - ONS_only_with_snomed and ONS_only_without_snomed are also expressed
#   as percentages of all deaths recorded in ONS
#
# Practice-years with <=30 deaths recorded in either source are excluded.

# ==================================================
# Practice-level counts
# ==================================================

practice_death_source_counts <- death_registration_analysis |>
  
  group_by(
    death_date_ref_year,
    practice
  ) |>
  
  summarise(
    n_both =
      sum(death_source == "Both", na.rm = TRUE),
    
    n_tpp_only =
      sum(death_source == "TPP_only", na.rm = TRUE),
    
    n_ons_only =
      sum(death_source == "ONS_only", na.rm = TRUE),
    
    n_ons_only_with_snomed =
      sum(
        death_source == "ONS_only" &
          !is.na(tpp_coded_death_date),
        na.rm = TRUE
      ),
    
    n_ons_only_without_snomed =
      sum(
        death_source == "ONS_only" &
          is.na(tpp_coded_death_date),
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  
  mutate(
    # Check that ONS-only is fully partitioned
    n_ons_only_unclassified =
      n_ons_only -
      n_ons_only_with_snomed -
      n_ons_only_without_snomed,
    
    # Total deaths recorded in either source
    total_practice_year =
      n_both +
      n_tpp_only +
      n_ons_only,
    
    # Source-specific denominators
    any_tpp =
      n_both +
      n_tpp_only,
    
    any_ons =
      n_both +
      n_ons_only
  ) #|>
  
 # filter(total_practice_year > 30)


# ==================================================
# Practice-level percentages
# ==================================================

practice_death_source <- practice_death_source_counts |>
  
  transmute(
    year = death_date_ref_year,
    practice,
    
    TPP_only =
      if_else(
        any_tpp > 0,
        100 * n_tpp_only / any_tpp,
        NA_real_
      ),
    
    ONS_only =
      if_else(
        any_ons > 0,
        100 * n_ons_only / any_ons,
        NA_real_
      ),
    
    ONS_only_with_snomed =
      if_else(
        any_ons > 0,
        100 * n_ons_only_with_snomed / any_ons,
        NA_real_
      ),
    
    ONS_only_without_snomed =
      if_else(
        any_ons > 0,
        100 * n_ons_only_without_snomed / any_ons,
        NA_real_
      )
  ) |>
  
  pivot_longer(
    cols = c(
      TPP_only,
      ONS_only,
      ONS_only_with_snomed,
      ONS_only_without_snomed
    ),
    names_to = "death_source",
    values_to = "perc_death_source"
  )


# ==================================================
# Practice-level percentiles by death source
# ==================================================

probs <- seq(0.1, 0.9, by = 0.1)
percentiles <- probs * 100


# Number of practices contributing to each year
n_by_year <- practice_death_source_counts |>
  group_by(death_date_ref_year) |>
  summarise(
    n_practices = rounding(n_distinct(practice)),
    .groups = "drop"
  ) |>
  rename(year = death_date_ref_year)


# Calculate percentiles
table_practice_percentiles <- practice_death_source |>
  
  group_by(
    year,
    death_source
  ) |>
  
  summarise(
    value = list(
      round(
        as.numeric(
          quantile(
            perc_death_source,
            probs = probs,
            na.rm = TRUE,
            type = 3
          )
        ),
        1
      )
    ),
    percentile = list(percentiles),
    .groups = "drop"
  ) |>
  
  unnest(
    c(percentile, value)
  ) |>
  
  left_join(
    n_by_year,
    by = "year"
  ) |>
  
  mutate(
    line_group =
      if_else(
        percentile == 50,
        "median",
        "decile"
      )
  ) |>
  
  select(
    year,
    death_source,
    n_practices,
    percentile,
    value,
    line_group
  ) |>
  
  arrange(
    year,
    death_source,
    percentile
  )


# View output ----
table_practice_percentiles


# Export ----
write_csv(
  table_practice_percentiles,
  here(
    output_dir_analysis_tables,
    "table_practice_percentiles_by_death_source.csv"
  )
)