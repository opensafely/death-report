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

# Restrict to the main analysis population
death_registration_analysis <- death_registration_processed |>
  filter(
    flag_any_date_death == TRUE,
    flag_any_date_death_implausible == FALSE,
    flag_is_registered == TRUE
  )

# ==================================================
# Practice-level % of deaths recorded only in ONS
# Includes practices with 0% ONS only
# ==================================================

practice_ons_only <- death_registration_analysis |>
  group_by(death_date_ref_year, practice) |>
  summarise(
    total_practice_year = n(),
    ons_only_n = sum(death_source == "ONS_only", na.rm = TRUE),
    perc_ons_only = 100 * ons_only_n / total_practice_year,
    .groups = "drop"
  ) |>
  filter(total_practice_year > 9) |>
  rename(year = death_date_ref_year)

# ==================================================
# Practice-level percentiles of % ONS-only deaths
# Long format for plotting
# ==================================================

probs <- seq(0.1, 0.9, by = 0.1)
percentiles <- probs * 100

n_by_year <- practice_ons_only |>
  group_by(year) |>
  summarise(
    n_practices = rounding(n_distinct(practice)),
    .groups = "drop"
  )

table_practice_percentiles <- practice_ons_only |>
  group_by(year) |>
  summarise(
    value = list(
      round(as.numeric(quantile(
        perc_ons_only,
        probs = probs,
        na.rm = TRUE,
        type = 3)), 1)
    ),
    percentile = list(percentiles),
    .groups = "drop"
  ) |>
  unnest(c(percentile, value)) |>
  left_join(n_by_year, by = "year") |>
  mutate(
    line_group = if_else(percentile == 50, "median", "decile")
  ) |>
  select(year, n_practices, percentile, value, line_group) |>
  arrange(year, percentile)

table_practice_percentiles

# Export main analysis table ----
write_csv(
  table_practice_percentiles,
  here(output_dir_analysis_tables, "table_practice_percentiles.csv")
)