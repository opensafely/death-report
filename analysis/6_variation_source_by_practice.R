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
    flag_any_date_death == TRUE,
    flag_any_date_death_implausible == FALSE,
    flag_is_registered == TRUE
  )

# ==================================================
# Practice-level % of deaths by death source
# ==================================================

# This calculates, for each practice-year:
# - the total number of deaths
# - the number of deaths from each death source
# - the percentage of deaths from each death source
#
# Calculates the distribution separately for each source

practice_death_source <- death_registration_analysis |>
  
  count(
    death_date_ref_year,
    practice,
    death_source,
    name = "death_source_n"
  ) |>
  
  group_by(death_date_ref_year, practice) |>
  
  complete(
    death_source = c("ONS_only", "TPP_only", "Both"),
    fill = list(death_source_n = 0)
  ) |>
  
  mutate(
    total_practice_year = sum(death_source_n),
    perc_death_source =
      100 * death_source_n / total_practice_year
  ) |>
  
  ungroup() |>
  
  filter(total_practice_year > 30) |>
  
  rename(year = death_date_ref_year)


# ==================================================
# Practice-level percentiles by death source
# ==================================================

# Define the percentiles to calculate:
# 10th, 20th, ..., 90th percentile
probs <- seq(0.1, 0.9, by = 0.1)
percentiles <- probs * 100

# Count number of practices contributing to each year.
# This is calculated once per year, not separately by death source,
# because the same set of practice-years contributes to each source.
n_by_year <- practice_death_source |>
  group_by(year) |>
  summarise(
    n_practices = rounding(n_distinct(practice)),
    .groups = "drop"
  )

# Calculate percentiles of practice-level percentages.
#
# For each year and death source, this summarises the distribution of
# practice-level percentages across practices.
#
table_practice_percentiles <- practice_death_source |>
  group_by(year, death_source) |>
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
  unnest(c(percentile, value)) |>
  left_join(n_by_year, by = "year") |>
  mutate(
    line_group = if_else(percentile == 50, "median", "decile")
  ) |>
  select(
    year,
    death_source,
    n_practices,
    percentile,
    value,
    line_group
  ) |>
  arrange(year, death_source, percentile)

# View output ----
table_practice_percentiles

# Export main analysis table ----
write_csv(
  table_practice_percentiles,
  here(output_dir_analysis_tables, "table_practice_percentiles_by_death_source.csv")
)