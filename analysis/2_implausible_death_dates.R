###################################################
# Author: Martina Pesce / Andrea Schaffer
# Bennett Institute for Applied Data Science
# University of Oxford, 2025
#
# Aim:
# Identify implausible death dates separately for ONS and TPP,
# including: before date of birth & outside study period
#
# Inclusion criteria:
#   - valid age
#   - non-disclosive sex
#   - at least one recorded death date (ONS and/or TPP)
#
###################################################

# Libraries ----

library(tidyverse)
library(here)
library(fs)

# Create output directory ----

output_dir_analysis_tables <- here("output", "analysis_tables")
dir_create(output_dir_analysis_tables)

# Import custom functions ----
source(here("analysis", "0_utility_functions.R"))

# Import data ----

death_registration_processed <- read_csv(
  here("output", "highly_sensitive", "death_registration_processed.csv.gz")
)

#--------------------------------------------------------
# Implausible death dates by source and year
#
# Filter: Patients with at least one death date (ONS or TPP)
#
# Output:
# Counts of plausibility categories by:
#   - year (reference death year)
#   - source (ONS vs TPP)
#   - plausibility category
#---------------------------------------------------------

death_implausible_source <- death_registration_processed |>
  
  # Keep individuals with any recorded death date 
  # (exclude patients with death only recorded in TPP codes)
  filter(flag_any_date_death == TRUE) |>
  
  select(
    patient_id,
    death_date_ref_year,
    cat_ons_death_date,
    cat_tpp_death_date
  ) |>
  
  pivot_longer(
    cols = c(cat_ons_death_date, cat_tpp_death_date),
    names_to = "source",
    values_to = "cat_death_date_plausi"
  ) |>
  
  mutate(
    source = case_when(
      source == "cat_ons_death_date" ~ "ons",
      source == "cat_tpp_death_date" ~ "tpp"
    )
  ) |>
  
  # Aggregate counts by year, source, and plausibility category
  group_by(death_date_ref_year, source, cat_death_date_plausi) |>
  
  summarise(
    total = rounding(n()), # SDC rounding
    .groups = "drop"
  ) |>
  
  arrange(death_date_ref_year, source, cat_death_date_plausi)

# Export results ----

write_csv(
  death_implausible_source,
  here(output_dir_analysis_tables, "death_implausible_source.csv")
)