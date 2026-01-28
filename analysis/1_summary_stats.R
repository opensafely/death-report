# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")
library("skimr")

## Create output directory
output_dir <- here("output", "summary_stats")
fs::dir_create(output_dir)

# Import processed data ----
dataset_death_TPP_ONS <- read_csv("output/highly_sensitive/dataset_death_TPP_ONS.csv.gz") %>%
  mutate(
    region = as.factor(region),
    age_band = as.factor(age_band),
    rural_urban = as.factor(rural_urban),
    ons_death_place = as.factor(ons_death_place),
    region = as.factor(region),
    IMD_q10 = as.factor(IMD_q10),
    ethnicity = as.factor(ethnicity),
    min_DoD = pmin(TPP_death_date, ons_death_date, na.rm = TRUE)
  ) %>%
  mutate(
    death_dereg_diff = case_when(
      !is.na(last_registration_end_date) ~ as.Date(last_registration_end_date) - min_DoD,
      TRUE ~ as.difftime(NA_real_, units = "days"
      )
    )
  )
                                                      
dataset_death_TPP_ONS_plus_inc_crit <- dataset_death_TPP_ONS %>% 
  filter(
    has_registration == TRUE & # was registered at the beginning of the year the person died
      any_death_during_study == TRUE # died between "2009-01-01" - "2025-06-06" + (deregistration date + 30 days) is after one date of death
  )

dataset_death_TPP_ONS_plus_inc_crit_0_dereg_death <- dataset_death_TPP_ONS_plus_inc_crit %>% 
  filter(
    death_dereg_diff >= 0)
    
  

# Summary
## Gral
summary_dataset_death_TPP_ONS <- skim(dataset_death_TPP_ONS)

summary_dataset_death_TPP_ONS_plus_inc_crit <- skim(dataset_death_TPP_ONS_plus_inc_crit)

summary_dataset_death_TPP_ONS_plus_inc_crit_0_dereg_death <- skim(dataset_death_TPP_ONS_plus_inc_crit_0_dereg_death)

write.csv(summary_dataset_death_TPP_ONS, file = here::here("output", "summary_stats","summary_stats_dataset_death_TPP_ONS.csv"),
          row.names = FALSE)

write.csv(summary_dataset_death_TPP_ONS_plus_inc_crit, file = here::here("output", "summary_stats","summary_dataset_death_TPP_ONS_plus_inc_crit.csv"),
          row.names = FALSE)

write.csv(summary_dataset_death_TPP_ONS_plus_inc_crit_0_dereg_death, file = here::here("output", "summary_stats","summary_dataset_death_TPP_ONS_plus_inc_crit_0_dereg_death.csv"),
          row.names = FALSE)

## Categorical variables

table_freq <- dataset_death_TPP_ONS %>% 
  pivot_longer(cols = c(age_band, ons_death_place:ethnicity), names_to = "subgroup", values_to = "category") %>% 
  group_by(year(min_DoD), subgroup, category) %>% 
  summarise(
    n=n()
  )

table_freq_plus_inc_crit <- dataset_death_TPP_ONS_plus_inc_crit %>% 
  pivot_longer(cols = c(age_band, ons_death_place:ethnicity), names_to = "subgroup", values_to = "category") %>% 
  group_by(year(min_DoD), subgroup, category) %>% 
  summarise(
    n=n()
  )

table_freq_plus_inc_crit_0_dereg_death <- dataset_death_TPP_ONS_plus_inc_crit_0_dereg_death %>% 
  pivot_longer(cols = c(age_band, ons_death_place:ethnicity), names_to = "subgroup", values_to = "category") %>% 
  group_by(year(min_DoD), subgroup, category) %>% 
  summarise(
    n=n()
  )

write.csv(table_freq, file = here::here("output", "summary_stats","table_freq_DoD.csv"),
          row.names = FALSE)

write.csv(table_freq_plus_inc_crit, file = here::here("output", "summary_stats","table_freq_plus_inc_crit.csv"),
          row.names = FALSE)

write.csv(table_freq_plus_inc_crit_0_dereg_death, file = here::here("output", "summary_stats","table_freq_plus_inc_crit_0_dereg_death.csv"),
          row.names = FALSE)


# Impossible dates of death
impossible_dod <- dataset_death_TPP_ONS %>% 
  mutate(
    ons_DoD_impossible = case_when(
      ons_death_date < date_of_birth                ~ "death_before_birth",
      ons_death_date < as.Date("2009-01-01")        ~ "before_study",
      ons_death_date > as.Date("2025-06-06")        ~ "after_study",
      is.na(ons_death_date)                         ~ "is empty",   
      TRUE                                          ~ "ok"
    ),
    TPP_DoD_impossible = case_when(
      TPP_death_date < date_of_birth                ~ "death_before_birth",
      TPP_death_date < as.Date("2009-01-01")        ~ "before_study",
      TPP_death_date > as.Date("2025-06-06")        ~ "after_study",
      is.na(TPP_death_date)                         ~ "is empty",
      TRUE                                          ~ "ok"
    ),
    year_month_min_dod = format(min_DoD, "%Y-%m"),
    year_min_dod = format(min_DoD, "%Y")
  ) %>% 
  pivot_longer(
    cols = c(ons_DoD_impossible, TPP_DoD_impossible),
    names_to = "source",
    values_to = "DoD_impossible"
  ) %>% 
  mutate(
    source = case_when(
      source == "ons_DoD_impossible" ~ "ONS",
      source == "TPP_DoD_impossible" ~ "TPP",
      TRUE                           ~ source
    )
  ) 

# by month
impossible_dod_month <- impossible_dod %>% 
  group_by(year_month_min_dod, source, DoD_impossible) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  )

write.csv(
  impossible_dod_month,
  here::here("output", "summary_stats", "impossible_dod_month.csv"),
  row.names = FALSE
)


# by year
impossible_dod_year <- impossible_dod %>% 
group_by(year_min_dod, source, DoD_impossible) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  )

write.csv(
  impossible_dod_year,
  here::here("output", "summary_stats", "impossible_dod_year.csv"),
  row.names = FALSE
)
  




