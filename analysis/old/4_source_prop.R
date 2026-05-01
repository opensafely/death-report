# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")

# Rounding function

rounding <- function(vars) {
  case_when(vars == 0 ~ 0,
            vars > 7 ~ round(vars / 5) * 5)
}


## Create output directory
output_dir <- here("output", "report","by_source")
fs::dir_create(output_dir)

# dataset with grace period
DoD_TPP_ONS_with_grace_period <- read_csv("output/highly_sensitive/DoD_TPP_ONS_with_grace_period.csv")

# 1- ONS / TPP distribution without grace period --------------------------------------------
DoD_TPP_ONS_without_grace_period <- DoD_TPP_ONS_with_grace_period %>%
  mutate(
    ons_without_grace_period = !is.na(ONS_death) & death_dereg_diff_ONS >= 0,
    tpp_without_grace_period = !is.na(TPP_death) & death_dereg_diff_TPP >= 0
  ) %>% 
    mutate(
    ONS_or_TPP_without_grace_period = case_when(
      ons_without_grace_period & tpp_without_grace_period ~ "ONS & TPP",
      ons_without_grace_period & !tpp_without_grace_period ~ "ONS",
      tpp_without_grace_period & !ons_without_grace_period ~ "TPP",
      TRUE ~ NA_character_
    )
    )

# # 2- Table by source 
table_source_general_without_grace_period <- DoD_TPP_ONS_without_grace_period %>%
  group_by(year_pref_ONS) %>%
  mutate(total = rounding(n())) %>%
  group_by(year_pref_ONS, ONS_or_TPP_without_grace_period, total) %>%
  summarise(
    count = rounding(n()),
    .groups = "drop"
  ) %>%
  mutate(
    group_var = "general population",
    group_value = "general population"
  ) %>%
  select(year_pref_ONS, ONS_or_TPP_without_grace_period, count, group_var, group_value)


table_source_by_subgroup_without_grace_period <- function(data, group_var) {
  group_var_name <- deparse(substitute(group_var))

  data %>%
    filter(year_pref_ONS > 2008) %>%
    group_by(year_pref_ONS, {{ group_var }}) %>%
    mutate(total = n()) %>%
    group_by(year_pref_ONS, {{ group_var }}, ONS_or_TPP_without_grace_period, total) %>%
    summarise(
      count = rounding(n()),
      .groups = "drop"
    ) %>%
    mutate(
      group_var = group_var_name,
      group_value = as.character({{ group_var }})
    ) %>%
    select(year_pref_ONS, ONS_or_TPP_without_grace_period, count, group_var, group_value)
}


table_source_age_without_grace_period       <- table_source_by_subgroup_without_grace_period(DoD_TPP_ONS_without_grace_period, age_band)
table_source_ethnicity_without_grace_period <- table_source_by_subgroup_without_grace_period(DoD_TPP_ONS_without_grace_period, ethnicity)
table_source_region_without_grace_period    <- table_source_by_subgroup_without_grace_period(DoD_TPP_ONS_without_grace_period, region)
table_source_place_without_grace_period     <- table_source_by_subgroup_without_grace_period(DoD_TPP_ONS_without_grace_period, ons_death_place)
table_source_urban_without_grace_period     <- table_source_by_subgroup_without_grace_period(DoD_TPP_ONS_without_grace_period, rural_urban)
table_source_IMD_without_grace_period       <- table_source_by_subgroup_without_grace_period(DoD_TPP_ONS_without_grace_period, IMD_q10)
table_source_sex_without_grace_period       <- table_source_by_subgroup_without_grace_period(DoD_TPP_ONS_without_grace_period, sex)


collate_death_source_without_grace_period <- bind_rows(
  table_source_general_without_grace_period,
  table_source_age_without_grace_period,
  table_source_ethnicity_without_grace_period,
  table_source_region_without_grace_period,
  table_source_place_without_grace_period,
  table_source_urban_without_grace_period,
  table_source_IMD_without_grace_period,
  table_source_sex_without_grace_period
)

write.csv(collate_death_source_without_grace_period, here::here("output", "report","by_source", "collate_death_source_without_grace_period.csv"))


# 2. ONS / TPP distribution with grace period ---------------------------------
table_source_general <- DoD_TPP_ONS_with_grace_period %>%
  group_by(year_pref_ONS) %>%
  mutate(total = rounding(n())) %>% 
  group_by(year_pref_ONS, ONS_or_TPP, total) %>%
  summarise(
    count = rounding(n()),
    .groups = "drop"
  ) %>%
  mutate(
    group_var = "general population",
    group_value = "general population"
  ) %>%
  select(year_pref_ONS, ONS_or_TPP, count, group_var, group_value)



#source by subgroup
table_source_by_subgroup <- function(data, group_var) {
  group_var_name <- deparse(substitute(group_var))
  
  data %>%
    filter(year_pref_ONS > 2008) %>%
    group_by(year_pref_ONS, {{ group_var }}) %>%
    mutate(total = n()) %>%
    group_by(year_pref_ONS, {{ group_var }}, ONS_or_TPP, total) %>%
    summarise(
      count = rounding(n()),
      .groups = "drop"
    ) %>%
    mutate(
      group_var = group_var_name,
      group_value = as.character({{ group_var }})
    ) %>%
    select(year_pref_ONS, ONS_or_TPP, count, group_var, group_value)
}


table_source_age       <- table_source_by_subgroup(DoD_TPP_ONS_with_grace_period, age_band)
table_source_ethnicity <- table_source_by_subgroup(DoD_TPP_ONS_with_grace_period, ethnicity)
table_source_region    <- table_source_by_subgroup(DoD_TPP_ONS_with_grace_period, region)
table_source_place     <- table_source_by_subgroup(DoD_TPP_ONS_with_grace_period, ons_death_place)
table_source_urban     <- table_source_by_subgroup(DoD_TPP_ONS_with_grace_period, rural_urban)
table_source_IMD       <- table_source_by_subgroup(DoD_TPP_ONS_with_grace_period, IMD_q10)
table_source_sex       <- table_source_by_subgroup(DoD_TPP_ONS_with_grace_period, sex)


collate_death_source_table_with_grace_period <- bind_rows(
  table_source_general, 
  table_source_age,
  table_source_ethnicity,
  table_source_region,
  table_source_place,
  table_source_urban,
  table_source_IMD,
  table_source_sex
)

write.csv(collate_death_source_table_with_grace_period, here::here("output", "report","by_source", "collate_death_source_with_grace_period.csv"))

# Special periods

# % by source 2020-2024
table_source_general_20_24 <- DoD_TPP_ONS_with_grace_period %>%
  filter(year_pref_ONS > 2019 & year_pref_ONS < 2025) %>% 
  group_by(ONS_or_TPP) %>%
  summarise(
    count = rounding(n()),
    .groups = "drop"
  )  %>%
  mutate(
    group_var = "general population",
    group_value = "general population",
    period = "2020-2024"
  ) %>%
  select(period, ONS_or_TPP, count, group_var, group_value)



# % by source 01-01-2025 to 06-06-2024 by month
table_source_general_2025 <- DoD_TPP_ONS_with_grace_period %>%
  filter(year_pref_ONS > 2023 & year_month_pref_ONS != "2025-06") %>% 
  group_by(year_month_pref_ONS) %>%
  mutate(total = rounding(n())) %>% 
  group_by(year_month_pref_ONS, ONS_or_TPP, total) %>%
  summarise(
    count = rounding(n()),
    .groups = "drop"
  ) %>%
  mutate(
    group_var = "general population",
    group_value = "general population"
  ) %>%
  rename(
    period = year_month_pref_ONS
  ) %>% 
  select(period, ONS_or_TPP, count, group_var, group_value)

collate_death_source_table_spec_periods <- bind_rows(table_source_general_2025, table_source_general_20_24)

write.csv(collate_death_source_table_spec_periods, here::here("output", "report","by_source", "collate_death_source_grace_per_spec_periods.csv"))


# 3. TPP death codes -------------------------------------------------------------
# number of deaths coded TPP
death_plus_coded_TPP <- DoD_TPP_ONS_with_grace_period %>%
  mutate(
    year_pref_ONS_TPP_plus_codes = case_when(
      !is.na(ons_death_date)   ~ year(ons_death_date),
      !is.na(TPP_death_date)   ~ year(TPP_death_date),
      !is.na(death_coded_date) ~ year(death_coded_date)
    ),
    TPP_date_or_coded = case_when( 
      (!is.na(TPP_death_date) &
         is.na(death_coded_date)) ~ "TPP - dated, not coded",
      (is.na(TPP_death_date) &
         !is.na(death_coded_date)) ~ "TPP - not dated, coded",
      (!is.na(TPP_death_date) &
         !is.na(death_coded_date)) ~ "TPP - dated and coded",
      (is.na(TPP_death_date) &
         is.na(death_coded_date)) ~ "TPP - neither dated nor coded"
    )
  )
  


# Check how many have a date recorded / are coded by year
TPP_coded_date_death <- death_plus_coded_TPP %>%
  group_by(year_pref_ONS_TPP_plus_codes) %>% 
  mutate(TPP_year = rounding(n())) %>% 
  group_by(TPP_date_or_coded, year_pref_ONS_TPP_plus_codes, TPP_year) %>%
  summarise(TPP_subgroup = rounding(n())) %>%
  mutate(pcent = TPP_subgroup / TPP_year * 100)

write.csv(TPP_coded_date_death, here::here("output", "report","by_source", "TPP_coding_date_year.csv"))


# Table tpp - coded / date + ONS
ons_tpp_date_code <- death_plus_coded_TPP %>% 
  count(year_pref_ONS_TPP_plus_codes,TPP_date_or_coded, ONS_death) %>% 
  mutate(
    n= rounding(n)
  )

write.csv(ons_tpp_date_code, here::here("output", "report", "by_source", "ons_tpp_date_code_year.csv"))