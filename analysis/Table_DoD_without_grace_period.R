# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")


## Create output directory
output_dir <- here("output", "report", "without_grace_period")
fs::dir_create(output_dir)

# Import processed data ----
dataset0 <- read_csv("output/dataset_death_date_diff.csv.gz") %>% 
  mutate(
    TPP_death_date = as.Date(TPP_death_date),
    ons_death_date = as.Date(ons_death_date),
    date_of_birth = as.Date(date_of_birth),
    age_band = as.factor(age_band),
    practice = as.factor(practice),
    ons_death_place = as.factor(ons_death_place),
    region = as.factor(region),
    IMD_q10 = as.factor(IMD_q10),
    ethnicity = as.factor(ethnicity),
    sex = as.factor(sex),
    last_registration_start_date = as.Date(last_registration_start_date),
    last_registration_end_date = as.Date(last_registration_end_date)
  ) %>% 
  filter(
    has_registration == TRUE & # was registered at the beginning of the year the person died
    any_death_during_study == TRUE # died between "2009-01-01" - "2025-06-06" + (deregistration date + 30 days) is after one date of death
    )

# Rounding function

rounding <- function(vars) {
  case_when(vars == 0 ~ 0,
            vars > 7 ~ round(vars / 5) * 5)
}

# -----------------------
# Create variables
DoD_diff_without_grace_period_dataset <- dataset0 %>%
  mutate(
    death_dereg_diff_TPP = case_when(
      !is.na(last_registration_end_date) ~ as.Date(last_registration_end_date) - TPP_death_date,
      TRUE ~ as.difftime(NA_real_, units = "days"
      )),
    death_dereg_diff_ONS = case_when(
      !is.na(last_registration_end_date) ~ as.Date(last_registration_end_date) - ons_death_date,
      TRUE ~ as.difftime(NA_real_, units = "days"
      )),
    DoD_min = pmin(TPP_death_date, ons_death_date, na.rm = TRUE),
    diff_DoD = TPP_death_date - ons_death_date,
    TPP_death = case_when(!is.na(TPP_death_date) ~ "yes",
                          TRUE ~ NA_character_),
    ONS_death = case_when(!is.na(ons_death_date) ~ "yes",
                          TRUE ~ NA_character_),
    rural_urb_recode = case_when(
      rural_urban < 5 ~ "urban",
      rural_urban >= 5 ~ "rural",
      TRUE ~ NA_character_
    ),
    year_pref_ONS = if_else(!is.na(ons_death_date), year(ons_death_date), year(TPP_death_date)),
    year_month_pref_ONS = if_else(!is.na(ons_death_date), format(ons_death_date, "%Y-%m"), format(TPP_death_date, "%Y-%m")
    )
  ) %>%
  mutate(
    ONS_or_TPP = case_when(
      !is.na(ONS_death) & !is.na(TPP_death) ~ "ONS & TPP",
      !is.na(ONS_death) & is.na(TPP_death) ~ "ONS",
      !is.na(TPP_death) & is.na(ONS_death) ~ "TPP",
      is.na(TPP_death) & is.na(ONS_death) ~ NA_character_
    ),
    DoD_groups = case_when(
      diff_DoD == 0 ~ "0",
      
      diff_DoD >= 1 & diff_DoD <= 7 ~ "1-7",
      diff_DoD >= 8 & diff_DoD <= 31 ~ "8-31",
      diff_DoD >= 32 ~ "32+",
      
      diff_DoD <= -1 & diff_DoD >= -7 ~ "-1 to -7",
      diff_DoD <= -8 & diff_DoD >= -31 ~ "-8 to -31",
      diff_DoD <= -32 ~ "-32+",      
      
      TRUE ~ NA_character_
    ),
    DoD_dereg_tpp = case_when(
      death_dereg_diff_TPP == 0 ~ "0",
      
      death_dereg_diff_TPP >= 1 & death_dereg_diff_TPP <= 7 ~ "1-7",
      death_dereg_diff_TPP >= 8 & death_dereg_diff_TPP <= 31 ~ "8-31",
      death_dereg_diff_TPP >= 32 ~ "32+",
      
      death_dereg_diff_TPP <= -1 & death_dereg_diff_TPP >= -7 ~ "-1 to -7",
      death_dereg_diff_TPP <= -8 & death_dereg_diff_TPP >= -31 ~ "-8 to -31",
      death_dereg_diff_TPP <= -32 ~ "-32+",      
      
      TRUE ~ NA_character_
    ),
    DoD_dereg_TPP_group = case_when(
      death_dereg_diff_TPP == 0 ~ "0",
      
      death_dereg_diff_TPP >= 1 & death_dereg_diff_TPP <= 7 ~ "1-7",
      death_dereg_diff_TPP >= 8 & death_dereg_diff_TPP <= 31 ~ "8-31",
      death_dereg_diff_TPP >= 32 ~ "32+",
      
      death_dereg_diff_TPP <= -1 & death_dereg_diff_TPP >= -7 ~ "-1 to -7",
      death_dereg_diff_TPP <= -8 & death_dereg_diff_TPP >= -31 ~ "-8 to -31",
      death_dereg_diff_TPP <= -32 ~ "-32+",      
      
      TRUE ~ NA_character_
    ),  
    DoD_dereg_ONS_group = case_when(
      death_dereg_diff_ONS == 0 ~ "0",
      
      death_dereg_diff_ONS >= 1 & death_dereg_diff_ONS <= 7 ~ "1-7",
      death_dereg_diff_ONS >= 8 & death_dereg_diff_ONS <= 31 ~ "8-31",
      death_dereg_diff_ONS >= 32 ~ "32+",
      
      death_dereg_diff_ONS <= -1 & death_dereg_diff_ONS >= -7 ~ "-1 to -7",
      death_dereg_diff_ONS <= -8 & death_dereg_diff_ONS >= -31 ~ "-8 to -31",
      death_dereg_diff_ONS <= -32 ~ "-32+",      
      
      TRUE ~ NA_character_
    )  
  ) %>%
  filter(
    death_dereg_diff_ONS >= 0 | death_dereg_diff_TPP >= 0
  )
  




##### 1- Diff DoD----------------------------------------------

# Table key indicators by year
table_DoD_general_without_grace_period_dataset <- DoD_diff_without_grace_period_dataset %>%
  filter(ONS_or_TPP == "ONS & TPP") %>%
  group_by(
    year_pref_ONS
  ) %>% 
  mutate(
    GP_ONS_annual_deaths = rounding(n())
  ) %>% 
  ungroup() %>% 
  group_by(year_pref_ONS,  DoD_groups , GP_ONS_annual_deaths) %>%
  summarise(count_by_group_DoD = rounding(n())) %>% 
  mutate(
    group_var = "general population",
    group_value = "general population"
  )


#Date diff by group
summarise_DoD_by_group <- function(data, group_var) {
  group_var_name <- deparse(substitute(group_var))
  
  data %>%
    filter(ONS_or_TPP == "ONS & TPP") %>%
    group_by(year_pref_ONS, {{ group_var }}) %>%
    mutate(GP_ONS_annual_deaths = rounding(n())) %>%
    ungroup() %>%
    group_by(year_pref_ONS, {{ group_var }}, DoD_groups, GP_ONS_annual_deaths) %>%
    summarise(count_by_group_DoD = rounding(n()), .groups = "drop") %>%
    mutate(
      group_var = group_var_name,
      group_value = as.character({{ group_var }})
    ) %>%
    select(year_pref_ONS, DoD_groups, GP_ONS_annual_deaths, count_by_group_DoD, group_var, group_value)
}

#tables by group

DoD_by_age <- summarise_DoD_by_group(DoD_diff_without_grace_period_dataset, age_band)

# DoD_by_practice <- summarise_DoD_by_group(DoD_diff_without_grace_period_dataset, practice)

DoD_by_ons_death_place <- summarise_DoD_by_group(DoD_diff_without_grace_period_dataset, ons_death_place)

DoD_by_region <- summarise_DoD_by_group(DoD_diff_without_grace_period_dataset, region)

DoD_by_rural_urban <- summarise_DoD_by_group(DoD_diff_without_grace_period_dataset, rural_urban)

DoD_by_IMD_q10 <- summarise_DoD_by_group(DoD_diff_without_grace_period_dataset, IMD_q10)

DoD_by_ethnicity <- summarise_DoD_by_group(DoD_diff_without_grace_period_dataset, ethnicity)

DoD_by_sex <- summarise_DoD_by_group(DoD_diff_without_grace_period_dataset, sex)

collate_DoD_diff_without_grace_period_table <- rbind(table_DoD_general_without_grace_period_dataset, DoD_by_age, DoD_by_rural_urban, DoD_by_ons_death_place, DoD_by_region, DoD_by_IMD_q10, DoD_by_ethnicity, DoD_by_sex)

write.csv(collate_DoD_diff_without_grace_period_table, here::here("output", "report", "without_grace_period", "collate_DoD_diff_without_grace_period_table.csv"))

# 2- Table by source --------------------------------------------------------------------------------------
table_source_general_without_grace_period <- DoD_diff_without_grace_period_dataset %>%
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


table_source_age       <- table_source_by_subgroup(DoD_diff_without_grace_period_dataset, age_band)
table_source_ethnicity <- table_source_by_subgroup(DoD_diff_without_grace_period_dataset, ethnicity)
table_source_region    <- table_source_by_subgroup(DoD_diff_without_grace_period_dataset, region)
table_source_place     <- table_source_by_subgroup(DoD_diff_without_grace_period_dataset, ons_death_place)
table_source_urban     <- table_source_by_subgroup(DoD_diff_without_grace_period_dataset, rural_urban)
table_source_IMD       <- table_source_by_subgroup(DoD_diff_without_grace_period_dataset, IMD_q10)
table_source_sex       <- table_source_by_subgroup(DoD_diff_without_grace_period_dataset, sex)


collate_death_source_table_without_grace_period <- bind_rows(
  table_source_general_without_grace_period,                                   
  table_source_age,
  table_source_ethnicity,
  table_source_region,
  table_source_place,
  table_source_urban,
  table_source_IMD,
  table_source_sex
)

write.csv(collate_death_source_table_without_grace_period, here::here("output", "report", "without_grace_period", "collate_death_source_table_without_grace_period.csv"))


# % by source 2020-2024
table_source_general_20_24_without_grace_period <- DoD_diff_without_grace_period_dataset %>%
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
table_source_general_2025_without_grace_period <- DoD_diff_without_grace_period_dataset %>%
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

collate_death_source_table_spec_periods_without_grace_period <- bind_rows(table_source_general_2025_without_grace_period, table_source_general_20_24_without_grace_period)

write.csv(collate_death_source_table_spec_periods_without_grace_period, here::here("output", "report", "without_grace_period", "collate_death_source_table_spec_periods_without_grace_period.csv"))

# Diff deregistration - death ---------------------------------
by_year_dereg_DoD_diff_without_grace_period <- DoD_diff_without_grace_period_dataset %>%
  select(
    year_pref_ONS,
    DoD_dereg_ONS_group,
    DoD_dereg_TPP_group
  ) %>%
  pivot_longer(
    cols = c(DoD_dereg_ONS_group, DoD_dereg_TPP_group),
    names_to = "source",
    values_to = "dereg_group"
  ) %>%
  mutate(
    source = case_when(
      source == "DoD_dereg_ONS_group" ~ "ONS",
      source == "DoD_dereg_TPP_group" ~ "TPP",
      TRUE ~ source
    )
  ) %>%
  group_by(year_pref_ONS, source, dereg_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year_pref_ONS, source) %>%
  mutate(
    total_year_source = sum(n),
    prop = n / total_year_source
  ) %>%
  ungroup()
write.csv(by_year_dereg_DoD_diff_without_grace_period, here::here("output", "report", "without_grace_period", "by_year_dereg_DoD_diff_without_grace_period.csv"))

