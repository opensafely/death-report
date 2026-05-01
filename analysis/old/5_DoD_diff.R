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
output_dir <- here("output", "report","DoD_diff")
fs::dir_create(output_dir)

# dataset with grace period
DoD_TPP_ONS_with_grace_period <- read_csv("output/highly_sensitive/DoD_TPP_ONS_with_grace_period.csv")



##### 1- Diff DoD with grace period----------------------------------------------

# Table key indicators by year
table_DoD_general_with_grace_period <- DoD_TPP_ONS_with_grace_period %>%
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

DoD_by_age <- summarise_DoD_by_group(DoD_TPP_ONS_with_grace_period, age_band)

# DoD_by_practice <- summarise_DoD_by_group(DoD_TPP_ONS_with_grace_period, practice)

DoD_by_ons_death_place <- summarise_DoD_by_group(DoD_TPP_ONS_with_grace_period, ons_death_place)

DoD_by_region <- summarise_DoD_by_group(DoD_TPP_ONS_with_grace_period, region)

DoD_by_rural_urban <- summarise_DoD_by_group(DoD_TPP_ONS_with_grace_period, rural_urban)

DoD_by_IMD_q10 <- summarise_DoD_by_group(DoD_TPP_ONS_with_grace_period, IMD_q10)

DoD_by_ethnicity <- summarise_DoD_by_group(DoD_TPP_ONS_with_grace_period, ethnicity)

DoD_by_sex <- summarise_DoD_by_group(DoD_TPP_ONS_with_grace_period, sex)

collate_DoD_diff_with_grace_period_table <- rbind(table_DoD_general_with_grace_period, DoD_by_age, DoD_by_rural_urban, DoD_by_ons_death_place, DoD_by_region, DoD_by_IMD_q10, DoD_by_ethnicity, DoD_by_sex)

write.csv(collate_DoD_diff_with_grace_period_table, here::here("output", "report", "DoD_diff", "collate_DoD_diff_with_grace_period_table.csv"))
