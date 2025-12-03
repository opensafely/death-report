# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")


## Create output directory
output_dir <- here("output", "report", "reg_dereg_ONS_TPP")
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
  )

# Rounding function

rounding <- function(vars) {
  case_when(vars == 0 ~ 0,
            vars > 7 ~ round(vars / 5) * 5)
}

# Data curating ---------------

dataset <- dataset0 %>%
  mutate(
    TPP_death_reg = case_when(
      !is.na(TPP_death_date) & !is.na(last_registration_start_date) ~
        as.Date(TPP_death_date) - as.Date(last_registration_start_date),
      TRUE ~ as.difftime(NA_real_, units = "days")
    ),
    TPP_death_dereg = case_when(
      !is.na(last_registration_end_date) & !is.na(TPP_death_date) ~
        as.Date(last_registration_end_date) - as.Date(TPP_death_date),
      TRUE ~ as.difftime(NA_real_, units = "days")
    ),
    ons_death_reg = case_when(
      !is.na(ons_death_date) & !is.na(last_registration_start_date) ~
        as.Date(ons_death_date) - as.Date(last_registration_start_date),
      TRUE ~ as.difftime(NA_real_, units = "days")
    ),
    ons_death_dereg = case_when(
      !is.na(last_registration_end_date) & !is.na(ons_death_date) ~
        as.Date(last_registration_end_date) - as.Date(ons_death_date),
      TRUE ~ as.difftime(NA_real_, units = "days")
    )
  ) %>%
  mutate(
    ons_death_reg_group = case_when(
      is.na(ons_death_reg) ~ NA_character_,
      ons_death_reg < 0    ~ "last_reg_after_death",
      ons_death_reg == 0   ~ "last_reg_same_death",
      ons_death_reg > 0    ~ "last_reg_before_death"
    ),
    ons_death_dereg_group = case_when(
      is.na(ons_death_dereg)                            ~ NA_character_,
      ons_death_dereg <= -32                            ~ "< -32",
      ons_death_dereg >= -31 & ons_death_dereg <= -8    ~ "-31 to -8",
      ons_death_dereg >= -7  & ons_death_dereg <= -1    ~ "-7 to -1",
      ons_death_dereg == 0                              ~ "0",
      ons_death_dereg >= 1   & ons_death_dereg <= 7     ~ "1-7",
      ons_death_dereg >= 8   & ons_death_dereg <= 31    ~ "8-31",
      ons_death_dereg >= 32                             ~ "32+"
    ),
    TPP_death_reg_group = case_when(
      is.na(TPP_death_reg) ~ NA_character_,
      TPP_death_reg < 0    ~ "last_reg_after_death",
      TPP_death_reg == 0   ~ "last_reg_same_death",
      TPP_death_reg > 0    ~ "last_reg_before_death"
    ),
    TPP_death_dereg_group = case_when(
      is.na(TPP_death_dereg)                            ~ NA_character_,
      TPP_death_dereg <= -32                            ~ "< -32",
      TPP_death_dereg >= -31 & TPP_death_dereg <= -8    ~ "-31 to -8",
      TPP_death_dereg >= -7  & TPP_death_dereg <= -1    ~ "-7 to -1",
      TPP_death_dereg == 0                              ~ "0",
      TPP_death_dereg >= 1   & TPP_death_dereg <= 7     ~ "1-7",
      TPP_death_dereg >= 8   & TPP_death_dereg <= 31    ~ "8-31",
      TPP_death_dereg >= 32                             ~ "32+"
    )    
  )

# Tables 1. Relation between ONS and registration data ------------------------------

# a- ONS deaths distribution across time and inclusion criteria

ONS_death_year_by_reg <- dataset %>%
  filter(!is.na(ons_death_date)) %>%
  group_by(ons_death_year = year(ons_death_date)) %>%
  summarise(
    any_ons_death = rounding(n()), # any ONS death
    ons_death_regist_before = rounding(sum(has_registration == TRUE, na.rm = TRUE)), # ONS + last registration before death
    ons_registred_during = rounding(sum(ons_death_during_study == TRUE & has_registration == TRUE, na.rm = TRUE)),
    tpp_any = rounding(sum(!is.na(TPP_death_date), na.rm = TRUE)), # any ONS death + any TPP death
    tpp_reg_during_study = rounding(sum(tpp_death_during_study == TRUE & has_registration == TRUE, na.rm = TRUE)), # ONS + TPP + last dereg date after death
    .groups = "drop"
  )

write.csv(ONS_death_year_by_reg, here::here("output", "report", "reg_dereg_ONS_TPP", "ONS_death_year_by_reg.csv"))

# b- Difference date of death - last registration date
ONS_death_reg_group<- dataset %>%
  filter(!is.na(ons_death_date)) %>%
  group_by(ons_death_year = year(ons_death_date), ons_death_reg_group) %>%
  summarise(n = rounding(n()), .groups = "drop")

write.csv(ONS_death_reg_group, here::here("output", "report", "reg_dereg_ONS_TPP", "ONS_death_reg_group.csv"))

# c- Difference deregistration - death
ons_death_dereg_group <- dataset %>%
  filter(!is.na(ons_death_date) & has_registration == TRUE) %>%
  group_by(ons_death_year = year(ons_death_date), ons_death_dereg_group) %>%
  summarise(n = rounding(n()), .groups = "drop")

write.csv(ons_death_dereg_group, here::here("output", "report", "reg_dereg_ONS_TPP", "ons_death_dereg_group.csv"))

# Tables 2. Relation between TPP and registration data ------------------------------

# a- NO

# b- Difference date of death - last registration date
TPP_death_reg_group<- dataset %>%
  filter(!is.na(TPP_death_date)) %>%
  group_by(TPP_death_year = year(TPP_death_date), TPP_death_reg_group) %>%
  summarise(n = rounding(n()), .groups = "drop")

write.csv(TPP_death_reg_group, here::here("output", "report", "reg_dereg_ONS_TPP", "TPP_death_reg_group.csv"))

# c- Difference deregistration - death
TPP_death_dereg_group <- dataset %>%
  filter(!is.na(TPP_death_date) & has_registration == TRUE) %>%
  group_by(TPP_death_year = year(TPP_death_date), TPP_death_dereg_group) %>%
  summarise(n = rounding(n()), .groups = "drop")

write.csv(TPP_death_dereg_group, here::here("output", "report", "reg_dereg_ONS_TPP", "TPP_death_dereg_group.csv"))