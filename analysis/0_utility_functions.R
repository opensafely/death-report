# #################################################
# 
# Purpose:
# define useful functions used in the codebase
# this script should be sourced (using `source(here("analysis", "0_utility_functions.R"))`) at the start of each R script
#
###################################################

# ---------------------------------------------------------
# Study period
# ---------------------------------------------------------

# study_start_date <- as.Date("2009-01-01")
# study_end_date   <- as.Date("2026-03-06")

# ---------------------------------------------------------
# Rounding function (sdc)
# ---------------------------------------------------------
# - values <= 7 suppressed (NA)
rounding <- function(vars) {
  case_when(
    vars == 0 ~ 0,
    vars > 7 ~ round(vars / 5) * 5
  )
}


# ---------------------------------------------------------
# Implausible death date flag
# ---------------------------------------------------------
# TODO: parameterize start and end dates ?
# Flags implausible death dates
# Returns a categorical status
cat_implausible_death_date <- function(death_date, date_of_birth) {
  case_when(
    is.na(death_date) ~ "missing",
    death_date < date_of_birth ~ "death_before_birth",
    death_date < as.Date("2009-01-01") ~ "before_study",
    death_date > as.Date("2026-03-06") ~ "after_study",
    TRUE ~ "ok"
  )|>
    factor(
      levels = c(
        "ok",
        "missing",
        "death_before_birth",
        "before_study",
        "after_study"
      ),
      ordered = TRUE
    )
}

# ---------------------------------------------------------
# Classify source of death
# ---------------------------------------------------------
# Categories: ONS_only, TPP_only, Both
cat_death_source <- function(ons_death_date, tpp_death_info) {
  case_when(
    !is.na(ons_death_date) & !is.na(tpp_death_info) ~ "Both",
    !is.na(ons_death_date) &  is.na(tpp_death_info) ~ "ONS_only",
     is.na(ons_death_date) & !is.na(tpp_death_info) ~ "TPP_only",
    TRUE ~ NA_character_
  )  |>
    factor(
      levels = c("ONS_only", "TPP_only", "Both")
    )
}

# ---------------------------------------------------------
# Categorise timing: registration start and death
# ---------------------------------------------------------
# Input: days = death_date - registration_start_date
cat_last_reg_start_to_death <- function(days) {
  case_when(
    is.na(days) ~ "missing_registration_start", # should be no cases
    days < 0 ~ "death_before_registration_start",
    days == 0 ~ "same_day_as_registration_start",
    days > 0 ~ "death_after_registration_start"
  ) |>
    factor(
      levels = c(
        "death_before_registration_start",
        "same_day_as_registration_start",
        "death_after_registration_start",
        "missing_registration_start"
      ),
      ordered = TRUE
    )
}


# ---------------------------------------------------------
# Categorise timing: registration end - death
# ---------------------------------------------------------
# Input: days = registration_end_date - death_date
# Interpretation:
#   negative  deregistration before death
#   positive  deregistration after death
#   0         same day
#   NA        missing deregistration date
cat_last_reg_end_minus_death <- function(days) {
  case_when(
    is.na(days) ~ "missing_registration_end",
    days <= -31 ~ "<-31",
    days >= -30 & days <= -8 ~ "-30_to_-8",
    days >= -7 & days <= -1 ~ "-7_to_-1",
    days == 0 ~ "0",
    days >= 1 & days <= 7 ~ "1_to_7",
    days >= 8 & days <= 30 ~ "8_to_30",
    days >= 31 ~ "31+"
  ) |>
    factor(
      levels = c(
        "<-31",
        "-30_to_-8",
        "-7_to_-1",
        "0",
        "1_to_7",
        "8_to_30",
        "31+",
        "missing_registration_end"
      ),
      ordered = TRUE
    )
}



# ---------------------------------------------------------
# Registration status at death
# ---------------------------------------------------------
# Temporal relationship between:
# - death date
# - registration start
# - registration end
#
# Includes grace period (default = 30 days)
cat_registration_status <- function(
    death_date,
    reg_start,
    reg_end,
    grace_days = 30 
) {
  case_when(
    is.na(death_date) ~ "no_death_date",
    is.na(reg_start) ~ "no_registration",
    
    death_date < reg_start ~ "death_before_last_registration_start",
    
    is.na(reg_end) & death_date >= reg_start ~ "death_during_registration_open_end",
    death_date <= reg_end ~ "death_during_registration",
    
    death_date > reg_end & death_date <= reg_end + grace_days ~ "death_after_deregistration_within_grace",
    death_date > reg_end + grace_days ~ "death_after_deregistration_outside_grace"
  ) |>
    factor(
      levels = c(
        "death_before_last_registration_start",
        "death_during_registration",
        "death_during_registration_open_end",
        "death_after_deregistration_within_grace",
        "death_after_deregistration_outside_grace",
        "no_registration",
        "no_death_date"
      ),
      ordered = TRUE
    )
}


#------------------------------------
# TPP death date vs SNOMED codes 
#------------------------------------
# IMP: individuals with a death SNOMED code only (and no ONS or TPP death date) are not
# currently included in the dataset definition. Coded deaths are only added as a variable among 
# those already in the dataset (with ONS or TPP death date).
#
# - year_pref_ONS_TPP_plus_codes - prioritises ONS date, then TPP date, then coded date
# - TPP_date_or_coded - classifies TPP death information availability (date, SNOMED code, both)
# - death_source_TPP_date_or_coded - clasif source including any TPP death evidence (dated and/or coded)

add_tpp_date_or_coded_vars <- function(data) {
  data |>
    mutate(
      # Preferred death year: ONS > TPP dated > TPP coded
      death_date_ref_year_w_tpp_codes = case_when(
        !is.na(ons_death_date)        ~ year(ons_death_date),
        !is.na(tpp_death_date)        ~ year(tpp_death_date),
        !is.na(tpp_coded_death_date)  ~ year(tpp_coded_death_date),
        TRUE ~ NA_real_
      ),
      
      # TPP death information availability
      tpp_date_or_coded = case_when(
        !is.na(tpp_death_date)       &  is.na(tpp_coded_death_date) ~ "tpp_dated_only",
        is.na(tpp_death_date)       & !is.na(tpp_coded_death_date) ~ "tpp_coded_only",
        !is.na(tpp_death_date)       & !is.na(tpp_coded_death_date) ~ "tpp_dated_and_coded",
        is.na(tpp_death_date)       &  is.na(tpp_coded_death_date) ~ NA_character_
      ) |>
        factor(
          levels = c("tpp_dated_only","tpp_coded_only","tpp_dated_and_coded")
          ),
      
      # Sensitivity source: any TPP death evidence (dated or coded)
      death_source_tpp_date_or_coded = cat_death_source(
        ons_death_date,
        coalesce(tpp_death_date, tpp_coded_death_date)
      )
    )
}


# ---------------------------------------------------------
# Add date-of-death difference variables (TPP vs ONS)
# ---------------------------------------------------------
# - diff_dod: difference in days (TPP - ONS)
#   > positive values: TPP death date after ONS
#   > negative values: TPP death date before ONS
# - dod_diff_groups: categorised difference in days (ordered factor) for tables/plots

add_dod_diff_vars <- function(data) {

  data |>
    mutate(
      # difference in days between TPP and ONS death dates
      diff_dod = as.numeric(tpp_death_date - ons_death_date),

      # grouped differences
      dod_diff_groups = case_when(
        is.na(diff_dod) ~ NA_character_,

        diff_dod == 0 ~ "0",

        diff_dod >= 1  & diff_dod <= 7  ~ "1-7",
        diff_dod >= 8  & diff_dod <= 30 ~ "8-30",
        diff_dod >= 31                  ~ "31+",

        diff_dod <= -1 & diff_dod >= -7  ~ "-1 to -7",
        diff_dod <= -8 & diff_dod >= -30 ~ "-8 to -30",
        diff_dod <= -31                  ~ "-31+"
      ),

      # convert to ordered factor for consistent display
      dod_diff_groups = factor(
        dod_diff_groups,
        levels = c("-31+", "-8 to -30", "-1 to -7", "0", "1-7", "8-30", "31+"),
        ordered = TRUE
      )
    )
}

# ---------------------------------------------------------
# Demographic vars
# ---------------------------------------------------------
add_demographic_vars <- function(data) {
  data |>
    mutate(
      age_band = cut(
        age,
        breaks = c(-Inf, 45, 55, 65, 75, 85, Inf),
        labels = c("0-44", "45-54", "55-64", "65-74", "75-84", "85+"),
        right = FALSE
      ),
      
      imd_quintile = cut(
        imd,
        breaks = c(0, 32844 * (1:5) / 5),
        labels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"),
        include.lowest = TRUE,
        right = FALSE
      ),
      
      rural_urban = factor(
        rural_urban,
        levels = 1:8,
        labels = c(
          "Urban major conurbation",
          "Urban minor conurbation",
          "Urban city and town",
          "Urban city and town in a sparse setting",
          "Rural town and fringe",
          "Rural town and fringe in a sparse setting",
          "Rural village and dispersed",
          "Rural village and dispersed in a sparse setting"
        ),
        ordered = TRUE
      )
    )
}