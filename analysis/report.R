# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")


## Create output directory
output_dir <- here("output", "report")
fs::dir_create(output_dir)

# Import processed data ----
dataset0 <- read_csv("output/dataset_2020.csv.gz")

# Create variables
dataset <- dataset0 %>% 
  mutate(
    year = case_when(
      TPP_death == TRUE ~ year(TPP_death_date),
      ONS_death == TRUE ~ year(ons_death_date),
      TRUE ~ NA_real_
    ),
    TPP_death_rec = case_when(
      TPP_death == TRUE ~ "yes",
      TRUE ~ NA_character_
    ),
    ONS_death_rec = case_when(
      ONS_death == TRUE ~ "yes",
      TRUE ~ NA_character_
    ),
    ONS_death_date_rec = case_when(
      ONS_death == TRUE | TPP_death == TRUE ~ ons_death_date,
      TRUE ~ NA_Date_
    ),
    TPP_death_date_rec = case_when(
      ONS_death == TRUE | TPP_death == TRUE ~ TPP_death_date,
      TRUE ~ NA_Date_
    ),
    rural_urb_recode = case_when(
      rural_urban < 5 ~ "urban",
      rural_urban >= 5 ~ "rural",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    dif_TPP_ONS = TPP_death_date_rec - ONS_death_date_rec, 
    died_in_year = case_when(
      TPP_death_rec == "yes" | ONS_death_rec == "yes" ~ "yes",
      TRUE ~ NA_character_),
    ONS_or_TPP = case_when(
      !is.na(ONS_death_date_rec) & !is.na(TPP_death_date_rec) ~ "ONS & TPP",
      !is.na(ONS_death_date_rec) & is.na(TPP_death_date_rec) ~ "ONS",
      !is.na(TPP_death_date_rec) & is.na(ONS_death_date_rec) ~ "TPP",
      is.na(TPP_death_date_rec) & is.na(ONS_death_date_rec) ~ NA_character_
    )
  )

#Add studied year
studied_year <- dataset0 %>%
  filter(TPP_death == TRUE & !is.na(TPP_death_date)) %>%
  summarise(year = year(TPP_death_date[1])) %>%
  pull(year)

#Global popualtion
population <- nrow(dataset)

  #Measures
# What do we do when 1 of the DoD is in the year studied and the oder is not? 
# SHould we count the person dead in both databases or only if it is on the year studied?
#Mortality rate by subgroup
  death_rate_by_subgroup <- function(data, subgroup_var) {
    subgroup_sym <- rlang::ensym(subgroup_var)  # capture input as symbol
    subgroup_name <- rlang::as_label(subgroup_sym)
    studied_year <- studied_year
    
    result <- data %>%
      group_by(!!subgroup_sym) %>%
      summarise(
        subgroup_name = subgroup_name,
        studied_year = studied_year,
        n = n(),
        TPP_death = sum(!is.na(TPP_death_rec)),
        TPP_death_rate = sum(!is.na(TPP_death_rec)) / n,
        ONS_death = sum(!is.na(ONS_death_rec)),
        ONS_death_rate = sum(!is.na(ONS_death_rec)) / n,
        ONS_TPP_death = sum(!is.na(ONS_or_TPP)),
        ONS_TPP_death_rate = sum(!is.na(ONS_or_TPP)) / n,        
      ) %>%
      ungroup() %>% 
      rename_with(.cols = 1, ~"subgroup_cat")
    # Create a variable named after the subgroup
    assign(paste0("death_rate_by_", subgroup_name), result, envir = .GlobalEnv)
    
    return(result)
  }


#global rate
global_death_rate <- dataset %>% 
    group_by(died_in_year) %>%
    summarise(
      subgroup_name = "overall",
      studied_year = studied_year,
      n = population,
      TPP_death = sum(!is.na(TPP_death_rec)),
      TPP_death_rate = sum(!is.na(TPP_death_rec)) / population,
      ONS_death = sum(!is.na(ONS_death_rec)),
      ONS_death_rate = sum(!is.na(ONS_death_rec)) / population,
      ONS_TPP_death = sum(!is.na(ONS_or_TPP)),
      ONS_TPP_death_rate = sum(!is.na(ONS_or_TPP)) / population,        
    ) %>% 
  filter(
    died_in_year == "yes"
  ) %>%
  ungroup() %>% 
  rename_with(.cols = 1, ~"subgroup_cat")

# Death rates by subgroup
#Age
death_rate_by_subgroup(dataset, age_band)

#Practice
death_rate_by_subgroup(dataset, practice)

death_rate_by_subgroup(dataset, region)

death_rate_by_subgroup(dataset, rural_urb_recode)


# collate death rate

collate_death_rate <- rbind(global_death_rate, death_rate_by_age_band, death_rate_by_practice, death_rate_by_region, death_rate_by_rural_urb_recode)

