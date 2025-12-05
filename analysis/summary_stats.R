# Preliminaries ----

# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")
library("skimr")

## Create output directory
output_dir <- here("output", "DoD")
fs::dir_create(output_dir)

# Import processed data ----
DoD_diff <- read_csv("output/dataset_death_date_diff.csv.gz") %>%
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
                                                      
DoD_diff_plus_inc_crit <- DoD_diff %>% 
  filter(
    has_registration == TRUE & # was registered at the beginning of the year the person died
      any_death_during_study == TRUE # died between "2009-01-01" - "2025-06-06" + (deregistration date + 30 days) is after one date of death
  )

DoD_diff_plus_inc_crit_0_dereg_death <- DoD_diff_plus_inc_crit %>% 
  filter(
    death_dereg_diff >= 0)
    
  

# Summary
## Gral
summary_DoD_diff <- skim(DoD_diff)

summary_DoD_diff_plus_inc_crit <- skim(DoD_diff_plus_inc_crit)

summary_DoD_diff_plus_inc_crit_0_dereg_death <- skim(DoD_diff_plus_inc_crit_0_dereg_death)

write.csv(summary_DoD_diff, file = here::here("output", "DoD","summary_stats_DoD.csv"),
          row.names = FALSE)

write.csv(summary_DoD_diff_plus_inc_crit, file = here::here("output", "DoD","summary_DoD_diff_plus_inc_crit.csv"),
          row.names = FALSE)

write.csv(summary_DoD_diff_plus_inc_crit_0_dereg_death, file = here::here("output", "DoD","summary_DoD_diff_plus_inc_crit_0_dereg_death.csv"),
          row.names = FALSE)

## Cat
table_freq <- DoD_diff %>% 
  pivot_longer(cols = c(age_band, ons_death_place:ethnicity), names_to = "subgroup", values_to = "category") %>% 
  group_by(year(min_DoD), subgroup, category) %>% 
  summarise(
    n=n()
  )

table_freq_plus_inc_crit <- DoD_diff_plus_inc_crit %>% 
  pivot_longer(cols = c(age_band, ons_death_place:ethnicity), names_to = "subgroup", values_to = "category") %>% 
  group_by(year(min_DoD), subgroup, category) %>% 
  summarise(
    n=n()
  )

table_freq_plus_inc_crit_0_dereg_death <- DoD_diff_plus_inc_crit_0_dereg_death %>% 
  pivot_longer(cols = c(age_band, ons_death_place:ethnicity), names_to = "subgroup", values_to = "category") %>% 
  group_by(year(min_DoD), subgroup, category) %>% 
  summarise(
    n=n()
  )

write.csv(table_freq, file = here::here("output", "DoD","table_freq_DoD.csv"),
          row.names = FALSE)

write.csv(table_freq_plus_inc_crit, file = here::here("output", "DoD","table_freq_plus_inc_crit.csv"),
          row.names = FALSE)

write.csv(table_freq_plus_inc_crit_0_dereg_death, file = here::here("output", "DoD","table_freq_plus_inc_crit_0_dereg_death.csv"),
          row.names = FALSE)


# Impossible dates of death not release
impossible_dod_month <- DoD_diff %>% 
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
    year_month_min_dod = format(min_DoD, "%Y-%m")
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
  ) %>% 
  group_by(year_month_min_dod, source, DoD_impossible) %>% 
  summarise(
    n = n(),
    .groups = "drop"
  )

write.csv(
  impossible_dod_month,
  here::here("output", "DoD", "impossible_dod_month.csv"),
  row.names = FALSE
)
# # plots
# 
# # Plot histogram faceted by data source
# ## Data prep
# DoD_diff_date_long <- DoD_diff %>%
#   pivot_longer(
#     cols = c(TPP_death_date, ons_death_date, min_DoD),
#     names_to = "source",
#     values_to = "death_date"
#   )

# ## Plot
# DoD_histogram <- ggplot(DoD_diff_date_long, aes(x = death_date)) +
#   geom_histogram() +  
#   facet_wrap(~ source, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "Death Dates by Source", x = "Date", y = "Count")
# 
# 
# # Cat variables bar plots
# ## Data prep
# DoD_diff_cat_long <- DoD_diff %>%
#   pivot_longer(
#     cols = c(ons_death_place, region, rural_urban, age_band, IMD_q10, ethnicity),
#     names_to = "variable",
#     values_to = "category"
#   )
# 
# # Bar plots subcat
# DoD_diff_cat_bar_plot <- ggplot(DoD_diff_cat_long, aes(x = category)) +
#   geom_bar(aes(fill=year)) +
#   facet_wrap(~ variable, scales = "free_x") +
#   theme_minimal() +
#   labs(title = "Counts by subgroup", 
#        x = "Category", y = "Count") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))




