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
      !is.na(last_registration_end) ~ last_registration_end - min_DoD,
      TRUE ~ as.difftime(NA_real_, units = "days")
    )
  )


# Summary
## Gral
summary_DoD_diff <- skim(DoD_diff)

write.csv(summary_DoD_diff, file = here::here("output", "DoD","summary_stats_DoD.csv"),
          row.names = FALSE)

## Cat
table_freq <- DoD_diff %>% 
  pivot_longer(cols = c(age_band, ons_death_place:ethnicity), names_to = "subgroup", values_to = "category") %>% 
  group_by(year(min_DoD), subgroup, category) %>% 
  summarise(
    n=n()
  )

write.csv(table_freq, file = here::here("output", "DoD","table_freq_DoD.csv"),
          row.names = FALSE)

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




