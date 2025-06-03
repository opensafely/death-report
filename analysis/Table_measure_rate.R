library(tidyverse)
library(lubridate)

## Create output directory
output_dir <- here::here("output", "report")
fs::dir_create(output_dir)

# Import processed data ----
dataset0 <- read_csv("output/measures/measures_overall.csv", 
                     col_types = cols(death_place = col_character(), 
                                      rural_urban = col_character(), 
                                      region = col_character()
                                      )
                     )


# Rounding function

rounding <- function(vars) {
  case_when(vars == 0 ~ 0,
            vars > 7 ~ round(vars / 5) * 5)
}



# Prepare data
dataset_measure <- dataset0 %>% 
  select(-ratio) %>% 
  mutate(
    numerator = rounding(numerator),
    denominator = rounding(denominator),
    subgroup_var = str_extract(measure, "(?<=mortality_).*"),
    data_source = str_extract(measure, ".*(?=_mortality)"),
    year = year(interval_start),
#    m_rate_10M = ratio * 10000,
    subgroup_cat = case_when(
      str_detect(measure, "mortality_age_band") ~ as.character(age_band),
      str_detect(measure, "mortality_death_place") ~ as.character(death_place),
      str_detect(measure, "mortality_region") ~ as.character(region),
      str_detect(measure, "mortality_rural_urban") ~ as.character(rural_urban),
      str_detect(measure, "mortality_IMD_q10") ~ as.character(IMD_q10),
      str_detect(measure, "mortality_ethnicity") ~ as.character(ethnicity),
      str_detect(measure, "mortality_sex") ~ as.character(sex),      
      str_detect(measure, "mortality_overall") ~ "overall",
      TRUE ~ NA_character_
    )
  ) %>% 
  select(-(age_band: ethnicity))


# Table overall rate/deaths by year

table_overall <- dataset_measure %>% 
  filter(subgroup_var == "overall") %>% 
  select(year, data_source, numerator, denominator) %>% 
  pivot_longer(cols = c(numerator, denominator),
               names_to = "metric",
               values_to = "value") %>%
  unite("source_metric", data_source, metric) %>%
  pivot_wider(names_from = source_metric,
              values_from = value) # %>% 
  # mutate(
  #   GP_ONS_ratio = GP_numerator/ ONS_numerator
  # )


# Table subgroups by rate/deaths by year
collate_measures_rate_table <- dataset_measure %>% 
  select(subgroup_var, subgroup_cat, year, data_source, numerator, denominator) %>% 
  pivot_longer(cols = c(numerator, denominator),
               names_to = "metric",
               values_to = "value") %>%
  unite("source_metric", data_source, metric) %>%
  pivot_wider(names_from = source_metric,
              values_from = value) # %>% 
  # mutate(
  #   GP_ONS_ratio = GP_numerator / ONS_numerator
  #)

write.csv(collate_measures_rate_table, here::here("output", "report", "collate_measures_rate_table.csv"))

#----------------------------------------------------------------

# # Plot 1: Number of Deaths by source over time
# F1a_death_overall <- ggplot(dataset_measure %>% filter(subgroup_var == "overall"), 
#              aes(x = year, y = numerator, color = data_source)) +
#   geom_line() +
#   labs(title = "Number of Deaths", x = "Year", y = "Deaths", color = "Source") +
#   theme_minimal()
# 
# 
# # Plot 2: Mortality Rate per 10M by source over time
# F1b_mort10M_all <- ggplot(dataset_measure %>% filter(subgroup_var == "overall"), 
#              aes(x = year, y = m_rate_10M, color = data_source)) +
#   geom_line() +
#   labs(title = "Mortality Rate per 10M", x = "Year", y = "Rate", color = "Source") +
#   theme_minimal()
# 
# # Plot 3: GP/ONS Ratio over time
# F1c_rate_all <- ggplot(table_overall, aes(x = year, y = GP_ONS_ratio)) +
#   geom_line(color = "purple") +
# #  geom_point(color = "purple") +
#   labs(title = "GP / ONS Ratio", x = "Year", y = "Ratio") +
#   theme_minimal()
# 
# # Combine all three plots
# # overall_plot <- p1 / p2 / p3 + 
# #   plot_layout(ncol = 1) + 
# #   plot_annotation(title = "# Deaths, Mortality Rate, and GP/ONS Ratio by year")
# 
# 
# 
# 
# ### Plot by subgroup
# 
# 
# 
# # Reshape to long for metric-wise faceting
# # plot_long <- table_by_subgroup %>%
# #   select(GP_numerator, ONS_numerator) %>% 
# #   pivot_longer(cols = c(numerator, m_rate_10M, GP_ONS_ratio),
# #                names_to = "metric", values_to = "value") 
# 
# #------
# 
#     # Plot 1: Number of Deaths by source over time
#     F1a_death_subgroups <- dataset_measure %>%
#     filter(
#       !is.na(numerator),
#       subgroup_var %in% c("age_band", "IMD_q10", "ethnicity")
#     ) %>%
#     ggplot(aes(
#       x = year,
#       y = numerator,
#       color = subgroup_cat,
#       linetype = data_source
#     )) +
#     geom_line(size = 1) +
#     labs(
#       title = "Number of Deaths by Subgroup",
#       x = "Year",
#       y = "Number of Deaths",
#       color = "Subgroup Level",
#       linetype = "Data Source"
#     ) +
#     facet_wrap(~subgroup_var, scales = "free_y", ncol = 1) +
#     theme_minimal() +
#     theme(
#       strip.text = element_text(size = 11, face = "bold"),
#       plot.title = element_text(hjust = 0.5, size = 14),
#       legend.position = "right"
#     )
#   
#   
#   # Plot 2: Mortality Rate per 10M by source over time
#   F1b_mort10M_all <- ggplot(dataset_measure %>% filter(subgroup_var == "overall"), 
#                             aes(x = year, y = m_rate_10M, color = data_source)) +
#     geom_line() +
#     labs(title = "Mortality Rate per 10M", x = "Year", y = "Rate", color = "Source") +
#     theme_minimal()
#   
#   # Plot 3: GP/ONS Ratio over time
#   F1c_rate_all <- ggplot(table_overall, aes(x = year, y = GP_ONS_ratio)) +
#     geom_line(color = "purple") +
#     #  geom_point(color = "purple") +
#     labs(title = "GP / ONS Ratio", x = "Year", y = "Ratio") +
#     theme_minimal()
# 
# 
# # ---------------
# 
# # Global Mortality rate by year, source and subgroup
# # ggplot(dataset, aes(x = year, y = numerator, linetype = data_source, color = subgroup_cat)) +
# #   geom_line() +
# #   labs(x = "Year", y = "Mortality rate × 10,000", line = "Source", color = "Subgroup") +
# #   theme_minimal()
# 
# # --------------
# #   #Measures
# #   # What do we do when 1 of the DoD is in the year studied and the other is not? 
# #   # SHould we count the person dead in both databases or only if it is on the year studied?
# #   #Mortality rate by subgroup
# #   death_rate_by_subgroup <- function(data, subgroup_var) {
# #     subgroup_sym <- rlang::ensym(subgroup_var)  # capture input as symbol
# #     subgroup_name <- rlang::as_label(subgroup_sym)
# #     studied_year <- studied_year
# #     
# #     result <- data %>%
# #       group_by(!!subgroup_sym) %>%
# #       summarise(
# #         subgroup_name = subgroup_name,
# #         studied_year = studied_year,
# #         n = n(),
# #         TPP_death = sum(!is.na(TPP_death_rec)),
# #         TPP_death_rate = sum(!is.na(TPP_death_rec)) / n,
# #         ONS_death = sum(!is.na(ONS_death_rec)),
# #         ONS_death_rate = sum(!is.na(ONS_death_rec)) / n,
# #         ONS_TPP_death = sum(!is.na(ONS_or_TPP)),
# #         ONS_TPP_death_rate = sum(!is.na(ONS_or_TPP)) / n,        
# #       ) %>%
# #       ungroup() %>% 
# #       rename_with(.cols = 1, ~"subgroup_cat")
# #     # Create a variable named after the subgroup
# #     assign(paste0("death_rate_by_", subgroup_name), result, envir = .GlobalEnv)
# #     
# #     return(result)
# #   }
# # 
# # 
# # #global rate
# # global_death_rate <- dataset %>% 
# #   group_by(died_in_year) %>%
# #   summarise(
# #     subgroup_name = "overall",
# #     studied_year = studied_year,
# #     n = population,
# #     TPP_death = sum(!is.na(TPP_death_rec)),
# #     TPP_death_rate = sum(!is.na(TPP_death_rec)) / population,
# #     ONS_death = sum(!is.na(ONS_death_rec)),
# #     ONS_death_rate = sum(!is.na(ONS_death_rec)) / population,
# #     ONS_TPP_death = sum(!is.na(ONS_or_TPP)),
# #     ONS_TPP_death_rate = sum(!is.na(ONS_or_TPP)) / population,        
# #   ) %>% 
# #   filter(
# #     died_in_year == "yes"
# #   ) %>%
# #   ungroup() %>% 
# #   rename_with(.cols = 1, ~"subgroup_cat")
# # 
# # # Death rates by subgroup
# # #Age
# # death_rate_by_subgroup(dataset, age_band)
# # 
# # #Practice
# # death_rate_by_subgroup(dataset, practice)
# # 
# # death_rate_by_subgroup(dataset, region)
# # 
# # death_rate_by_subgroup(dataset, rural_urb_recode)
# # 
# # 
# # # collate death rate
# # 
# # collate_death_rate <- rbind(global_death_rate, death_rate_by_age_band, death_rate_by_practice, death_rate_by_region, death_rate_by_rural_urb_recode)
# # 
# # 
# 
#   
# 
