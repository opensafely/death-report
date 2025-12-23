###################################################
# This script creates (...)
#
# Author: Martina Pesce / Andrea Schaffer
#   Bennett Institute for Applied Data Science
#   University of Oxford, 2025
###################################################

library(tidyverse)
library(lubridate)
library(glue)
library(here)

## Create output directory
output_dir <- here::here("output", "report", "deciles")
fs::dir_create(output_dir)

# Import processed data ----
dataset0 <- read_csv("output/measures/measures_by_practice.csv")


# Rounding functionn ----------------

rounding <- function(vars) {
  case_when(vars == 0 ~ 0,
            vars > 7 ~ round(vars / 5) * 5)
}



# Prepare data --------------------------------------------------------
dataset_measure_practice <- dataset0 %>%
  mutate(
    data_source = str_extract(measure, ".*(?=_mortality)"),
    year        = as.integer(lubridate::year(interval_start)),
    numerator   = rounding(numerator),
    denominator = rounding(denominator)
  ) %>%
  select(data_source, year, numerator, denominator, practice) %>%
  pivot_longer(c(numerator, denominator),
               names_to = "metric",
               values_to = "value") %>%
  unite("source_metric", data_source, metric) %>%
  pivot_wider(names_from = source_metric, values_from = value) %>%
  mutate(GP_global_perc = case_when(
    is.na(GP_numerator) | is.na(global_numerator) & (GP_numerator < 1 | global_numerator <1)  ~ NA_real_,
    TRUE ~ GP_numerator / global_numerator * 100
  )) 

# Deciles table + plot--------------------

# Deciles (10:90)---
probs       <- seq(0.1, 0.9, by = 0.1)
percentiles <- as.integer(probs * 100)

#Table-----
# Practices with > 1000 and finite ratio
base <- dataset_measure_practice %>%
  filter(global_denominator > 1000, is.finite(GP_global_perc))

# Count contributing practices per year ----
n_by_year <- base %>%
  group_by(year) %>%
  summarise(n_practices = rounding(n_distinct(practice)), .groups = "drop")

# Deciles per year
df_quantiles <- base %>%
  group_by(year) %>%
  summarise(
    value      = list(as.numeric(quantile(GP_global_perc, probs = probs, type = 3, na.rm = TRUE))),
    percentile = list(percentiles),
    .groups = "drop"
  ) %>%
  unnest(c(percentile, value)) %>%
  left_join(n_by_year, by = "year") %>%
  mutate(
    line_group = factor(if_else(percentile == 50, "median", "decile"),
                        levels = c("decile", "median"))
  ) %>%
  arrange(year, percentile)

write.csv(df_quantiles, fs::path(output_dir, glue("df_quantiles.csv")))

# 4) Plot (median bold solid; deciles thin dashed)
practice_deciles_TPP_perc <- ggplot(df_quantiles,
       aes(x = year, y = value, group = percentile,
           linetype = line_group, color = line_group, linewidth = line_group)) +
  geom_line() +
  scale_color_manual(values   = c(decile = "grey60", median = "black")) +
  scale_linetype_manual(values = c(decile = "dashed", median = "solid")) +
  scale_linewidth_manual(values = c(decile = 0.3,      median = 0.9)) +
  scale_x_continuous(breaks = sort(unique(df_quantiles$year))) +
  labs(
    title   = "GP / Global ratio by decile (across practices)",
    subtitle= paste0("Practices per year: ",
                     paste(with(n_by_year, paste0(year, "=", n_practices)), collapse = "  ·  ")),
    x = "Year", y = "Ratio", color = NULL, linetype = NULL, linewidth = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fs::path(output_dir, glue("practice_deciles_TPP_perc.png")), plot = practice_deciles_TPP_perc,
       width = 11, height = 4, dpi = 300)

#------------------------------------
# Table: practices with 0 deaths and less than 1000 people
pract_deaths_population <- dataset_measure_practice %>%
  group_by(year) %>%
  summarise(
    practices_total = rounding(n_distinct(practice)),
    practices_den_less_1000 = rounding(sum(is.na(global_denominator) | global_denominator < 1000)),
    practices_zero_deaths   = rounding(sum(global_numerator == 0L, na.rm = TRUE)),
    practices_den_gt_1000_zero_deaths = rounding(sum((global_denominator > 1000) & (global_numerator == 0L), na.rm = TRUE)),
    .groups = "drop"
  )

write.csv(pract_deaths_population,fs::path(output_dir, glue("pract_deaths_population.csv")))



  