# Import libraries
library(tidyverse)
library(lubridate)
library(glue)
library(here)
library(patchwork)
library(scales)

#Import data
collate_death_source_table <- read_csv("output/report/collate_death_source_table.csv")

collate_DoD_diff_table <- read_csv("output/report/collate_DoD_diff_table.csv")

# collate_measures_rate_table <- read_csv("output/report/collate_measures_rate_table.csv")

# Create output directory
output_dir <- here("output", "final_visualization")
fs::dir_create(output_dir)


#-----------------------------------------------------------------------------------------
# Figure 1. A: Deaths over time by source
table_by_source <- collate_death_source_table %>% 
  filter(group_var == "general population" & year_pref_ONS > 2018) %>%
  group_by(year_pref_ONS) %>%
  summarise(
    tpp = sum(count[ONS_or_TPP %in% c("TPP", "ONS & TPP")]),
    ons = sum(count[ONS_or_TPP %in% c("ONS", "ONS & TPP")]),
    total = sum(count[ONS_or_TPP %in% c("TPP", "ONS & TPP", "ONS")])
  ) %>%
  pivot_longer(cols = c("tpp", "ons", "total"),
               names_to = "source",
               values_to = "deaths") %>%
  group_by(source) %>%
  arrange(year_pref_ONS) %>%
  filter(cumsum(deaths != 0) > 0) 

# Plot
all_years <- sort(unique(table_by_source$year_pref_ONS))
year_labels <- ifelse(all_years == 2025, "2025*", as.character(all_years))

F1a_general_death_source_total <- ggplot(table_by_source, aes(x = year_pref_ONS, y = deaths, color = source)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Figure 1: Deaths over time by source",
    x = "Year",
    y = "Number of deaths",
    color = "Source"
  ) + scale_y_continuous(
    limits = c(0, NA) )+
  scale_color_manual(values = c(
    "tpp" = "#56B4E9",
    "ons" = "#E69F00",
    "total" = "black"
  )) +
  scale_x_continuous(
    breaks = all_years,
    labels = year_labels  # Add space to the right for text
  ) +
  theme_minimal()
#
# # OLD: Figure 1. B: Death over time by source stacked bars
# # Prepare data
# table_by_source <- collate_death_source_table %>% 
#   filter(group_var == "general population" & year_pref_ONS >2019) %>%
#   group_by(year_pref_ONS, ONS_or_TPP) %>%
#   summarise(deaths = sum(count), .groups = "drop")  
# 
# table_by_source$ONS_or_TPP <- factor(
#   table_by_source$ONS_or_TPP,
#   levels = c("ONS","TPP", "ONS & TPP")
# )
# 
# # Plot stacked bars
# ggplot(table_by_source, aes(x = year_pref_ONS, y = deaths, fill = ONS_or_TPP)) +
#   geom_col() +
#   labs(
#     title = "Figure: Death numbers by source over time",
#     x = "Year",
#     y = "Number of deaths",
#     fill = "Source"
#   ) +
#   theme_minimal()


#Figure 1. B: Death over time by source percentages 

#Prepare data
table_by_source_pct <- collate_death_source_table %>% 
  filter(group_var == "general population" & year_pref_ONS >2019) %>%
  group_by(year_pref_ONS) %>%
  mutate(percentage = count / sum(count) * 100)

table_by_source_pct$ONS_or_TPP <- factor(
  table_by_source_pct$ONS_or_TPP,
  levels = c("TPP", "ONS", "ONS & TPP")  
)

# Plot
all_years <- sort(unique(table_by_source_pct$year_pref_ONS))
year_labels <- ifelse(all_years == 2025, "2025*", as.character(all_years))

F1b_general_death_source_perc <- ggplot(table_by_source_pct, aes(x = year_pref_ONS, y = percentage, fill = ONS_or_TPP)) +
  geom_col() +
  geom_hline(yintercept = 99, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 90, linetype = "dashed", color = "black") +
  annotate("text", x = max(table_by_source_pct$year_pref_ONS) + 0.6, y = 91, label = "90%", hjust = 0, size = 3) +
  annotate("text", x = max(table_by_source_pct$year_pref_ONS) + 0.6, y = 100, label = "99%", hjust = 0, size = 3) +
  labs(
    title = "Figure: Death source distribution by year (percentages)",
    x = "Year",
    y = "Percentage of deaths",
    fill = "Source"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  +
  scale_x_continuous(
    breaks = all_years,
    labels = year_labels  # Add space to the right for text
  ) +
  scale_fill_manual(
    values = c(
      "TPP" = "#56B4E9",
      "ONS" = "#E69F00",
      "ONS & TPP" = "#009E73"
    )
  ) +
  theme_minimal()

#Overal figure
plot_death_by_source <- F1a_general_death_source_total  + F1b_general_death_source_perc
plot_death_by_source


write.csv(table_by_source_pct, fs::path(output_dir, glue("table_by_source_pct.csv")))
write.csv(table_by_source, fs::path(output_dir, glue("table_by_source_total.csv")))
ggsave(fs::path(output_dir, glue("plot_death_by_source.png")), plot = plot_death_by_source,
       width = 11, height = 4, dpi = 300)

# Figure 2: death day difference-----------------------------------------------
# Prepare data
#
table_DoD_general <- collate_DoD_diff_table %>% 
  filter(group_var == "general population") %>% 
  mutate(
    proportion = count_by_group_DoD / GP_ONS_annual_deaths*100
  )

table_DoD_general$DoD_groups <- factor(
  table_DoD_general$DoD_groups,
  levels = c(
    "-32+",
    "-8 to -31",
    "-1 to -7",
    "0",
    "1-7",
    "8-31",
    "32+"
  )
)

# # barplot
# 
# ggplot(table_DoD_general, aes(x = DoD_groups, y = proportion, fill = DoD_groups)) +
#   geom_bar(stat = "identity") +
#   geom_text(data = subset(table_DoD_general, DoD_groups == "0"),
#             aes(label = round(proportion, 2)),
#             vjust = -0.3, size = 3) +
#   scale_fill_manual(values = c(
#     "-1 to -7" = "#56B4E9", "-8 to -31" = "#56B4E9", "-32 to -365" = "#56B4E9", "-366+" = "#56B4E9",
#     "0" = "#009E73",
#     "1-7" = "#E69F00", "8-31" = "#E69F00", "32-365" = "#E69F00", "366+" = "#E69F00"
#   )) +
#   facet_wrap(~ year_pref_ONS, scales = "free_y") +
#   labs(x = "DoD group", y = "Proportion", title = "Proportion by DoD group and Year") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


# option 2:

plot_same_DoD <- table_DoD_general %>%
  filter(DoD_groups == "0" & year_pref_ONS < 2025) %>%
  ggplot(aes(x = year_pref_ONS, y = proportion)) +  # ← numeric x
  geom_bar(stat = "identity", fill = "#009E73", width = 0.3) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  annotate("text", x = 2024.2, y = 98, label = "95%", hjust = 0, size = 3) +
  geom_text(aes(label = round(proportion, 2)), vjust = -0.3, size = 3) +
  labs(x = "Year", y = "Percentage", title = "Deaths with same date in ONS and TPP (%), by year") +
  theme_minimal()

plot_other_DoD <- table_DoD_general %>%
  filter(DoD_groups != "0" & year_pref_ONS < 2025) %>%
  ggplot(aes(x = DoD_groups, y = proportion, fill = DoD_groups)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 4.5, linetype = "dashed") +  # separates negative from positive
  geom_text(data = subset(table_DoD_general, DoD_groups == "-1 to -7" & year_pref_ONS < 2025),
            aes(label = round(proportion, 2)),
            vjust = -0.3, size = 3) +
  geom_text(data = subset(table_DoD_general, DoD_groups == "1-7" & year_pref_ONS < 2025),
            aes(label = round(proportion, 2)),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "-1 to -7" = "#56B4E9", "-8 to -31" = "#56B4E9", "-32+" = "#56B4E9",
    "1-7" = "#E69F00", "8-31" = "#E69F00", "32+" = "#E69F00"
  )) +
  facet_wrap(~ year_pref_ONS, nrow =1) +
  labs(x = "DoD group", y = "Percentage", title = 'Deaths with different date in ONS and TPP (%), by year') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1), legend.position='none'
  )
  
plot_DoD_general <-plot_same_DoD / plot_other_DoD + plot_layout(heights = c(2, 1))

plot_DoD_general

write.csv(table_DoD_general, fs::path(output_dir, glue("table_DoD_general.csv")))
ggsave(fs::path(output_dir, glue("plot_DoD_general.png")), plot = plot_DoD_general,
       width = 7, height = 5, dpi = 300)


# Plot by group ---------------------------------------------------------------------

# 3.A table % TPP of total per subgroup-year

# table % TPP of total per subgroup-year
tpp_share_by_year <- collate_death_source_table %>%
  filter(year_pref_ONS > 2019) %>%
  group_by(group_var, group_value, year_pref_ONS) %>%
  summarise(
    tpp   = sum(count[ONS_or_TPP %in% c("TPP", "ONS & TPP")], na.rm = TRUE),
    total = sum(count[ONS_or_TPP %in% c("TPP", "ONS & TPP", "ONS")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(group_var, group_value) %>%
  arrange(year_pref_ONS, .by_group = TRUE) %>%
  mutate(started = cumsum(replace_na(total, 0) > 0) > 0) %>%
  filter(started) %>%
  mutate(pct_tpp = if_else(total > 0, tpp / total, NA_real_)) %>%
  ungroup()

#Plot function by gorup

subgroup_perc_plot <- function(var_name, data) {
  
  df <- data %>% filter(group_var == var_name)
  
  all_years   <- sort(unique(df$year_pref_ONS))
  year_labels <- as.character(all_years)
  
  temp_plot<- ggplot(df, aes(x = year_pref_ONS, y = pct_tpp, group = group_value, color = group_value)) +
    geom_line(linetype = "dashed") +
    geom_point(size = 2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = all_years, labels = year_labels) +
    coord_cartesian(ylim = c(0.75, 1), clip = "off") +
    labs(
      title = paste0("Share of TPP deaths among total deaths — ", var_name),
      x = "Year (ONS-preferred)",
      y = "% TPP of total",
      color = var_name
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      strip.placement = "outside",
      axis.ticks.x = element_line(),
      legend.position = "bottom"
    ) 
  
  print(temp_plot)
  ggsave(fs::path(output_dir, glue("subgroup_perc_plot_{var_name}.png")), plot = temp_plot, width = 11, height = 4)
}


subgroup_perc_plot("age_band", tpp_share_by_year)
subgroup_perc_plot("ethnicity", tpp_share_by_year)
subgroup_perc_plot("general population", tpp_share_by_year)
subgroup_perc_plot("IMD_q10", tpp_share_by_year)
subgroup_perc_plot("ons_death_place", tpp_share_by_year)
subgroup_perc_plot("region", tpp_share_by_year)
subgroup_perc_plot("rural_urban", tpp_share_by_year)
subgroup_perc_plot("sex", tpp_share_by_year)


#-------------------------------------------------------------------------
# 4.A % same day by subgroup

# 1) Build % same-day dataset (2020+ to match earlier filtering)
same_day_share_by_year <- collate_DoD_diff_table %>%
  filter(DoD_groups == "0", year_pref_ONS > 2019) %>% 
  mutate(
    proportion = count_by_group_DoD / GP_ONS_annual_deaths
  ) 

# 2) Plotting function
subgroup_same_DoD_plot <- function(var_name, data) {
  
  df <- data %>% filter(group_var == var_name)
  
  all_years   <- sort(unique(df$year_pref_ONS))
  year_labels <- as.character(all_years)
  
  temp_plot <- ggplot(df, aes(x = year_pref_ONS, y = proportion, group = group_value, color = group_value)) +
    geom_line(linetype = "dashed") +
    geom_point(size = 2) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    coord_cartesian(ylim = c(0.9, 1), clip = "off") +
    labs(
      title = paste0("Same-day date in ONS and TPP — ", var_name),
      x = "Year (ONS-preferred)",
      y = "% with same day",
      color = var_name
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      strip.placement = "outside",
      axis.ticks.x = element_line(),
      legend.position = "bottom"
    ) 
  
  print(temp_plot)
  ggsave(fs::path(output_dir, glue("subgroup_same_DoD_plot_{var_name}.png")), plot = temp_plot, width = 11, height = 4)
}


#plots by group
subgroup_same_DoD_plot("age_band",same_day_share_by_year)
subgroup_same_DoD_plot("ethnicity",same_day_share_by_year)
subgroup_same_DoD_plot("general population",same_day_share_by_year)
subgroup_same_DoD_plot("IMD_q10",same_day_share_by_year)
subgroup_same_DoD_plot("ons_death_place",same_day_share_by_year)
subgroup_same_DoD_plot("region",same_day_share_by_year)
subgroup_same_DoD_plot("rural_urban",same_day_share_by_year)
subgroup_same_DoD_plot("sex",same_day_share_by_year)


# Extra options ---
## Distribution of proportions by subgroup and source
bar_prop_by_group <- function(data, var_name, year = 2024) {
  df_prop <- data %>%
    filter(year_pref_ONS == year,
           group_var == var_name,
           ONS_or_TPP != "TPP") %>%
    group_by(ONS_or_TPP, group_value) %>%
    summarise(n = sum(count), .groups = "drop_last") %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  # n total by facet
  totals <- df_prop %>%
    group_by(ONS_or_TPP) %>%
    summarise(n_total = sum(n), .groups = "drop")
  
  df_plot <- df_prop %>%
    left_join(totals, by = "ONS_or_TPP") %>%
    mutate(facet_lab = paste0(ONS_or_TPP, " (n = ", scales::comma(n_total), ")"))
  
  temp_plot <- ggplot(df_plot, aes(x = group_value, y = prop, fill = group_value)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = scales::percent(prop, accuracy = 1)),
      vjust = -0.35, size = 3
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.10)) 
    ) +
    facet_wrap(~ facet_lab) +
    labs(
      title = paste0("Distribution of proportions by '", var_name, "' and source (", year, ")"),
      x = "Group value",
      y = "Proporción"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.text.x.top = element_text(hjust = 0),
      axis.text.x.bottom = element_text(hjust = 0),
      strip.text.y.left = element_text(angle = 0, hjust = 1),
      strip.placement = "outside",
      axis.ticks.x = element_line(),
      legend.position = "bottom"
    ) 
  print(temp_plot)
  ggsave(fs::path(output_dir, glue("ditrib_death_subgroup_source_{var_name}.png")), plot = temp_plot, width = 11, height = 4)
}

# Individuales
bar_prop_by_group(collate_death_source_table, var_name = "age_band",          year = 2024)
bar_prop_by_group(collate_death_source_table, var_name = "ethnicity",         year = 2024)
bar_prop_by_group(collate_death_source_table, var_name = "general population",year = 2024)
bar_prop_by_group(collate_death_source_table, var_name = "IMD_q10",           year = 2024)
bar_prop_by_group(collate_death_source_table, var_name = "ons_death_place",   year = 2024)
bar_prop_by_group(collate_death_source_table, var_name = "region",            year = 2024)
bar_prop_by_group(collate_death_source_table, var_name = "rural_urban",       year = 2024)
bar_prop_by_group(collate_death_source_table, var_name = "sex",               year = 2024)

## Bar stack by subgroup and source

bar_stack_100_by_source <- function(data, var_name, year = 2024) {
  df_prop <- data %>%
    filter(year_pref_ONS == year, group_var == var_name) %>%
    group_by(ONS_or_TPP, group_value) %>%
    summarise(n = sum(count), .groups = "drop") %>%
    group_by(ONS_or_TPP) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  temp_plot<- ggplot(df_prop, aes(x = ONS_or_TPP, y = prop, fill = group_value)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "Death source",
      y = "Percentage",
      fill = var_name
      # title=
    ) +
    theme_minimal(base_size = 14) +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x.top = element_text(hjust = 0),
    axis.text.x.bottom = element_text(hjust = 0),
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    strip.placement = "outside",
    axis.ticks.x = element_line(),
    legend.position = "bottom"
  ) 
  print(temp_plot)
  ggsave(fs::path(output_dir, glue("bar_stack_100_by_source_{var_name}.png")), plot = temp_plot, width = 11, height = 4)
}

bar_stack_100_by_source(collate_death_source_table, var_name = "age_band",           year = 2024)
bar_stack_100_by_source(collate_death_source_table, var_name = "ethnicity",          year = 2024)
bar_stack_100_by_source(collate_death_source_table, var_name = "general population", year = 2024)
bar_stack_100_by_source(collate_death_source_table, var_name = "IMD_q10",            year = 2024)
bar_stack_100_by_source(collate_death_source_table, var_name = "ons_death_place",    year = 2024)
bar_stack_100_by_source(collate_death_source_table, var_name = "region",             year = 2024)
bar_stack_100_by_source(collate_death_source_table, var_name = "rural_urban",        year = 2024)
bar_stack_100_by_source(collate_death_source_table, var_name = "sex",                year = 2024)