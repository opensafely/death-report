


# Import libraries
library("tidyverse")
library("dtplyr")
library("lubridate")
library("glue")
library("here")
library(patchwork)

#Import data
collate_death_source_table <- read_csv("output/report/collate_death_source_table.csv")

collate_DoD_diff_table <- read_csv("output/report/collate_DoD_diff_table.csv")

collate_measures_rate_table <- read_csv("output/report/collate_measures_rate_table.csv")


#Figure 1. A: Deaths over time by source
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
  filter(cumsum(deaths != 0) > 0)  # only keep rows after first non-zero

# Plot
all_years <- sort(unique(table_by_source$year_pref_ONS))
year_labels <- ifelse(all_years == 2025, "2025*", as.character(all_years))

total <- ggplot(table_by_source, aes(x = year_pref_ONS, y = deaths, color = source)) +
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
#---------------------
# # Figure 1. B: Death over time by source stacked bars
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


#Figure 1. B: Death over time by source percentages ------------------------------------------------------

#Prepare data
table_by_source_pct <- collate_death_source_table %>% 
  filter(group_var == "general population" & year_pref_ONS >2019) %>%
  group_by(year_pref_ONS) %>%
  mutate(percentage = count / sum(count) * 100)

table_by_source_pct$ONS_or_TPP <- factor(
  table_by_source_pct$ONS_or_TPP,
  levels = c("TPP", "ONS", "ONS & TPP")  # orden deseado de abajo hacia arriba
)

# Plot
all_years <- sort(unique(table_by_source_pct$year_pref_ONS))
year_labels <- ifelse(all_years == 2025, "2025*", as.character(all_years))

perc <- ggplot(table_by_source_pct, aes(x = year_pref_ONS, y = percentage, fill = ONS_or_TPP)) +
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
plot_death_by_sourece <- total + perc
plot_death_by_sourece

# Figure 2: death day difference
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
    "-366+",
    "-32 to -365",
    "-8 to -31",
    "-1 to -7",
    "0",
    "1-7",
    "8-31",
    "32-365",
    "366+"
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
    "-1 to -7" = "#56B4E9", "-8 to -31" = "#56B4E9", "-32 to -365" = "#56B4E9", "-366+" = "#56B4E9",
    "1-7" = "#E69F00", "8-31" = "#E69F00", "32-365" = "#E69F00", "366+" = "#E69F00"
  )) +
  facet_wrap(~ year_pref_ONS, nrow =1) +
  labs(x = "DoD group", y = "Percentage", title = 'Deaths with different date in ONS and TPP (%), by year') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1), legend.position='none'
  )
  
plot_DoD <-plot_same_DoD / plot_other_DoD + plot_layout(heights = c(2, 1))

plot_DoD


# Plot by group ---------------------------------------------------------------------
# Have a general view
#Figure 1. A: Deaths over time by source
table_by_source <- collate_death_source_table %>% 
  filter(year_pref_ONS > 2018) %>%
  group_by(group_var, group_value, year_pref_ONS) %>%
  summarise(
    tpp = sum(count[ONS_or_TPP %in% c("TPP", "ONS & TPP")]),
    ons = sum(count[ONS_or_TPP %in% c("ONS", "ONS & TPP")]),
    total = sum(count[ONS_or_TPP %in% c("TPP", "ONS & TPP", "ONS")])
  ) %>%
  pivot_longer(cols = c("tpp", "ons", "total"),
               names_to = "source",
               values_to = "deaths") %>%
  group_by(source) %>%
  arrange(group_var, group_value, year_pref_ONS) %>%
  filter(cumsum(deaths != 0) > 0)  # only keep rows after first non-zero

# Plot
all_years <- sort(unique(table_by_source$year_pref_ONS))
year_labels <- ifelse(all_years == 2025, "2025*", as.character(all_years))

total <- ggplot(table_by_source, aes(x = year_pref_ONS, y = deaths, color = source)) +
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
  facet_wrap(group_var ~ group_value, scales = "free_y")+
  theme_minimal()



# plot 2
# Figure 2: death day difference
# Prepare data
#
table_DoD <- collate_DoD_diff_table %>% 
  mutate(
    proportion = count_by_group_DoD / GP_ONS_annual_deaths*100
  )

table_DoD$DoD_groups <- factor(
  table_DoD$DoD_groups,
  levels = c(
    "-366+",
    "-32 to -365",
    "-8 to -31",
    "-1 to -7",
    "0",
    "1-7",
    "8-31",
    "32-365",
    "366+"
  )
)




plot_same_DoD <- table_DoD %>%
  filter(DoD_groups == "0" & year_pref_ONS < 2025) %>%
  ggplot(aes(x = year_pref_ONS, y = proportion)) +  # ← numeric x
  geom_bar(stat = "identity", fill = "#009E73", width = 0.3) +
  geom_hline(yintercept = 95, linetype = "dashed") +
  annotate("text", x = 2024.2, y = 98, label = "95%", hjust = 0, size = 3) +
  geom_text(aes(label = round(proportion, 2)), vjust = -0.3, size = 3) +
  labs(x = "Year", y = "Percentage", title = "Deaths with same date in ONS and TPP (%), by year") +
  facet_wrap(group_var ~ group_value, scales = "free_y")+
  theme_minimal()


