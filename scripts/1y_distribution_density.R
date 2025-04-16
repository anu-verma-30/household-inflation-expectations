# ----------------------------------------------------------------------------
# Title: 1-Year Inflation Expectation Distribution Density
# Description: Generates smoothed density plot of 1-year ahead inflation
#              expectations from Michigan survey microdata.
# Author: Anu Verma
# Last updated: 2025-04-16
# ----------------------------------------------------------------------------

# --- Install and load required packages ---
install.packages(c("tidyverse", "zoo", "moments", "pracma", "readr", "wesanderson"))
library(tidyverse)
library(dplyr)
library(tidyr)
library(moments)
library(pracma)
library(zoo)
library(readr)
library(wesanderson)

# --- Load and clean data ---
Msurvey <- read_csv("data/Msurvey_1y_infl_exp.csv", skip = 1)

colnames(Msurvey)[c(3:11)] <- c(
  "inf_down", "inf_0", "inf_1to2", "inf_3to4", "inf_5",
  "inf_6to9", "inf_10to14", "inf_15plus", "inf_dontknowup"
)
colnames(Msurvey)[12] <- "inf_dontknow"

# --- Process survey responses into long format ---
df <- Msurvey %>%
  mutate(
    inf_lowerbound = 0,
    inf_upperbound = 0,
    mean_positive = ifelse(
      (inf_1to2 + inf_3to4 + inf_5 + inf_6to9 + inf_10to14 + inf_15plus) > 0,
      (1.5 * inf_1to2 + 3.5 * inf_3to4 + 5 * inf_5 +
       7.5 * inf_6to9 + 12 * inf_10to14 + 17 * inf_15plus) /
      (inf_1to2 + inf_3to4 + inf_5 + inf_6to9 + inf_10to14 + inf_15plus),
      NA_real_
    )
  ) %>%
  select(-inf_dontknow) %>%
  pivot_longer(cols = starts_with("inf"), names_to = "x", values_to = "freq") %>%
  mutate(x = as.numeric(recode(x,
    inf_lowerbound = "-5",
    inf_down = "-2",
    inf_0 = "0",
    inf_1to2 = "1.5",
    inf_3to4 = "3.5",
    inf_5 = "5",
    inf_6to9 = "7.5",
    inf_10to14 = "12",
    inf_15plus = "17",
    inf_upperbound = "20",
    inf_dontknowup = as.character(mean_positive)
  ))) %>%
  group_by(Year, Month) %>%
  mutate(sum = sum(freq), y = freq / sum) %>%
  select(-c(sum, freq)) %>%
  mutate(
    date = zoo::as.Date(zoo::yearmon(Year + ((Month - 1)/12))),
    group = case_when(
      Year == 2019 & Month == 12 ~ 1,
      Year == 2021 & Month == 4 ~ 2,
      Year == 2022 & Month == 8 ~ 3,
      TRUE ~ 0
    )
  )

# --- Styling for groups ---
group_labels <- tibble(
  group = c(1, 2, 3),
  label = c("Dec-2019", "Apr-2020", "Aug-2022"),
  color = wes_palette("GrandBudapest1", 3),
  linetype = c("solid", "solid", "solid")
)

df_subset <- df %>%
  filter(group > 0) %>%
  left_join(group_labels, by = "group")

# --- Plot ---
plot_1y_distribution <- ggplot(df_subset, aes(x = x, y = y, color = label, linetype = label)) +
  geom_smooth(se = FALSE, size = 1, span = 0.8) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "gray40", size = 0.8) +
  annotate("text", x = 2.5, y = 0.18, label = "2% Inflation Target", hjust = 0, size = 4, color = "gray40") +
  scale_color_manual(values = setNames(group_labels$color, group_labels$label)) +
  scale_linetype_manual(values = setNames(group_labels$linetype, group_labels$label)) +
  scale_x_continuous(breaks = seq(-5, 20, 5), limits = c(-5, 20)) +
  scale_y_continuous(limits = c(0, 0.2)) +
  labs(
    title = "1-Year Ahead Inflation Expectation Distribution Density",
    x = "Expected Inflation (%)",
    y = "Density",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_line(color = "white"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    axis.text = element_text(color = "gray20", size = 12),
    axis.title = element_text(color = "gray20", size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

print(plot_1y_distribution)

ggsave("figures/1y_inflation_density.png", plot_1y_distribution, width = 10, height = 6, dpi = 300)
