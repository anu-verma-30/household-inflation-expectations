# ----------------------------------------------------------------------------
# Title: Main Analysis Pipeline
# Description: Merges expectation data, plots inflation vs. expectations,
#              and estimates baseline OLS regressions.
# Author: Anu Verma
# Last updated: 2025-04-16
# ----------------------------------------------------------------------------

# --- Install and load required packages ---
install.packages(c("tidyverse", "zoo", "moments", "pracma", "readr",
                   "fredr", "wesanderson", "scales", "lubridate", "broom"))

library(tidyverse)
library(dplyr)
library(tidyr)
library(moments)
library(pracma)
library(zoo)
library(readr)
library(fredr)
library(wesanderson)
library(scales)
library(lubridate)
library(broom)

# --- ACTUAL INFLATION VS EXPECTATIONS TIME SERIES ---

# --- Load data ---
cpi <- read_csv("data/cpi_yoy.csv")
msurvey <- read_csv("data/Msurvey_1y_infl_exp.csv", skip = 1)
mkt <- read_csv("data/mkt_inflation_cleaned.csv")
moments <- read_csv("data/moments.csv")

# --- Clean CPI data ---
cpi_clean <- cpi %>%
  mutate(date = ymd(date)) %>%
  filter(!is.na(yoy_inflation)) %>%
  select(date, observed_inflation = yoy_inflation)

# --- Clean Michigan Survey data ---
msurvey_clean <- msurvey %>%
  rename_with(tolower) %>%
  rename(median = median) %>%
  filter(!is.na(median)) %>%
  mutate(date = make_date(year, month, 1)) %>%
  select(date, michigan_median = median) %>%
  mutate(date = date + months(12))

# --- Clean Market Expectations data ---
mkt_clean <- mkt %>%
  rename(expected_inflation = `1 year Expected Inflation`) %>%
  mutate(date = mdy(`Model Output Date`)) %>%
  filter(!is.na(expected_inflation)) %>%
  mutate(expected_inflation = expected_inflation * 100) %>%
  mutate(date = floor_date(date, "month") + months(12)) %>%
  select(date, mkt_expectation = expected_inflation)

# --- Plot comparing Actual Inflation v/s Household Expectation v/s Mkt-based expectation ---
merged <- full_join(cpi_clean, msurvey_clean, by = "date") %>%
  full_join(mkt_clean, by = "date") %>%
  arrange(date)

infl_comp <- ggplot(merged, aes(x = date)) +
  geom_line(aes(y = observed_inflation, color = "Observed Inflation"), size = 0.8) +
  geom_line(aes(y = michigan_median, color = "Median Michigan Survey Expectations"), size = 0.8) +
  geom_line(aes(y = mkt_expectation, color = "Market-based Inflation Expectations"), size = 0.8) +
  scale_color_manual(values = wes_palette("GrandBudapest1", 3), name = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    axis.text = element_text(color = "gray20", size = 12),
    axis.title = element_text(color = "gray20", size=12),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(
    title = "1-Year Ahead Inflation Expectations vs. Realized Inflation",
    x = "Year",
    y = "Inflation Rate (%)"
  )

print(infl_comp)
ggsave("figures/inflation_expectations_comparison.png", plot = infl_comp, width = 10, height = 6, dpi = 300)


# --- INFLATION EXPECTATION MOMENTS TIME SERIES ---

# --- Clean and merge moments ---
moments_clean <- moments %>%
  rename_with(tolower) %>%
  mutate(date = make_date(year, month, 1)) %>%
  select(date, variance, skewness) %>%
  mutate(
    skewness_scaled = skewness * 10,
    date = date + months(12)
  )

merged_moments <- full_join(cpi_clean, msurvey_clean, by = "date") %>%
  full_join(moments_clean, by = "date") %>%
  filter(date >= as.Date("2015-01-01"))

infl_moments_plot <- ggplot(merged_moments, aes(x = date)) +
  geom_line(aes(y = observed_inflation, color = "Observed Inflation"), size = 1) +
  geom_line(aes(y = michigan_median, color = "Median Expectation"), size = 1) +
  geom_line(aes(y = skewness_scaled, color = "Skewness of Expectations (\u00d710)"), size = 1) +
  geom_line(aes(y = variance), size = 1, color = wes_palette("GrandBudapest1", 4)[4]) +
  scale_y_continuous(
    name = "Observed Inflation / Expectation Moments",
    sec.axis = sec_axis(~., name = "Variance of Expectations")
  ) +
  scale_color_manual(
    values = wes_palette("GrandBudapest1", 4)[1:3],
    name = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    axis.text = element_text(color = "gray20", size = 12),
    axis.title.y = element_text(color = "gray20", size = 12),
    axis.title.y.right = element_text(color = "gray20", size = 12),
    axis.title.x = element_text(color = "gray20", size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  labs(title = "Observed Inflation vs. Expectation Moments (1-Year Ahead)", x = "Year")

print(infl_moments_plot)
ggsave("figures/inflation_moments.png", infl_moments_plot, width = 10, height = 6, dpi = 300)

# --- Regression setup ---

reg_target <- cpi_clean %>%
  mutate(future_inflation = lead(observed_inflation, 12)) %>%
  select(date, future_inflation)

msurvey_reg <- msurvey_clean %>% mutate(date = date - months(12))
moments_reg <- moments_clean %>% mutate(date = date - months(12))

predictors <- msurvey_reg %>%
  full_join(moments_reg %>% select(date, variance, skewness), by = "date") %>%
  left_join(cpi_clean, by = "date") %>%
  rename(
    current_inflation = observed_inflation,
    michigan_median = michigan_median
  )

reg_df <- predictors %>%
  inner_join(reg_target, by = "date") %>%
  filter(!is.na(future_inflation), !is.na(current_inflation),
         !is.na(michigan_median), !is.na(variance), !is.na(skewness))

# --- Baseline OLS regression ---
ols_model <- lm(future_inflation ~ current_inflation + michigan_median + variance + skewness, data = reg_df)
summary(ols_model)
tidy(ols_model)

# --- Reduced model with only median ---
ols_model_reduced <- lm(future_inflation ~ current_inflation + michigan_median, data = reg_df)
summary(ols_model_reduced)
tidy(ols_model_reduced)

# --- Subsample regression (2019-2022) ---
reg_df_2019 <- reg_df %>%
  filter(date >= as.Date("2019-01-01"), date <= as.Date("2022-12-01"))

ols_model_2019 <- lm(future_inflation ~ current_inflation + michigan_median + variance + skewness, data = reg_df_2019)
summary(ols_model_2019)
