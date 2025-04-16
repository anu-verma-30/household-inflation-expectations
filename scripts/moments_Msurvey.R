# ----------------------------------------------------------------------------
# Title: Moments from Michigan Survey
# Description: Computes median, variance, skewness, and other moments
#              from 1-year-ahead inflation expectations survey microdata.
# Author: Anu Verma
# Last updated: 2025-04-16
# ----------------------------------------------------------------------------

# --- Install and load required packages ---
install.packages(c("tidyverse", "zoo", "moments", "pracma", "readr"))
library(tidyverse)
library(dplyr)
library(tidyr)
library(moments)
library(pracma)
library(zoo)
library(readr)

# --- Load the data ---
Msurvey <- read_csv("data/Msurvey_1y_infl_exp.csv", skip = 1)

colnames(Msurvey)[c(3:11)] <- c(
  "inf_down", "inf_0", "inf_1to2", "inf_3to4", "inf_5",
  "inf_6to9", "inf_10to14", "inf_15plus", "inf_dontknowup"
)
colnames(Msurvey)[12] <- "inf_dontknow"

# --- Reshape survey data ---
df_moments <- Msurvey %>%
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
  mutate(date = zoo::as.Date(zoo::yearmon(Year + ((Month - 1)/12))))

# --- Prepare month-year vector ---
month_year_vec <- df_moments %>% distinct(Year, Month)

# --- Initialize moment storage vectors ---
mu_vector <- c()
var_vector <- c()
median_vector <- c()
sigma_vector <- c()
skew_vector <- c()
kurt_vector <- c()
q25_vector <- c()
q75_vector <- c()
IQR_vector <- c()

# --- Loop through months and compute moments ---
for (i in 1:nrow(month_year_vec)) {
  yr <- month_year_vec$Year[i]
  mn <- month_year_vec$Month[i]

  p1 <- df_moments %>% filter(Year == yr, Month == mn) %>% arrange(x)

  if (nrow(p1) > 0 && all(!is.na(p1$y)) && sum(p1$y) > 0) {
    p1$y <- p1$y / sum(p1$y)

    mu <- sum(p1$x * p1$y)
    var <- sum((p1$x - mu)^2 * p1$y)
    sigma <- sqrt(var)

    cdf <- cumsum(p1$y)
    median <- p1$x[which(cdf >= 0.5)[1]]
    q25 <- p1$x[which(cdf >= 0.25)[1]]
    q75 <- p1$x[which(cdf >= 0.75)[1]]
    IQR <- q75 - q25

    if (sigma > 0) {
      skew <- sum((((p1$x - mu) / sigma)^3) * p1$y)
      kurt <- sum((((p1$x - mu) / sigma)^4) * p1$y)
    } else {
      skew <- NA
      kurt <- NA
    }

    mu_vector <- c(mu_vector, mu)
    var_vector <- c(var_vector, var)
    median_vector <- c(median_vector, median)
    sigma_vector <- c(sigma_vector, sigma)
    skew_vector <- c(skew_vector, skew)
    kurt_vector <- c(kurt_vector, kurt)
    q25_vector <- c(q25_vector, q25)
    q75_vector <- c(q75_vector, q75)
    IQR_vector <- c(IQR_vector, IQR)
  }
}

# --- Create final output dataframe ---
moments <- tibble(
  Year = month_year_vec$Year,
  Month = month_year_vec$Month,
  mean = mu_vector,
  variance = var_vector,
  median = median_vector,
  std_dev = sigma_vector,
  skewness = skew_vector,
  kurtosis = kurt_vector,
  q25 = q25_vector,
  q75 = q75_vector,
  IQR = IQR_vector
)

# --- Save output ---
write_csv(moments, "data/moments.csv")
