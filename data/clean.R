# 01_clean.R

library(tidyverse)
library(janitor)

raw_path <- "data/raw/arabica_data_cleaned.csv"
out_path <- "data/processed/coffee_clean.csv"

# clean_names() turns "Country.of.Origin" -> country_of_origin and tidies the
# unnamed row-index column that jldbc's file carries.
raw <- read_csv(raw_path, show_col_types = FALSE) |>
  clean_names()

#the 11 sensory scores (0-10)
sensory_vars <- c("aroma", "flavor", "aftertaste", "acidity", "body",
                  "balance", "uniformity", "clean_cup", "sweetness",
                  "cupper_points", "total_cup_points")
coffee <- raw |>
  select(
    country_of_origin, region, variety, processing_method, color,
    harvest_year, altitude_mean_meters,
    all_of(sensory_vars)
  )

# clean
coffee <- coffee |>
  # tidy text fields
  mutate(across(c(country_of_origin, region, variety, processing_method, color),
                ~ str_squish(as.character(.x)))) |>
  mutate(across(where(is.character), ~ na_if(.x, ""))) |>
  # drop the one corrupt all-zero row, and rows missing any sensory score
  filter(total_cup_points > 50) |>
  drop_na(all_of(sensory_vars)) |>
  drop_na(country_of_origin) |>
  # altitude: NA out implausible values (unit confusion gives huge numbers);
  # keep plausible coffee-growing range 1-3000 m
  mutate(altitude_mean_meters = if_else(
    altitude_mean_meters > 0 & altitude_mean_meters <= 3000,
    altitude_mean_meters, NA_real_
  ))

#keep only countries with enough samples for MANOVA / LDA (n >= 30)
big_countries <- coffee |>
  count(country_of_origin) |>
  filter(n >= 30) |>
  pull(country_of_origin)

coffee <- coffee |>
  filter(country_of_origin %in% big_countries) |>
  mutate(
    country_of_origin = factor(country_of_origin),
    processing_method = factor(processing_method),
    variety           = factor(variety),
    color             = factor(color)
  )

#sanity check
cat("Rows after cleaning:", nrow(coffee), "\n")
cat("Countries kept:", nlevels(coffee$country_of_origin), "\n\n")
print(sort(table(coffee$country_of_origin), decreasing = TRUE))
cat("\nSensory variable summary:\n")
print(summary(as.data.frame(coffee[sensory_vars])))
write_csv(coffee, out_path)
