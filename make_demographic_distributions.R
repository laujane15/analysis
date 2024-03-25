library(tidyverse)
library(stringr)
library(tidycensus)

my_counties <- c("DOUGLAS", "CHELAN", "OKANOGAN")
sex_and_age_codes <-
  c(paste0("B01001", "_00", 1:9), paste0("B01001", "_0", 10:49))

sex_age_names <- load_variables(2020, "acs5", cache = TRUE) |> filter(
  name %in% sex_and_age_codes
)

sex_race_coarse <- list(
  "M_0-19" = paste0("00", 3:7),
  "M_20-29" = c("008", "009", "010", "011"),
  "M_30-39" = paste0("0", 12:13),
  "M_40+" = paste0("0", 14:25),
  "F_0-19" = paste0("0", 27:31),
  "F_20-29" = paste0("0", 32:35),
  "F_30-39" = paste0("0", 36:37),
  "F_40+" = paste0("0", 38:49)
)

sex_race_key <-
  purrr::map2(names(sex_race_coarse), sex_race_coarse, .f = function(x, y) {
  data.frame("name" = x, "variable" = paste0("B01001_", y))
}
) |> do.call(what = bind_rows)

make_marginal_counts <- function(yr_val) {
  sex_and_age <- get_acs(
    geography = "county",
    variables = sex_and_age_codes,
    state = "WA",
    year = yr_val,
    geometry = FALSE
  )
  sex_and_age_margins <-
    sex_and_age |> left_join(sex_race_key, by = "variable") |>
    # Some age groups are not included and have NA for name.  These are removed.
    filter(!is.na(name)) |>
    # Group by the desired sex and age ranges and create totals:
    group_by(NAME, GEOID, name) |> summarise(count = sum(estimate)) |>
    # Get proportions
    group_by(NAME, GEOID) |> mutate(prop = count / sum(count))
  ## Above sex and age are a single variable, here, I split them into
  ## two variables
  split_names <- sex_and_age_margins$name |> strsplit("_") |>
    purrr::list_transpose()
  sex_and_age_margins$sex <- split_names[[1]]
  sex_and_age_margins$age <- split_names[[2]]
  county_names <- sex_and_age_margins$NAME |> strsplit("County") |>
    map(.f = toupper) |> purrr::list_transpose()
  sex_and_age_margins$cnty <- county_names[[1]] |> stringr::str_trim("right")
  sex_distribution <- sex_and_age_margins |> group_by(cnty, sex) |>
    summarise(count = sum(count))
  age_distribution <- sex_and_age_margins |> group_by(cnty, age) |>
    summarise(count = sum(count))
  ## Hispanic or Latino counts
  hispanic_counts <- get_acs(
    geography = "county",
    variables =  c("B01001_001", # Total
                   "B01001I_001"), # Count of just hispanics
    state = "WA",
    year = yr_val,
    geometry = FALSE
  )
  county_names <- hispanic_counts$NAME |> strsplit("County") |>
    map(.f = toupper) |> purrr::list_transpose()
  hispanic_counts$cnty <- county_names[[1]] |> stringr::str_trim("right")
  clean_hispanic <-
    hispanic_counts |> select(estimate, cnty, variable) |>
    pivot_wider(id_cols = cnty, names_from = "variable",
                values_from = "estimate") |>
    mutate("hispanic" = B01001I_001,
           "non_hispanic" = B01001_001 - hispanic) |>
    select(cnty, hispanic, non_hispanic) |>
    pivot_longer(cols = -cnty, names_to = "hispanic_status",
                 values_to = "count")
  return(list(
    "sex_df" = sex_distribution |> mutate(year = yr_val),
    "age_df" = age_distribution |> mutate(year = yr_val),
    "hispanic_df" = clean_hispanic |> mutate(year = yr_val)
  ))
}

all_years <- map(2018:2021, make_marginal_counts)

dfs_all_years_all_counties <- all_years |> list_transpose() |>
  map(.f = bind_rows)
# Now summing over all years
combined_sex_cnty_df <- dfs_all_years_all_counties[[1]] |>
  filter(cnty %in% my_counties) |>
  group_by(sex, cnty) |>
  summarise(count = sum(count))
combined_age_cnty_df <- dfs_all_years_all_counties[[2]] |>
  filter(cnty %in% my_counties) |>
  group_by(age, cnty) |>
  summarise(count = sum(count))
combined_hispanic_cnty_df <- dfs_all_years_all_counties[[3]] |>
  filter(cnty %in% my_counties) |>
  group_by(hispanic_status, cnty) |>
  summarise(count = sum(count))

# Check to make sure county counts all match
combined_sex_cnty_df |> group_by(cnty) |> summarise(count = sum(count))
combined_age_cnty_df |> group_by(cnty) |> summarise(count = sum(count))
combined_hispanic_cnty_df |> group_by(cnty) |> summarise(count = sum(count))

# Now summing over counties
combined_sex_df <- combined_sex_cnty_df |> group_by(sex) |>
  summarise(count = sum(count))
combined_age_df <- combined_age_cnty_df |> group_by(age) |>
  summarise(count = sum(count))
combined_hispanic_df <- combined_hispanic_cnty_df |>
  group_by(hispanic_status) |>
  summarise(count = sum(count))
