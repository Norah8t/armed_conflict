library(tidyverse)

# Function: read a World Bank mortality file and convert it from wide to long
#   filename: name of the csv file inside data/raw
#   varname:  name to give the mortality column in the output
clean_wb <- function(filename, varname) {
  read.csv(file.path("data", "raw", filename), header = TRUE) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",       # removes X from year column
                 values_to = varname) |>   # column name supplied as an argument
    mutate(year = as.numeric(year)) |>     # change year to numeric
    select(iso, year, all_of(varname))
}

# Apply the function to each World Bank data set
matmor  <- clean_wb("maternal_mortality.csv", "maternal_mortality")
infmor  <- clean_wb("infant_mortality.csv",   "infant_mortality")
neomor  <- clean_wb("neonatal_mortality.csv", "neonatal_mortality")
un5mor  <- clean_wb("under5_mortality.csv",   "under5_mortality")

# Combine the four into one data set (one row per country-year)
wb_mortality <- list(matmor, infmor, neomor, un5mor) |>
  reduce(full_join, by = c("iso", "year"))


library(janitor)

# Read in disaster data and clean variable names
# (e.g., "Disaster Type" -> disaster_type, "ISO" -> iso, "Year" -> year)
disaster <- read.csv(file.path("data", "raw", "disaster.csv"), header = TRUE) |>
  clean_names()


disaster_clean <- disaster |>
  # b. keep years 2000-2019 and only earthquakes and droughts
  filter(between(year, 2000, 2019),
         disaster_type %in% c("Earthquake", "Drought")) |>
  # c. keep only the variables we need
  select(year, iso, disaster_type) |>
  # d. one dummy per disaster type
  mutate(drought    = if_else(disaster_type == "Drought", 1, 0),
         earthquake = if_else(disaster_type == "Earthquake", 1, 0)) |>
  # a country can have several disasters in a year, so collapse to one row
  # per country-year; max() gives 1 if any disaster of that type occurred
  group_by(year, iso) |>
  summarise(drought    = max(drought),
            earthquake = max(earthquake),
            .groups = "drop") |>
  # e. retain final variables
  select(year, iso, earthquake, drought)




# Read in conflict data (variables: conflict_id, iso, year, best)
conflict <- read.csv(file.path("data", "raw", "conflict.csv"), header = TRUE)

conflict_clean <- conflict |>
  # Classify EACH conflict first (paper: thresholds apply per
  # country-conflict-year, not to the country's summed deaths)
  mutate(conf_binary = if_else(best >= 25, 1, 0),
         conf_cat    = case_when(best >= 1000 ~ 2,   # war
                                 best >= 25   ~ 1,   # minor conflict
                                 TRUE         ~ 0)) |> # no conflict
  # a. collapse to one row per country-year: take the most severe conflict
  group_by(iso, year) |>
  summarise(armconf1 = max(conf_binary),
            armconf3 = max(conf_cat),
            totdeath = sum(best, na.rm = TRUE),  # kept for the continuous spec
            .groups = "drop") |>
  # b. lag by one year: conflict in year t is paired with outcomes in year t + 1
  mutate(year = year + 1) |>
  mutate(armconf3 = factor(armconf3, levels = 0:2,
                           labels = c("none", "minor", "war")))


covariates <- read.csv(file.path("data", "raw", "covariates.csv"), header = TRUE) |>
  rename(any_of(c(iso = "ISO")))   # use lowercase iso as the key everywhere

final_data <- covariates |>
  left_join(wb_mortality,   by = c("iso", "year")) |>
  left_join(disaster_clean, by = c("iso", "year")) |>
  left_join(conflict_clean, by = c("iso", "year")) |>
  # country-years absent from the disaster / conflict data had none
  mutate(across(c(drought, earthquake, armconf1, armconf3, totdeath),
                ~ replace_na(.x, 0))) |>
  mutate(armconf3 = factor(armconf3, levels = 0:2,
                           labels = c("none", "minor", "war")))

dir.create(file.path("data", "processed"), showWarnings = FALSE)
write.csv(final_data, file.path("data", "processed", "final_data.csv"),
          row.names = FALSE)
 