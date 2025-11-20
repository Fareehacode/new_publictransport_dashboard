
library(tidyverse)
library(lubridate)
library(stringr)

raw_dir  <- "data/raw"
proc_dir <- "data/processed"

if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)


pc_file <- file.path(raw_dir, "australian_postcodes.csv")

if (!file.exists(pc_file)) {
  stop("ERROR: australian_postcodes.csv missing in data/raw/")
}

pc_raw <- read_csv(pc_file, show_col_types = FALSE)

postcode_locations <- pc_raw %>%
  filter(state == "NSW") %>%                          # NSW only
  mutate(
    postcode = as.character(postcode),
    lat_best = if_else(!is.na(Lat_precise) & Lat_precise != 0, Lat_precise, lat),
    lon_best = if_else(!is.na(Long_precise) & Long_precise != 0, Long_precise, long)
  ) %>%
  transmute(
    postcode,
    lat = lat_best,
    lon = lon_best
  ) %>%
  filter(!is.na(lat), !is.na(lon), lat != 0, lon != 0) %>%
  distinct(postcode, .keep_all = TRUE) %>%
  arrange(postcode)

write_csv(postcode_locations, file.path(raw_dir, "postcode_locations.csv"))
cat("Saved postcode_locations.csv\n")


parse_opal_date <- function(x) {
  suppressWarnings(parse_date_time(x, orders = c("Ymd", "Y-m-d", "d/m/Y", "Y/m/d")))
}


time_files <- list.files(raw_dir, pattern = "^time_.*\\.csv$", full.names = TRUE)

read_time <- function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      Date = parse_opal_date(date),
      tap_type = case_when(
        str_detect(tolower(tap), "on") ~ "Tap On",
        str_detect(tolower(tap), "off") ~ "Tap Off",
        TRUE ~ "Other"
      )
    ) %>%
    select(mode, Date, tap_type, count) %>%
    filter(!is.na(Date))
}

time_trends <- map_dfr(time_files, read_time) %>%
  group_by(mode, Date, tap_type) %>%
  summarise(TotalTaps = sum(count), .groups = "drop")

write_csv(time_trends, file.path(proc_dir, "time_trends.csv"))


od_files <- list.files(raw_dir, pattern = "^origin_destination_.*\\.csv$", full.names = TRUE)

read_od <- function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(Date = parse_opal_date(date)) %>%
    select(mode, Date, TZname_on, TZname_off, count) %>%
    filter(!is.na(Date))
}

od_clean <- map_dfr(od_files, read_od)
write_csv(od_clean, file.path(proc_dir, "od_clean.csv"))


time_loc_files <- list.files(raw_dir, pattern = "^time-loc_.*\\.csv$", full.names = TRUE)
location_files <- list.files(raw_dir, pattern = "^location_.*\\.csv$", full.names = TRUE)

read_time_loc <- function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      Date = parse_opal_date(date),
      tap_type = case_when(
        str_detect(tolower(tap), "on") ~ "Tap On",
        str_detect(tolower(tap), "off") ~ "Tap Off",
        TRUE ~ "Other"
      ),
      loc = as.character(loc),
      is_postcode = str_detect(loc, "^[0-9]{4}$"),
      postcode = if_else(is_postcode, loc, NA_character_),
      stop_raw = if_else(!is_postcode, loc, NA_character_),
      stop_clean = stop_raw %>% str_replace("\\([0-9]+\\)", "") %>% str_squish()
    ) %>%
    select(mode, Date, tap_type, postcode, stop_clean, count) %>%
    filter(!is.na(Date))
}

read_location <- function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(
      Date = parse_opal_date(date),
      tap_type = case_when(
        str_detect(tolower(tap), "on") ~ "Tap On",
        str_detect(tolower(tap), "off") ~ "Tap Off",
        TRUE ~ "Other"
      ),
      postcode = if_else(str_detect(as.character(loc), "^[0-9]{4}$"),
                         as.character(loc),
                         NA_character_)
    ) %>%
    select(mode, Date, tap_type, postcode, count) %>%
    filter(!is.na(postcode))
}

time_loc <- map_dfr(time_loc_files, read_time_loc)
location_data <- map_dfr(location_files, read_location)

# POSTCODE TAPS
postcode_taps <- bind_rows(
  time_loc %>% filter(!is.na(postcode)),
  location_data
) %>%
  group_by(mode, Date, postcode, tap_type) %>%
  summarise(TotalTaps = sum(count), .groups = "drop")

write_csv(postcode_taps, file.path(proc_dir, "postcode_taps.csv"))

# STOP TAPS (for bar chart + table)
stop_taps <- time_loc %>%
  filter(!is.na(stop_clean)) %>%
  group_by(mode, Date, stop = stop_clean, tap_type) %>%
  summarise(TotalTaps = sum(count), .groups = "drop")

write_csv(stop_taps, file.path(proc_dir, "stop_taps.csv"))

cat("All processing complete ✔\n")
