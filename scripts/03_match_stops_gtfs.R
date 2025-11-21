
library(readr)
library(dplyr)
library(stringr)


opal_stops_raw <- read_csv("data/processed/stop_taps.csv", show_col_types = FALSE) %>%
  distinct(stop) %>%
  filter(!is.na(stop), stop != "")

cat("Unique Opal stops:", nrow(opal_stops_raw), "\n")


gtfs_stops <- read_csv("data/raw/gtfs_stops_clean.csv", show_col_types = FALSE) %>%
  mutate(
    stop_id   = as.character(stop_id),
    stop_name = as.character(stop_name)
  )

cat("GTFS stops:", nrow(gtfs_stops), "\n")


normalise_name <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("\\([^)]+\\)", " ") %>%   
    str_replace_all("_", " ") %>%            
    str_replace_all("-", " ") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%   
    str_squish()
}

opal_stops <- opal_stops_raw %>%
  mutate(
    stop_norm = normalise_name(stop)
  )

gtfs_stops_norm <- gtfs_stops %>%
  mutate(
    stop_name_norm = normalise_name(stop_name)
  )


stop_matches <- opal_stops %>%
  inner_join(
    gtfs_stops_norm,
    by = c("stop_norm" = "stop_name_norm")
  ) %>%
  select(
    stop,              # original Opal stop text
    stop_id,
    gtfs_stop_name = stop_name,
    lat = stop_lat,
    lon = stop_lon,
    location_type,
    parent_station
  )

cat("Matched stops:", nrow(stop_matches), "\n")


out_path <- "data/raw/stop_locations.csv"
write_csv(stop_matches, out_path)

cat("Saved matched stop locations to:", out_path, "\n")


unmatched_stops <- opal_stops %>%
  anti_join(stop_matches, by = "stop") %>%
  arrange(stop)

unmatched_path <- "data/raw/unmatched_stops.csv"
write_csv(unmatched_stops, unmatched_path)

cat("Saved unmatched stops to:", unmatched_path, "\n")

