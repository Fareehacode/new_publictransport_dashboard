
library(readr)
library(dplyr)
library(stringr)
library(utils)


gtfs_dir <- "data/raw/gtfs"


gtfs_zip <- list.files(gtfs_dir, pattern = "\\.zip$", full.names = TRUE)

if (length(gtfs_zip) == 0) {
  stop("No GTFS .zip file found in data/raw/gtfs. Please check the file name and location.")
}

gtfs_zip <- gtfs_zip[1]
cat("Using GTFS file:", gtfs_zip, "\n")


unzipped_dir <- file.path(gtfs_dir, "unzipped")
if (!dir.exists(unzipped_dir)) dir.create(unzipped_dir, recursive = TRUE)


unzip(gtfs_zip, files = "stops.txt", exdir = unzipped_dir)

stops_path <- file.path(unzipped_dir, "stops.txt")
if (!file.exists(stops_path)) {
  stop("stops.txt not found inside the GTFS zip. Check that the GTFS is a standard feed.")
}


stops_raw <- read_csv(stops_path, show_col_types = FALSE)

cat("stops.txt loaded. Rows:", nrow(stops_raw), "\n")

stops_clean <- stops_raw %>%
  mutate(
    stop_id   = as.character(stop_id),
    stop_name = as.character(stop_name)
  ) %>%
  select(any_of(c("stop_id", "stop_name", "stop_lat", "stop_lon", "location_type", "parent_station")))

stops_clean <- stops_clean %>%
  filter(is.na(location_type) | location_type %in% c(0, 1))

out_path <- "data/raw/gtfs_stops_clean.csv"
write_csv(stops_clean, out_path)

cat("Saved cleaned GTFS stops to:", out_path, "\n")
