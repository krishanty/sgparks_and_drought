library(tidyverse)
library(jsonlite)
library(here)

start_date <- as.Date("2025-01-01")
end_date <- as.Date("2025-10-31")
output_path <- here("data", "temp2025_janoct.csv")

json_paths <- list.files(here("data", "temp_raw"), pattern = "^\\d{4}-\\d{2}-\\d{2}\\.json$", full.names = TRUE)
json_dates <- as.Date(str_sub(basename(json_paths), 1, 10))
json_paths <- json_paths[json_dates >= start_date & json_dates <= end_date]

read_temp_json <- function(json_path){
    day <- fromJSON(json_path, simplifyVector = TRUE)
    stations <- tibble(
        station_id = day$stations$id,
        station_name = day$stations$name,
        station_device_id = day$stations$deviceId,
        location_longitude = day$stations$location$longitude,
        location_latitude = day$stations$location$latitude
    )
    readings <- tibble(timestamp = day$readings$timestamp, data = day$readings$data) |>
        unnest(data) |>
        rename(station_id = stationId, reading_value = value)

    readings |>
        left_join(stations, by = "station_id") |>
        mutate(
            date = str_sub(timestamp, 1, 10),
            update_timestamp = timestamp,
            reading_update_timestamp = timestamp,
            reading_type = day$readingType,
            reading_unit = day$readingUnit
        ) |>
        select(date, timestamp, update_timestamp, station_id, station_name, station_device_id,
               location_longitude, location_latitude, reading_update_timestamp, reading_value,
               reading_type, reading_unit)
}

temp_janoct <- map(json_paths, read_temp_json, .progress = TRUE) |>
    list_rbind() |>
    arrange(timestamp, station_id)

write_csv(temp_janoct, output_path, na = "")
message(output_path, " written: ", nrow(temp_janoct), " rows from ", length(json_paths), " days.")
