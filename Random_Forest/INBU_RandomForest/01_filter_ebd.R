library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
# resolve namespace conflicts
select <- dplyr::select

# setup data directory
dir.create("inbu_data", showWarnings = FALSE)

ebd <- auk_ebd("F:/ebd_relSep-2019.txt",
               file_sampling = "F:/ebd_sampling_relSep-2019.txt")

ebd_filters <- ebd %>% 
  auk_species("Indigo Bunting") %>% 
  # southeastern coastal plain bcr
  auk_bcr(bcr = 27) %>% 
  # june, use * to get data from any year
  auk_date(date = c("*-06-01", "*-06-30")) %>% 
  # restrict to the standard traveling and stationary count protocols
  auk_protocol(protocol = c("Stationary", "Traveling")) %>% 
  auk_complete()
ebd_filters


# output files
data_dir <- "inbu_data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
f_ebd <- file.path(data_dir, "ebd_inbu.txt")
f_sampling <- file.path(data_dir, "ebd_checklists.txt")

# only run if the files don't already exist
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}


### Zerofill the data and combine sampling with ebd###

ebd_zf <- auk_zerofill(f_ebd, f_sampling, collapse = TRUE)


# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
ebd_zf <- ebd_zf %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling", 
                                 0, effort_distance_km),
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )


# additional filtering
ebd_zf_filtered <- ebd_zf %>% 
  filter(
    # effort filters
    duration_minutes <= 5 * 60,
    effort_distance_km <= 5,
    # last 10 years of data
    year >= 2010,
    # 10 or fewer observers
    number_observers <= 10)


ebird <- ebd_zf_filtered %>% 
  select(checklist_id, observer_id, sampling_event_identifier,
         scientific_name,
         observation_count, species_observed, 
         state_code, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         time_observations_started, 
         duration_minutes, effort_distance_km,
         number_observers)
write_csv(ebird, "inbu_data/ebd_inbu_zf.csv", na = "")
