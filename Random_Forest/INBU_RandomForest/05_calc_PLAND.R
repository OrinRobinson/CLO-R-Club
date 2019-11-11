library(sf)
library(raster)
library(MODIS)
library(velox)
library(viridis)
library(tidyverse)
library(curl)
# resolve namespace conflicts
select <- dplyr::select
projection <- raster::projection


<<<<<<< HEAD
ebird <- read_csv("inbu_data/ebd_inbu_zf.csv") %>% 
=======
ebird <- read_csv("Random_Forest/inbu_data/ebd_inbu_zf.csv") %>% 
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f
  mutate(observation_date = as.Date(observation_date))


# load the landcover data
<<<<<<< HEAD
landcover <- list.files("inbu_data/modis", "^modis_mcd12q1_umd", 
=======
landcover <- list.files("Random_Forest/inbu_data/modis", "^modis_mcd12q1_umd", 
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f
                        full.names = TRUE) %>% 
  stack()
# label layers with year
landcover <- names(landcover) %>% 
  str_extract("(?<=modis_mcd12q1_umd_)[0-9]{4}") %>% 
  paste0("y", .) %>% 
  setNames(landcover, .)


max_lc_year <- names(landcover) %>% 
  str_extract("[0-9]{4}") %>% 
  as.integer() %>% 
  max()


neighborhood_radius <- 5 * ceiling(max(res(landcover))) / 2
ebird_buff <- ebird %>% 
  distinct(year = format(observation_date, "%Y"),
           locality_id, latitude, longitude) %>% 
  # for 2018 use 2017 landcover data
  mutate(year_lc = if_else(as.integer(year) > max_lc_year, 
                           as.character(max_lc_year), year),
         year_lc = paste0("y", year_lc)) %>% 
  # convert to spatial features
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  # transform to modis projection
  st_transform(crs = projection(landcover)) %>% 
  # buffer to create neighborhood around each point
  st_buffer(dist = neighborhood_radius) %>% 
  nest(data = c(year, locality_id, geometry))

# function to extract landcover data for all checklists in a given year
calculate_pland <- function(yr, regions, lc) {
  # create a lookup table to get locality_id from row number
  locs <- st_set_geometry(regions, NULL) %>% 
    mutate(id = row_number())
  
  # extract using velox
  lc_vlx <- velox(lc[[yr]])
  lc_vlx$extract(regions, df = TRUE) %>% 
    # velox doesn't properly name columns, fix that
    set_names(c("id", "landcover")) %>% 
    # join to lookup table to get locality_id
    inner_join(locs, ., by = "id") %>% 
    select(-id)
}
# iterate over all years extracting landcover for all checklists in each
lc_extract <- ebird_buff %>% 
  mutate(pland = map2(year_lc, data, calculate_pland, lc = landcover)) %>% 
  select(pland) %>% 
  unnest(cols = pland)

pland <- lc_extract %>% 
  # count landcovers
  count(locality_id, year, landcover) %>% 
  # calculate proporiton
  group_by(locality_id, year) %>% 
  mutate(pland = n / sum(n)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  # remove NAs after tallying so pland is relative to total number of cells
  filter(!is.na(landcover))

# tranform to wide format, filling in implicit missing values with 0s
pland <- pland %>% 
  mutate(landcover = paste0("pland_", str_pad(landcover, 2, pad = "0"))) %>% 
  pivot_wider(names_from = landcover, 
              values_from = pland, 
              values_fill = list(pland = 0))

# save
<<<<<<< HEAD
write_csv(pland, "inbu_data/modis_pland_location-year.csv")
=======
write_csv(pland, "Random_Forest/inbu_data/modis_pland_location-year.csv")
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f
