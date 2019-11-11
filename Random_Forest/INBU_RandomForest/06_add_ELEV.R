library(sf)
library(raster)
library(MODIS)
library(velox)
library(viridis)
library(tidyverse)


<<<<<<< HEAD
bcr <- read_sf("inbu_data/gis-data.gpkg", "bcr") %>% 
=======
bcr <- read_sf("Random_Forest/inbu_data/gis-data.gpkg", "bcr") %>% 
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f
  filter(bcr_code == 27) %>%
  # project to the native modis projectin
  st_transform(crs = paste("+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
                           "+a=6371007.181 +b=6371007.181 +units=m +no_defs"))



agg_factor <- round(2 * neighborhood_radius / res(landcover))
r <- raster(landcover) %>% 
  aggregate(agg_factor) 
r <- bcr %>% 
  st_transform(crs = projection(r)) %>% 
  rasterize(r, field = 1) %>% 
  # remove any empty cells at edges
  trim()
<<<<<<< HEAD
r <- writeRaster(r, filename = "inbu_data/prediction-surface.tif", overwrite = TRUE)
=======
r <- writeRaster(r, filename = "Random_Forest/inbu_data/prediction-surface.tif", overwrite = TRUE)
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f





# get cell centers and create neighborhoods
r_centers <- rasterToPoints(r, spatial = TRUE) %>% 
  st_as_sf() %>% 
  transmute(id = row_number())
r_cells <- st_buffer(r_centers, dist = neighborhood_radius)

# extract landcover values within neighborhoods, only need most recent year
lc_vlx <- velox(landcover[[paste0("y", max_lc_year)]])
lc_extract_pred <- lc_vlx$extract(r_cells, df = TRUE) %>% 
  set_names(c("id", "landcover"))

# calculate the percent for each landcover class
pland_pred <- lc_extract_pred %>% 
  count(id, landcover) %>% 
  group_by(id) %>% 
  mutate(pland = n / sum(n)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  # remove NAs after tallying so pland is relative to total number of cells
  filter(!is.na(landcover))

# tranform to wide format, filling in implicit missing values with 0s
pland_pred <- pland_pred %>% 
  mutate(landcover = paste0("pland_", str_pad(landcover, 2, pad = "0"))) %>% 
  pivot_wider(names_from = landcover, 
              values_from = pland, 
              values_fill = list(pland = 0)) %>% 
  mutate(year = max_lc_year) %>% 
  select(id, year, everything())

# join in coordinates
pland_coords <- st_transform(r_centers, crs = 4326) %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  cbind(id = r_centers$id, .) %>% 
  rename(longitude = X, latitude = Y) %>% 
  inner_join(pland_pred, by = "id")


<<<<<<< HEAD
elev <- raster("inbu_data/elevation_1KMmd_GMTEDmd.tif")
=======
elev <- raster("Random_Forest/inbu_data/elevation_1KMmd_GMTEDmd.tif")
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f
# crop, buffer bcr by 10 km to provide a little wiggly room
elev <- bcr %>% 
  st_buffer(dist = 10000) %>% 
  st_transform(crs = projection(elev)) %>% 
  crop(elev, .) %>% 
  projectRaster(crs = projection(landcover))


# buffer each checklist location
ebird_buff_noyear <- ebird %>% 
  distinct(locality_id, latitude, longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = projection(elev)) %>% 
  st_buffer(dist = neighborhood_radius)

# extract using velox and calculate median and sd
locs <- st_set_geometry(ebird_buff_noyear, NULL) %>% 
  mutate(id = row_number())
elev_checklists <- velox(elev)$extract(ebird_buff_noyear, df = TRUE) %>% 
  # velox doesn't properly name columns, fix that
  set_names(c("id", "elevation")) %>% 
  # join to lookup table to get locality_id
  inner_join(locs, ., by = "id") %>% 
  # summarize
  group_by(locality_id) %>% 
  summarize(elevation_median = median(elevation, na.rm = TRUE),
            elevation_sd = sd(elevation, na.rm = TRUE))


# extract using velox and calculate median and sd
elev_pred <- velox(elev)$extract(r_cells, df = TRUE) %>% 
  # velox doesn't properly name columns, fix that
  set_names(c("id", "elevation")) %>% 
  # summarize
  group_by(id) %>% 
  summarize(elevation_median = median(elevation, na.rm = TRUE),
            elevation_sd = sd(elevation, na.rm = TRUE))

pland_elev_checklist <- inner_join(pland, elev_checklists, by = "locality_id")
<<<<<<< HEAD
write_csv(pland_elev_checklist, "inbu_data/pland-elev_location-year.csv")

# prediction surface covariates
pland_elev_pred <- inner_join(pland_coords, elev_pred, by = "id")
write_csv(pland_elev_pred, "inbu_data/pland-elev_prediction-surface.csv")
=======
write_csv(pland_elev_checklist, "Random_Forest/inbu_data/pland-elev_location-year.csv")

# prediction surface covariates
pland_elev_pred <- inner_join(pland_coords, elev_pred, by = "id")
write_csv(pland_elev_pred, "Random_Forest/inbu_data/pland-elev_prediction-surface.csv")
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f
glimpse(pland_elev_pred)
