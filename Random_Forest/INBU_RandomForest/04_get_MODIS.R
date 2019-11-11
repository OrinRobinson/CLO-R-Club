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
bcr <- read_sf("inbu_data/gis-data.gpkg", "bcr") %>% 
=======
bcr <- read_sf("Random_Forest/inbu_data/gis-data.gpkg", "bcr") %>% 
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f
  filter(bcr_code == 27) %>%
  # project to the native modis projectin
  st_transform(crs = paste("+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
                           "+a=6371007.181 +b=6371007.181 +units=m +no_defs"))

<<<<<<< HEAD
ebird <- read.csv("inbu_data/ebd_inbu_zf.csv") %>% mutate(observation_date = as.Date(observation_date))
=======
ebird <- read.csv("Random_Forest/inbu_data/ebd_inbu_zf.csv") %>% mutate(observation_date = as.Date(observation_date))
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f


tiles <- getTile(bcr)
tiles@tile


MODIS::EarthdataLogin(usr = "orobinson350", pwd = "gZwiX_a$D7_6u97")

# earliest year of ebird data
begin_year <- format(min(ebird$observation_date), "%Y.01.01")
# end date for ebird data
end_year <- format(max(ebird$observation_date), "%Y.12.31")
# download tiles and combine into a single raster for each year
tifs <- runGdal(product = "MCD12Q1", collection = "006", SDSstring = "01", 
                extent = bcr %>% st_buffer(dist = 10000), 
                begin = begin_year, end = end_year, 
<<<<<<< HEAD
                outDirPath = "inbu_data", job = "modis") %>% 
=======
                outDirPath = "Random_Forest/inbu_data", job = "modis") %>% 
>>>>>>> ac34e9ed6a230a245ed9aa2c01e1299a1a40555f
  pluck("MCD12Q1.006") %>% 
  unlist()

# rename tifs to have more descriptive names
new_names <- format(as.Date(names(tifs)), "%Y") %>% 
  sprintf("modis_mcd12q1_umd_%s.tif", .) %>% 
  file.path(dirname(tifs), .)
file.rename(tifs, new_names)

### You will notice that there is no 2019 MODIS data file.
### Ths is because there is no 2019 MODIS data available yet.

