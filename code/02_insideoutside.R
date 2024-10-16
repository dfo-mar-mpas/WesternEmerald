# Load libraries ----
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(MarConsNetData)
library(robis)

# Projections ----
latlong <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Load mapping data ----
# Load the Scotian Shelf-Bay of Fundy Planning Region
bioregion <- data_planning_areas() %>% 
  st_transform(CanProj)

# Load the network polygons 
network <- data_draft_areas() %>% 
  st_transform(CanProj)

# Aquire the RV survey data ----
rvopen <- data_MarRV_survey_open()

# Extract and clean RV data ----
gs_names <- c("GSCAT", "GSINF", "GSMISSIONS", "GSSPECIES")
for (i in gs_names) {assign(i, rvopen[[which(grepl(i, names(rvopen)))]] )}

# Identify coordinates for each station (calculate midpoints if necessary) ----
GSINF <- GSINF %>%
  rowwise() %>%
  mutate(
    MLAT = if (!is.na(SLAT) & !is.na(SLONG) & !is.na(ELAT) & !is.na(ELONG)) {
      geosphere::midPoint(c(SLONG, SLAT), c(ELONG, ELAT))[2]
    } else if (!is.na(SLAT) & !is.na(SLONG)) {
      SLAT
    } else if (!is.na(ELAT) & !is.na(ELONG)) {
      ELAT
    } else {
      NA_real_
    },
    MLONG = if (!is.na(SLAT) & !is.na(SLONG) & !is.na(ELAT) & !is.na(ELONG)) {
      geosphere::midPoint(c(SLONG, SLAT), c(ELONG, ELAT))[1]
    } else if (!is.na(SLAT) & !is.na(SLONG)) {
      SLONG
    } else if (!is.na(ELAT) & !is.na(ELONG)) {
      ELONG
    } else {
      NA_real_
    },
    ID = case_when(
      !is.na(SLAT) & !is.na(ELAT) ~ "Middle",
      !is.na(SLAT) ~ "Start",
      !is.na(ELAT) ~ "End",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

# Convert stations to sf object ----
rv_stations <- GSINF %>%
  mutate(date = as.POSIXct(SDATE), year = year(date), decade = paste0(floor(year / 10) * 10, "'s")) %>%
  st_as_sf(coords = c("MLONG", "MLAT"), crs = latlong) %>%
  st_transform(CanProj)

# Intersect stations with network polygons ----
rv_stations_with_network <- st_join(rv_stations, network, join = st_intersects)

# Add columns to classify points ----
rv_stations_with_network <- rv_stations_with_network %>%
  mutate(
    in_network = ifelse(!is.na(SiteName_E), "In Draft Network", "Outside Network"),
    in_western_emerald_bank = ifelse(SiteName_E == "Western/Emerald Banks Marine Refuge", 
                                     "Western Emerald Bank", 
                                     in_network)
  )

# Plot the result ----
p1 <- ggplot() +
  geom_sf(data = bioregion, fill = NA) +
  geom_sf(data = rv_stations_with_network, size = 0.12) +
  geom_sf(data = network, alpha = 0.5) +
  geom_sf(data = network %>% filter(SiteName_E == "Western/Emerald Banks Marine Refuge"), fill = "coral2", alpha = 0.5) +
  facet_wrap(. ~ decade, nrow = 2) +
  coord_sf(expand = 0, xlim = st_bbox(bioregion)[c(1, 3)], ylim = st_bbox(bioregion)[c(2, 4)]) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

# Save the plot ----
ggsave("output/rvsets_with_network.png", p1, width = 8, height = 5, units = "in", dpi = 300)

# Save the output data ----
# Save the updated data if needed
# st_write(rv_stations_with_network, "output/rv_stations_with_network.shp")
