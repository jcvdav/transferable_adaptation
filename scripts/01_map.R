################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  cowplot,
  rnaturalearth,
  smoothr,
  sf,
  tidyverse
)

sf_use_s2(F)

# Load data --------------------------------------------------------------------
world <- ne_countries(returnclass = "sf")


continent <- world %>% 
  st_crop(xmin = -130, ymin = -60,
          xmax = -30, ymax = 40)

countries <- world %>% 
  filter(iso_a3 %in% c("ARG",
                       "CHL",
                       "COL",
                       "ECU",
                       "MEX",
                       "PER",
                       "URY"))

pts <- read_csv(here("data", "cases_master_sheet.csv")) %>% 
  replace_na(replace = list(Lon = -40,
                            Lat = 20)) %>% 
  st_as_sf(coords = c("Lon", "Lat"),
           crs = 4326)


## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

main <- ggplot() +
  geom_sf(data = continent,
          fill = "gray95",
          color = "black") +
  geom_sf(data = countries,
          fill = "cadetblue",
          color = "black") +
  geom_sf(data = pts,
          fill = "orange",
          color = "black",
          shape = 21,
          size = 2) +
  coord_sf(crs = "EPSG:8858") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

main

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------