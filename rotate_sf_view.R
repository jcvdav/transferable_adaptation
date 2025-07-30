
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

# Define a funmction to split and rotate the world for a Pacific-centerde view
lr_stitch <- function(data, meridian) {
  
  pol1 <- tribble(~x, ~y,
                  -180, -90,
                  -180, 90,
                  meridian, 90,
                  meridian, -90,
                  -180, -90) %>%
    as.matrix() %>%
    list() %>%
    st_polygon() %>%
    st_sfc(crs = "EPSG:4326") %>% 
    densify(n = 100L)
  
  pol2 <- tribble(~x, ~y,
                  meridian+0.01, -90,
                  meridian+0.01, 90,
                  180, 90,
                  180, -90,
                  meridian+0.01, -90) %>%
    as.matrix() %>%
    list() %>%
    st_polygon() %>%
    st_sfc(crs = "EPSG:4326") %>% 
    densify(n = 100L)
  
  left <- data %>% 
    # st_crop(ymin = -90, ymax = 90,
    # xmin=-180, xmax=meridian-0.1) %>% 
    st_intersection(pol1)
  
  right <- data %>% 
    # st_crop(ymin = -90, ymax = 90,
    #         xmin=meridian+0.1, xmax=180)
    st_intersection(pol2)
  
  bind_rows(left, right)
}


# Load data --------------------------------------------------------------------
world <- ne_countries(returnclass = "sf")
ref <- world %>% 
  lr_stitch(meridian = 90) %>% 
  st_transform(crs = "EPSG:8858") %>%
  ggplot() +
  geom_sf(fill = "black", color = "black") +
  theme_void()