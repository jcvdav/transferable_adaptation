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
library(here)
library(readxl)
library(janitor)
library(cowplot)
library(tidyverse)

# Load data --------------------------------------------------------------------
adapt_raw <- read_xlsx(here("data", "raw", "Elias_Ilosvay", "1_database.xlsx"))

# Define aprsing function ------------------------------------------------------
addline_format <- function(x,...){
  str_to_sentence(x) %>% 
    str_replace_all("\\s", "\n")
}

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
adapt <- adapt_raw %>% 
  clean_names() %>% 
  filter(type_response == "adaptive",
         adaptation_directly_related_to_respective_impact == "yes"
         ) %>%
  select(id, author, year, location, fishing_resources,
         text_harzard_impact, type_of_stressor, type_of_fisheries_stressor,
         adaptive_response, type_adaptation_specific) %>%
  mutate(type_of_stressor = str_remove_all(type_of_stressor, "\\?")) %>% 
  drop_na(type_of_stressor)

# Same response for same stressor across communities
transfer_data <- adapt %>% 
  distinct() %>% 
  group_by(type_of_stressor, type_adaptation_specific) %>% 
  summarize(n = n(),
            n_ses = n_distinct(location)) %>% 
  ungroup() %>% 
  mutate(type_of_stressor = str_squish(type_of_stressor),
         type_of_stressor = fct_reorder(type_of_stressor, -n_ses, .fun = sum),
         type_adaptation_specific = fct_reorder(type_adaptation_specific, n_ses, .fun = max))

# Same response for same community across stressors
transpose_data <- adapt %>% 
  group_by(location, type_adaptation_specific) %>% 
  summarize(n = n(),
            n_stress = n_distinct(type_of_stressor)) %>% 
  ungroup() %>% 
  mutate(location = fct_reorder(location, -n),
         type_adaptation_specific = fct_reorder(type_adaptation_specific, n))

# Same response across community and stressors
general_data <- adapt %>% 
  group_by(type_adaptation_specific, location) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  group_by(type_adaptation_specific) %>% 
  mutate(nn = sum(n)) %>% 
  ungroup() %>% 
  mutate(type_adaptation_specific = fct_reorder(type_adaptation_specific, -nn))
  


## VISUALIZE ###################################################################

# Transfer ----------------------------------------------------------------------
# All
# ggplot(data = transfer_data,
#        aes(x = type_of_stressor,
#            y = type_adaptation_specific,
#            fill = n_ses)) +
#   geom_tile(color = "black") +
#   geom_text(aes(label = n_ses), color = "white") +
#   geom_rect(xmin = 0.5, xmax = 4.5, ymin = 8.5, ymax = 21.5, fill = "transparent", color = "black") +
#   scale_x_discrete(position = "top", labels = addline_format) +
#   scale_fill_viridis_c() +
#   labs(x = "Stressor",
#        y = "Adaptive Response",
#        fill = "# SES") +
#   guides(fill = guide_colorbar(frame.colour = "black",
#                                ticks.colour = "black")) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust=0),
#         legend.position = "None")

# More than 1
transfer <- ggplot(data = transfer_data %>% 
         filter(n_ses > 1),
       aes(x = type_of_stressor,
           y = type_adaptation_specific,
           fill = n_ses)) +
  geom_tile(color = "black") +
  geom_text(aes(label = n), color = "white") +
  scale_x_discrete(position = "top", labels = addline_format) +
  scale_fill_viridis_c() +
  labs(x = "Stressor",
       y = "Adaptive Response",
       fill = "# SES") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust=0),
        legend.justification = c(1, 0),
        legend.position = c(1, 0))

# Translocation ----------------------------------------------------------------
translocation <- ggplot(data = transpose_data %>% 
         filter(n > 1),
       mapping = aes(x = location, y = type_adaptation_specific, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white") +
  scale_x_discrete(position = "top", labels = addline_format) +
  scale_fill_viridis_c() +
  labs(x = "Community",
       y = "Adaptive Response",
       fill = "# SES") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust=0),
        legend.justification = c(1, 1),
        legend.position = c(1, 1))

# Generalixability -------------------------------------------------------------
general <- ggplot(data = general_data %>% 
         filter(nn > 1),
       mapping = aes(x = type_adaptation_specific, y = n)) +
  geom_col(color = "black") +
  labs(x = "",
       y = "# times") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black")) +
  theme_minimal() +
  coord_flip() 


# Combine ----------------------------------------------------------------------

plot_grid(plot_grid(transfer, translocation),
          general,
          ncol = 1)

ggdraw() +
  draw_plot(general) +
  draw_plot(transfer, height = 0.6, width = 0.5, x = 0.5, y = 0.5)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------