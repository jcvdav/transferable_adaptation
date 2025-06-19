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
  ggalluvial,
  tidyverse
)

# Load data --------------------------------------------------------------------
data <- read_csv(here("data","cases_master_sheet.csv")) %>% 
  janitor::clean_names() %>% 
  select(case_code) %>% 
  mutate(case_code = str_replace_all(case_code, "-", "_"),
         ar = as.numeric(str_extract(case_code, "[:digit:]+")),
         domain = str_extract(str_extract(case_code, "_[abcd]_"), "[:alpha:]"),
         country = str_remove_all(case_code, pattern = paste("_|[:digit:]+", ar, domain, sep = "|")),
         country = case_when(country == "CHL" ~ "Chile",
                             country == "URY" ~ "Uruguay",
                             country == "ECU" ~ "Ecuador",
                             country == "PER" ~ "Peru",
                             country == "MEX" ~ "Mexico",
                             country == "ARG" ~ "Argentina")) %>% 
  mutate(domain = case_when(domain == "a" ~ "Environmental",
                            domain == "b" ~ "Market",
                            domain == "c" ~ "Institutional",
                            domain == "d" ~ "Social",
                            T ~ "missing")) %>% 
  mutate(ar_a = paste0("AR", ar)) %>% 
  group_by(ar_a, ar, domain, country) %>% 
  count() %>% 
  ungroup() %>% 
  
  ###
  group_by(domain) %>% 
  mutate(n_domain = sum(n)) %>% 
  group_by(ar_a) %>% 
  mutate(n_ar = sum(n)) %>% 
  group_by(country) %>% 
  mutate(n_country = sum(n)) %>% 
  ungroup() %>% 
  mutate(domain = paste0(domain, " (n = ", n_domain, ")"),
         ar_a = paste0(ar_a, " (n = ", n_ar, ")"),
         ar_a = fct_reorder(ar_a, ar, .fun = "max"),
         country = paste0(country, " (n = ", n_country, ")")) %>% 
  ###
   
  mutate(domain = fct_reorder(domain, n, .fun = "sum"),
         country = fct_reorder(country, n, .fun = "sum")) %>% 
  mutate(pct = n / sum(n))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
ggplot(data = data,
       mapping = aes(axis1 = domain,
                     axis2 = ar_a,
                     axis3 = country,
           y = pct)) +
  geom_alluvium(aes(fill = str_remove_all(domain, " (.+)")),
                color = "black",
                linewidth = 0.5,
                alpha = 0.7) +
  geom_stratum(width = 1/2) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3) +
  labs(x = "",
       y = "Percent of cases",
       fill = "Domain") +
  scale_x_discrete(limits = c("Domain", "Adaptive Response", "Country"), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2", aesthetics = c("colour", "fill")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        legend.title.position = "top",
        legend.box.spacing = unit(0, "pt"))


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------