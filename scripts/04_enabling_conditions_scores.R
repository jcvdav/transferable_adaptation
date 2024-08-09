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
  janitor,
  tidyverse
)

ar_codes <- tribble(~"ar", ~"ar_text",
  1, "Spatial relocation of resources",
  2, "Modify fishing activity",
  3, "Increase inclusion and representation",
  4, "No-take zones",
  5, "Collective action",
  6, "Shortening supply chain",
  7, "Engage in Aquaculture / Mariculture",
  8, "Livelihood diversification",
  9, "Saving and financing mechanism",
  10, "Changes to marketing strategies"
)

domain_codes <- tribble(~"domain", ~"domain_text",
                        "a", "Environmental",
                        "b", "Market",
                        "c", "Institutional",
                        "d", "Social")

country_codes <- tribble(~"iso3", ~"country",
                         "ARG", "Argentina",
                         "CHL", "Chile", 
                         "ECU", "Ecuador",
                         "MEX", "Mexico",
                         "PER", "Peru",
                         "URY", "Uruguay")

# Load data --------------------------------------------------------------------
enabling_raw <- read_csv(here("data", "enabling_conditions.csv")) %>% 
  select(1:16) %>% 
  select(-4) %>% 
  clean_names() %>% 
  drop_na(iso3) %>% 
  mutate(ar = str_extract(string = case_code,
                          pattern = "([:digit:]+)(?=_)"),
         ar = as.numeric(ar),
         domain = str_extract(strin = case_code,
                              pattern = "(?<=_)([:alpha:])(?=_)"),
         iso3 = str_extract(string = case_code,
                            pattern = "(?<=_)([:alpha:]{3})")) %>% 
  select(case_code, ar, domain, iso3, everything()) %>% 
  mutate_at(.vars = c(5:16), ~ifelse(is.na(.x), 0, 1)) %>% 
  left_join(ar_codes, by = "ar") %>% 
  left_join(domain_codes, by = "domain") %>% 
  left_join(country_codes, by = "iso3")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
enabling_scores <- enabling_raw %>%
  # Add a new variable that counts the number of 
  mutate(enabling_score = clearly_defined_vision + 
           collaboration_across_jurisdictions + 
           conflict_resolution_mechanisms + 
           coordination_with_governance_inst + 
           evidence_based_decision_making + 
           knowledge_integration + 
           long_term_political_will + 
           public_participation_with_fairness + 
           recog_of_local_or_indigenous_governance + 
           sustainable_financing + 
           transparency_and_communication + 
           up_and_down_accountability)

calculate_group_score <- function(data, group) {
  
  unique_enabling_conditions_by_group <- data %>%
    group_by_at({{group}}) %>%
    summarize_all(.funs = ~any(.x == 1)) %>% 
    mutate(n_conditions = clearly_defined_vision + 
             collaboration_across_jurisdictions + 
             conflict_resolution_mechanisms + 
             coordination_with_governance_inst + 
             evidence_based_decision_making + 
             knowledge_integration + 
             long_term_political_will + 
             public_participation_with_fairness + 
             recog_of_local_or_indigenous_governance + 
             sustainable_financing + 
             transparency_and_communication + 
             up_and_down_accountability) %>% 
    select_at(.vars = c(group, "n_conditions"))
  
  
  enabling_score_by_group <- data %>% 
    group_by_at(group) %>% 
    summarize(n_cases = n(),
              enabling_score_sum = sum(enabling_score),
              min = min(enabling_score),
              max = max(enabling_score)) %>% 
    mutate(mean_enabling_score = enabling_score_sum / n_cases)
  
  out <- left_join(enabling_score_by_group,
                   unique_enabling_conditions_by_group,
                   by = group)
}

ar_scores <- calculate_group_score(enabling_scores, "ar_text") %>% 
  left_join(ar_codes, by = join_by("ar_text")) %>% 
  mutate(ar_text = paste0("AR", ar, " - ", ar_text),
         ar_text = fct_reorder(ar_text, mean_enabling_score, .desc = T))

domain_scores <- calculate_group_score(enabling_scores, "domain_text") %>% 
  left_join(domain_codes, by = join_by("domain_text")) %>% 
  mutate(domain_text = paste0(domain, ") ", domain_text),
         domain_text = fct_reorder(domain_text, mean_enabling_score, .desc = T))

country_scores <- calculate_group_score(enabling_scores, "country") %>% 
  mutate(country = fct_reorder(country, mean_enabling_score, .desc = T))

## VISUALIZE ###################################################################

#Absence of X is not a limiting condition. Instead, things like "rules that say you can't do X" are limitng conditons

enabling_scores %>% 
  mutate(ar_text = paste0("AR", ar, " - ", ar_text),
         ar_text = fct_reorder(ar_text, ar, mean, .desc = T)) %>% 
  select(-c(case_code, ar, domain, iso3, domain_text, country, enabling_score)) %>% 
  group_by(ar_text) %>% 
  summarize_all(sum) %>% 
  pivot_longer(cols = 2:13,
               names_to = "condition",
               values_to = "count") %>% 
  mutate(condition = str_to_sentence(str_replace_all(condition, "_", " ")),
         count_text = ifelse(count == 0, "", count)) %>% 
  ggplot(aes(x = condition, y = ar_text, fill = count, label = count_text)) +
  geom_tile() +
  geom_text() +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "white",
                      high = "darkgreen") +
  guides(fill = guide_colorbar(title = "Count",
                               frame.colour = "black",
                               frame.linewidth = 0.5,
                               ticks.colour = "black",
                               ticks.linewidth = 0.5)) +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 0))
  

# X ----------------------------------------------------------------------------
plot_scores <- function(data, group) {
  ggplot(data = data,
         mapping = aes(x = {{group}}, y = mean_enabling_score / 12, size = n_cases)) +
    geom_pointrange(aes(ymin = min / 12, ymax = max / 12), fatten = 1, size = 0) +
    geom_hline(yintercept = mean(data$mean_enabling_score / 12, na.rm = T),
               linetype = "dashed") +
    geom_point(fill = "black",
               color = "black",
               shape = 21) +
    geom_point(aes(y = n_conditions / 12),
               fill = "red3",
               color = "black",
               shape = 22,
               size =1.5) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "",
         y = "",
         size = "N") +
    theme(legend.position = "inside",
          legend.position.inside = c(1, 1),
          legend.justification = c(1,1),
          legend.background = element_rect(color = "black",
                                           linewidth = 0.1)) +
    coord_flip()
}

ar_plot <- plot_scores(ar_scores, ar_text) +
  labs(title = "A) By adaptive response")
domain_plot <- plot_scores(domain_scores, domain_text) +
  labs(title = "B) By domain")
country_plot <- plot_scores(country_scores, country) +
  labs(y = "Enabling score (Mean ± Range)",
       title = "C) By country")

enabling_scores_plot <- plot_grid(ar_plot,
                                  domain_plot,
                                  country_plot,
                                  ncol = 1,
                                  align = "hv")

enabling_scores_plot


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------