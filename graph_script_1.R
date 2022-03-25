library(tidyverse)

median_cost_distance_graph <- expenditures_and_scores %>%
  mutate(school_districts = fct_reorder(school_districts, desc(distance_from_median_cost))) %>%
  ggplot(aes(x = school_districts, y = distance_from_median_cost)) +
  geom_col()

median_cost_distance_graph
