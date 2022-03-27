library(tidyverse)

median_cost_distance_graph <- expenditures_and_scores %>%
  mutate(school_districts = fct_reorder(school_districts, desc(distance_from_median_cost))) %>%
  ggplot(aes(x = school_districts, y = distance_from_median_cost)) +
  geom_col() +
  geom_vline(xintercept = 188) +
  scale_y_continuous(limits = c(-5000,7500)) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

median_cost_distance_graph
