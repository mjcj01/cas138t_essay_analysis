library(tidyverse)
library(showtext)

font_add_google("Roboto Condensed")
showtext_auto()

median_cost_distance_graph <- expenditures_and_scores %>%
  mutate(school_districts = fct_reorder(school_districts, desc(distance_from_median_cost))) %>%
  ggplot(aes(x = school_districts, y = distance_from_median_cost)) +
  geom_col(fill = "#000000") +
  labs(title = "School Districts' Distance from Adequacy Cost",
       x = "",
       y = "Distance from Adequacy Cost (Measured from $10,321.85)",
       caption = "Data from the 2018-19 school year, compiled from the Pennsylvania Department of Education") +
  scale_y_continuous(limits = c(-5000,7500)) +
  theme(
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Roboto Condensed", color = "#000000", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#000000"),
    plot.title = element_text(family = "Roboto Condensed", color = "#000000", size = 25,
                              margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(family = "Roboto Condensed", color = "#000000", size = 12.5,
                                margin = margin(t = 0, r = 5, b = 0, l = 5)),
    plot.caption = element_text(family = "Roboto Condensed", color = "#000000", size = 12)
  )

median_cost_distance_graph

