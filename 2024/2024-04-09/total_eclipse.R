# Tidy Tuesday (2024-04-09)
# Developed By: @datadoodlr

# Load Packages ----
library(tidyverse)
library(maps)

# Load Data ----
eclipse_annular_2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_annular_2023.csv')
eclipse_total_2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_total_2024.csv')
eclipse_partial_2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_partial_2023.csv')
eclipse_partial_2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_partial_2024.csv')

# Generate Plot ----
states <- map_data("state")
primary_font <- "Krungthep"

eclipse_total_2024 |>
  mutate(t_totality_sec = as.numeric(eclipse_4 - eclipse_3)) |>
  ggplot(aes(lon, lat, color = t_totality_sec)) +
  geom_polygon(data=states, 
               aes(x=long, y=lat, group=group), 
               fill='#A13941', color = "#F0E1B9") +
  geom_point(size = 1) +
  labs(title = "2024 Total Solar Eclipse",
       subtitle = "Duration of Totality",
       caption = "Data Source:  NASA's Scientific Visualization Studio
       Produced By: @datadoodlr") +
  binned_scale(name = "Maximum Duration (seconds)",
               aesthetics = "color",
               scale_name = "stepsn", 
               palette = function(x) c("#D3D0CB", "#9FB1BC", "#6E8898", "#2E5266", "#28334A"),
               breaks = seq(0, 300, 60),
               guide = "colorsteps"
  ) +
  theme(panel.background = element_rect(fill = "#F0E1B9"),
        plot.background = element_rect(fill = "#F0E1B9"),
        text = element_text(face = "bold", color = "#A13941", family = primary_font, size = 14),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(6, "lines"),
        legend.background = element_blank(),
        legend.title.position = "top",
        legend.justification = "center",
        legend.position = "bottom",
        plot.margin = margin(30, 5, 5, 5))


