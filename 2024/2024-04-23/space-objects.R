# Load Packages
library(tidyverse)
library(cowplot)

# Load Data
outer_space_objects <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv')

# Determine top entities
x <- outer_space_objects |>
  filter(Entity != "World",
         Year >= 2013) |>
  group_by(Entity) |>
  summarise(num_objects = sum(num_objects)) |>
  mutate(Entity = factor(Entity),
         Entity = fct_lump_n(Entity, n = 3, w = num_objects)) |>
  group_by(Entity) |>
  summarise(num_objects = sum(num_objects)) 
  
primary_font <- "Krungthep"

# Create space object figure
fig <- outer_space_objects |>
  filter(Entity != "World",
         Year >= 2013) |>
  mutate(Entity = ifelse(Entity %in% x$Entity, Entity, "Other"),
         Year = factor(Year)) |>
  group_by(Entity, Year) |>
  summarise(num_objects = sum(num_objects) / 10) |>
  mutate(num_objects = purrr::map(num_objects, ~ rep(1, .x)))  |>
  unnest(cols = c(num_objects)) |>
  ungroup() |>
  group_by(Year) |>
  mutate(seq_val = 1:n(),
         spacing = seq_val / max(seq_val)) |> 
  ggplot(aes(spacing, Year, color = Entity)) +
  geom_point(size = 1.75) +
  coord_radial(inner.radius = 0.5, expand = F,) +
  labs(title = "Top Entities Launching Objects into Space",
       subtitle = "1 Dot per 10 Objects Launched in Last 10 Years",
       caption = "Data Source:  United Nations Office for Outer Space Affairs (2024)
       Produced By: @datadoodlr") +
  guides(color = guide_legend(override.aes = list(size=5),title = NULL)) +
   theme(
    plot.background = element_rect(fill = "#2a3038", color = "#2a3038"),
    panel.background = element_rect(fill = "#2a3038", color = "#2a3038"),
    text = element_text(face = "bold", color = "#F0E1B9", family = primary_font, size = 14),
    plot.title = element_text(hjust = 0.5,size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 16,margin = margin(t = 10)),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold", color = "#F0E1B9", family = primary_font),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#716868"),
    legend.background = element_blank(),
    legend.title.position = "top",
    legend.key.width = unit(6, "lines"),
    legend.justification = "center", 
    legend.margin = margin(20,5,5,0),
    legend.text.position = 'bottom',
    legend.position = "top",
    plot.margin = margin(30, 5, 5, 5)
  ) 

# Combine figures
space_object_fig <- cowplot::ggdraw(fig) +
  cowplot::draw_image(image = "Rplot.png",width = 0.25, hjust = -1.5, vjust = 0.085,scale = 1.5) 

ggsave("space-objects.png", space_object_fig,path = "plots",width = 14, height = 10, units = "in")
