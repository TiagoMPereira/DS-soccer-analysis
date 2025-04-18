library(ggplot2)
library(png)
library(dplyr)
library(ggplot2)
library(ggimage)
library(magick)
library(cowplot)
library(tidyr)
library(reshape2)
library(ggpubr)

setwd("./analysis")
players <- read.csv("../data/processed/250015/players.csv")
players$position_lineup <- factor(as.character(players$position_lineup),
                                     levels=c('GK', 'RB', 'CB', 'LB', 'CDM', 'RM', 'CM', 'LM', 'CAM', 'RW', 'LW', 'ST'))


left_shoe <- magick::image_transparent(
  magick::image_read("../data/images/left_shoe.png"),
  color = "white"
)
right_shoe <- magick::image_transparent(
  magick::image_read("../data/images/right_shoe.png"),
  color = "white"
)

plot_foot <- function(dataset, left_foot_img, right_foot_img, title="", calc_porc=TRUE, note=""){
  if (calc_porc==TRUE){
    dataset$pct <- dataset$value / sum(dataset$value)
  }
  
  left_pct = dataset$pct[dataset$foot == "left"]
  left_pct_str = paste0(as.character(round(left_pct*100, 1)), "%")
  right_pct = dataset$pct[dataset$foot == "right"]
  right_pct_str = paste0(as.character(round(right_pct*100, 1)), "%")
  
  left_foot_img_alpha <- magick::image_fx(
    left_foot_img,
    expression = paste0(as.character(left_pct), "*a"),
    channel = "alpha"
  )
  right_foot_img_alpha <- magick::image_fx(
    right_foot_img,
    expression = paste0(as.character(right_pct), "*a"),
    channel = "alpha"
  )
  
  fig <- ggplot(data.frame(x = 0:2, y = 0:2), aes(x, y)) +
    draw_image(left_foot_img_alpha , x = -0.15, y = 0, scale = .7) +
    draw_image(right_foot_img_alpha, x = 0.15, y = 0, scale = .7) +
    geom_text(x=0.32, y=0.8, label=left_pct_str, size=3) +
    geom_text(x=0.68, y=0.8, label=right_pct_str, size=3) +
    geom_text(x=0.75, y=0.2, label=note, size=3) +
    theme_void() +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank()
    )

  return(fig)
}

plots <- list()

positions <- unique(players$position_lineup)

for (i in 1:length(positions)){
  position_current <- positions[[i]]
  
  players_current <- players |> 
    filter(position_lineup==position_current)
  
  players_foot <- players_current |> 
    group_by(preferredfoot) |> 
    summarise(value=n()) |> 
    mutate(foot=tolower(preferredfoot), value=value) |> 
    select(c(foot, value))
  
  fig <- plot_foot(players_foot, left_shoe, right_shoe, note=position_current)
  plots[[i]] <- fig
}
names(plots) <- positions


field_dimensions <- data.frame(
  x = c(0.25, 0.25, 0.75, 0.75, 0.25, 0.25, 0.75, 0.75, 0.65, 0.65, 0.35, 0.35, 0.25, 0.25, 0.35, 0.35, 0.65, 0.65),
  y = c(0, 1.45, 1.45, 0, 0, 0.735, 0.735, 0, 0, 0.29, 0.29, 0, 0, 1.45, 1.45, 1.16, 1.16, 1.45)
)

rect1 <- data.frame (xmin=0.25, xmax=0.75, ymin=0, ymax=1.45)

# Creating the base soccer field
base_plot <- ggplot() + 
  geom_path(data = field_dimensions, aes(x = x, y = y), color = "#aaa", linewidth = 1) +
  xlim(0, 1.5) +
  ylim(0, 1.5) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#04750d", alpha=0.1, inherit.aes = FALSE) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        line=element_blank(),
        text=element_blank())

# Adding foot plots using draw_plot
positions_coordinates <- data.frame(
  GK = c(5, 0),
  RB = c(9, 4),
  CB = c(5, 4),
  LB = c(1, 4),
  CDM = c(5, 7),
  RM = c(9, 10),
  CM = c(5, 10),
  LM = c(1, 10),
  CAM = c(5, 13),
  RW = c(9, 16),
  LW = c(1, 16),
  ST = c(5, 19)
)
rownames(positions_coordinates) <- c("x", "y")

final_plot <- base_plot
for (i in names(plots)) {
  
  pos_x <- (positions_coordinates[[i]][[1]]) / 25 + 0.2
  pos_y <- positions_coordinates[[i]][[2]] / 15
  
  final_plot <- final_plot +
    draw_plot(plots[[i]], x = pos_x, y = pos_y, 
              width = 0.2, height = 0.2) 
}

final_plot <- final_plot +
  theme_void() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank()
  )


print(final_plot)

## Analysis of preferred foot based on players on the side of the field
side_players <- players |> 
  mutate(
    tackle = rowMeans(select(players, c(standingtackle, slidingtackle)))
  ) |> 
  filter(position_lineup %in% c("RB", "LB", "RM", "LM", "RW", "LW")) |> 
  mutate(
    foot_group = case_when(
      position_lineup %in% c("RB", "RM", "RW") & preferredfoot == "Right" ~ "Natural foot",
      position_lineup %in% c("LB", "LM", "LW") & preferredfoot == "Left" ~ "Natural foot",
      TRUE ~ "Switched foot"
    )
  ) |> 
  select(c("position_lineup", "crossing", "finishing", "tackle", "foot_group"))

descriptive_side_players <- side_players |> 
  group_by(position_lineup, foot_group) |> 
  summarise(
    absolute = n()
  ) |> 
  arrange(position_lineup, foot_group) |> 
  pivot_wider(
    names_from=foot_group,
    values_from = absolute
  ) |>
  mutate(
    total_position = sum(`Natural foot`, `Switched foot`),
    `Natural foot (%)` = `Natural foot` / total_position * 100,
    `Switched foot (%)` = `Switched foot` / total_position * 100
  ) |> 
  select(-c("total_position"))

names(descriptive_side_players)[names(descriptive_side_players) == 'position_lineup'] <- 'Position'

descriptive_side_players

##  attributes
side_players_attr <- side_players |>
  group_by(position_lineup, foot_group) |> 
  summarise(across(where(is.numeric), median, na.rm = TRUE)) |> 
  melt(id=c("position_lineup", "foot_group"))

plot_foot_stats <- function(players_data, variable_name, title){
  
  data_aux <- players_data |> filter(variable==variable_name)
  
  fig <- ggplot(data_aux, aes(foot_group, position_lineup, fill= value)) + 
    geom_tile(color = "white", lwd=1, linetype=1) +
    scale_fill_gradient2(
      low = "white",
      mid = "white",
      high = "red"
    ) +
    theme_minimal() +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5)) +
    labs(
      title=title,
      x = NULL,
      y = NULL
    ) +
    geom_text(aes(label = round(value, 3)), color = "black", size = 4)
  
  return(fig)
  
}

tackle_stats <- plot_foot_stats(side_players_attr, "tackle", "Tackle")
crossing_stats <- plot_foot_stats(side_players_attr, "crossing", "Crossing")
finishing_stats <- plot_foot_stats(side_players_attr, "finishing", "Finishing")

foot_stats_side <- ggarrange(tackle_stats, crossing_stats, finishing_stats, ncol = 3, nrow = 1)
annotate_figure(foot_stats_side, top = text_grob("Median score of side players grouped by preferred foot"))
