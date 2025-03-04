library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(rlang)
library(ggpubr)

setwd("./analysis")
players <- read.csv("../data/processed/250015/players.csv")

attributes <- c(
  "height", "weight", "net_worth", "releaseclause", "overallrating",
  "potential", "wage", "crossing", "finishing", "headingaccuracy",
  "shortpassing", "volleys", "dribbling", "curve", "fk_accuracy", "longpassing",
  "ballcontrol", "acceleration", "sprintspeed", "agility", "reactions",
  "balance", "shotpower", "jumping", "stamina", "strength", "longshots",
  "aggression", "interceptions", "vision", "penalties", "composure",
  "defensiveawareness", "standingtackle", "slidingtackle", "gk_diving",
  "gk_handling", "gk_kicking", "gk_positioning", "gk_reflexes",
  "contract_duration", "age_hire", "age_end_contract", "current_age"
)

field_dimensions <- data.frame(
  x = c(0.25, 0.25, 0.75, 0.75, 0.25, 0.25, 0.75, 0.75, 0.65, 0.65, 0.35, 0.35, 0.25, 0.25, 0.35, 0.35, 0.65, 0.65),
  y = c(0, 1.45, 1.45, 0, 0, 0.735, 0.735, 0, 0, 0.29, 0.29, 0, 0, 1.45, 1.45, 1.16, 1.16, 1.45)
)

rect1 <- data.frame (xmin=0.25, xmax=0.75, ymin=0, ymax=1.45)

# Adding foot plots using draw_plot
positions_coordinates <- data.frame(
  GK = c(5, 1),
  RB = c(7, 4),
  CB = c(5, 4),
  LB = c(3, 4),
  CDM = c(5, 7),
  RM = c(7, 10),
  CM = c(5, 10),
  LM = c(3, 10),
  CAM = c(5, 13),
  RW = c(7, 16),
  LW = c(3, 16),
  ST = c(5, 19)
)
rownames(positions_coordinates) <- c("x", "y")

positions_coordinates <- data.frame(t(positions_coordinates))
positions_coordinates$x <- positions_coordinates$x / 10
positions_coordinates$y <- positions_coordinates$y / 15

positions_coordinates$position_lineup <- rownames(positions_coordinates)

players <- left_join(players, positions_coordinates, by="position_lineup")

players_positions <- players |> select(c(c("position_lineup", "x", "y"), attributes))

players_positions_gr <- players_positions |>
  group_by(position_lineup) |> 
  summarize(across(where(is.numeric), median, na.rm = TRUE))

plot_positions <- function(players_positions_gr, attribute, field_dimensions, field_rect, title=""){
  attribute_plot <- ggplot() +
    geom_path(data=field_dimensions, aes(x = x, y = y), color = "#aaa", linewidth = 1) +
    geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#04750d", alpha=0.1, inherit.aes = FALSE) +
    geom_point(data=players_positions_gr, aes(x=x, y=y, size=!!sym(attribute), fill=!!sym(attribute)), shape = 21) +
    scale_fill_gradient(low = "grey", high = "red") +
    xlim(0.22, 0.77) +
    ylim(0, 1.5) +
    geom_label(data=players_positions_gr, aes(x=x, y=y, label = paste0("(",position_lineup,")")), vjust = 1.5, hjust=0.5, size = 2, color = "#000", label.size = 0, alpha=0.1) + 
    geom_label(data=players_positions_gr, aes(x=x, y=y, label = round(!!sym(attribute), 1)), vjust = -0.5, hjust = 0.5, size = 3, color = "#000", label.size = 0, alpha=0.1) +
    theme_void() +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      line=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.position = "none") +
    ggtitle(title)
  return(attribute_plot)
}

attribute <- attributes[[2]]
attribute_plot <- plot_positions(players_positions_gr, attribute, field_dimensions, field_rect, attribute)
print(attribute_plot)

## Multiple plots
height_plot <- plot_positions(players_positions_gr, "height", field_dimensions, field_rect, "height")
weight_plot <- plot_positions(players_positions_gr, "weight", field_dimensions, field_rect, "weight")
strength_plot <- plot_positions(players_positions_gr, "strength", field_dimensions, field_rect, "strength")

ggarrange(height_plot, weight_plot, strength_plot, ncol = 3, nrow = 1)
