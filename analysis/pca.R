library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(FactoMineR)
library(factoextra)

setwd("./analysis")
players <- read.csv("../data/processed/250015/players.csv")

attributes <- c(
  "overallrating", "height", "weight",
  "crossing", "finishing", "headingaccuracy",
  "shortpassing", "volleys", "dribbling", "curve", "fk_accuracy", "longpassing",
  "ballcontrol", "acceleration", "sprintspeed", "agility", "reactions",
  "balance", "shotpower", "jumping", "stamina", "strength", "longshots",
  "aggression", "interceptions", "vision", "penalties", "composure",
  "defensiveawareness", "standingtackle", "slidingtackle", "gk_diving",
  "gk_handling", "gk_kicking", "gk_positioning", "gk_reflexes"
)

attributes_data <- players |> 
  mutate(reference_position = case_when(
    position_lineup %in% c("CB", "LB", "RB", "CDM") ~ "defense",
    position_lineup %in% c("LW", "RW", "CAM", "ST") ~ "attack",
    position_lineup %in% c("CM", "LM", "RM") ~ "middle",
    position_lineup %in% c("GK") ~ "goalkeeper"
  )) |> 
  select(c(c(attributes), c(reference_position, position_lineup)))


# PCA
pca <- PCA(attributes_data[,c(-37, -38)], graph = FALSE)

components <- data.frame(pca$ind)
components <- components[1:nrow(components), 1:3]
components <- cbind(components, attributes_data$reference_position, attributes_data$position_lineup)
names(components)[4] <- "reference_position"
names(components)[5] <- "position_lineup"

fig <- plot_ly(
  components,
  x = ~coord.Dim.1, y = ~coord.Dim.2, z = ~coord.Dim.3,
  color = ~reference_position,
  colors = c("#00AFBB", "#E7B800", "#0C4", "#F7B"),
  text = ~position_lineup,
  hoverinfo = "text"
) |>
  add_markers(size = 12)


fig <- fig |> 
  layout(
    title = "3 main components",
    scene = list(
      bgcolor = "#e5ecf6",
      xaxis = list(showspikes=FALSE),
      yaxis = list(showspikes=FALSE),
      zaxis = list(showspikes=FALSE)
    )
  )

fig
