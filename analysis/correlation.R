library(readr)
library(ggcorrplot)
library(ggplot2)
library(dplyr)
library(cowplot)
library(GGally)
library(rlang)
library(reshape2)

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
  "gk_handling", "gk_kicking", "gk_positioning", "gk_reflexes", "current_age"
)

attributes_data <- players |> 
  mutate(reference_position = case_when(
    position_lineup %in% c("CB", "LB", "RB", "CDM") ~ "defense",
    position_lineup %in% c("LW", "RW", "CAM", "ST") ~ "attack",
    position_lineup %in% c("CM", "LM", "RM") ~ "middle",
    position_lineup %in% c("GK") ~ "goalkeeper"
  )) |> 
  select(c(c(attributes), c(reference_position)))

### CORRELATION

corr <- round(cor(attributes_data |> select(-c(reference_position, current_age)), use = "complete.obs"), 2)

diag(corr) <- NA
corr[upper.tri(corr)] <- NA

corr_melt <- corr |>
  melt() |>
  filter(!is.na(value)) |>
  mutate(label=case_when(
    abs(value) > 0.5 ~ value,
    TRUE ~ NA
  ))

ggplot(corr_melt, aes(Var1, Var2, fill=value)) + 
  geom_tile(color = "white", lwd=1, linetype=1) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red"
  ) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) +
  labs(
    title="Correlation between player's attributes",
    x = NULL,
    y = NULL
  ) +
  geom_text(aes(label = label), color = "black", size = 3) +
  scale_y_discrete(position = "right") 

# Analyzing overallrating based on positions
plot_correlation <- function(data, reference_column, reference_value=NULL, title=NULL){
  
  if(!is.null(reference_column) & !is.null(reference_value)){
    data <- data |> filter(!!sym(reference_column)==reference_value)
  }

  corr <- round(cor(data |> select(-c(reference_column, current_age)), use = "complete.obs"), 2)
  
  diag(corr) <- NA
  corr[upper.tri(corr)] <- NA
  
  corr_melt <- corr |>
    melt() |>
    filter(!is.na(value)) |>
    mutate(label=case_when(
      abs(value) > 0.5 ~ value,
      TRUE ~ NA
    ))
  
  corr_plot <- ggplot(corr_melt, aes(Var1, Var2, fill=value)) + 
    geom_tile(color = "white", lwd=1, linetype=1) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red"
    ) +
    theme_minimal() +
    theme(
      legend.position="none",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) +
    labs(
      title=title,
      x = NULL,
      y = NULL
    ) +
    geom_text(aes(label = label), color = "black", size = 3) +
    scale_y_discrete(position = "right") 
  
  return(list(corr_melt, corr_plot))
  
}

general_info <- plot_correlation(attributes_data, reference_column="reference_position", title="Correlation between players' attributes")
general_corr <- general_info[[1]]
general_plot <- general_info[[2]]

gk_info <- plot_correlation(attributes_data, "reference_position", "goalkeeper", "Correlation between goalkeepers' attributes")
gk_corr <- gk_info[[1]]
gk_plot <- gk_info[[2]]

defense_info <- plot_correlation(attributes_data, "reference_position", "defense", "Correlation between defense players' attributes")
defense_corr <- defense_info[[1]]
defense_plot <- defense_info[[2]]

middle_info <- plot_correlation(attributes_data, "reference_position", "middle", "Correlation between middle players' attributes")
middle_corr <- middle_info[[1]]
middle_plot <- middle_info[[2]]

attack_info <- plot_correlation(attributes_data, "reference_position", "attack", "Correlation between attack players' attributes")
attack_corr <- attack_info[[1]]
attack_plot <- attack_info[[2]]

general_plot
gk_plot
defense_plot
middle_plot
attack_plot

## Presenting only overall
gk_overall <- gk_corr |> filter(Var2=="overallrating") |> mutate(position="goalkeeper")
defense_overall <- defense_corr |> filter(Var2=="overallrating") |> mutate(position="defense")
middle_overall <- middle_corr |> filter(Var2=="overallrating") |> mutate(position="middle")
attack_overall <- attack_corr |> filter(Var2=="overallrating") |> mutate(position="attack")

overall_corr <- rbind(gk_overall, defense_overall, middle_overall, attack_overall) |> 
  select(-Var2) |> 
  mutate(position = factor(position, levels=c("goalkeeper", "defense", "middle", "attack")))

overall_plot <- ggplot(overall_corr, aes(Var1, position, fill=value)) + 
  geom_tile(color = "white", lwd=1, linetype=1) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red"
  ) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) +
  labs(
    title="Correlation between attributes and Overall Rating for players by position",
    x = "Attributes",
    y = "Position"
  ) +
  geom_text(aes(label = label), color = "black", size = 3) +
  scale_y_discrete(position = "right") 
overall_plot

## Splitting by position and age
ages <- c("[min, 21[", "[21, 26[", "[26, 31[", "[31, 36[", "[36, max]")
lower_limit <- c(-Inf, 21, 26, 31, 36)
upper_limit <- c(21, 26, 31, 36, Inf)
positions <- c("goalkeeper", "defense", "middle", "attack")

ages_overall_corr <- data.frame()

for(i in 1:length(ages)){
  
  current_age_range <- ages[i]
  current_lower_limit <- lower_limit[i]
  current_upper_limit <- upper_limit[i]
  
  players_in_range <- attributes_data |> 
    filter(current_age >= current_lower_limit & current_age < current_upper_limit)
  
  for (p in positions){
    
    players_in_position <- players_in_range |> 
      filter(reference_position==p)
    
    corr <- round(cor(players_in_position |> select(-c(reference_position, current_age)), use = "complete.obs"), 2)
    diag(corr) <- NA
    
    corr_df <- data.frame(data.frame(corr)$overallrating)
    colnames(corr_df) <- c("value")
    corr_df$var <- rownames(corr)
    corr_df <- corr_df |> 
      filter(var!="overallrating") |> 
      mutate(
        label=case_when(
          abs(value) > 0.5 ~ value,
          TRUE ~ NA
        ),
        position=p,
        age_range=current_age_range
      )
    
    ages_overall_corr <- rbind(ages_overall_corr, corr_df)
  }
}
ages_overall_corr$age_range <- factor(ages_overall_corr$age_range, levels=ages)
ages_overall_corr$var <- factor(ages_overall_corr$var, levels=attributes)


plot_age_corr <- function(corr_data, position_ref, title=NULL){
  
  positions_corr <- corr_data |> filter(position==position_ref)
  
  position_age_corr_plot <- ggplot(positions_corr, aes(var, age_range, fill=value)) + 
    geom_tile(color = "white", lwd=1, linetype=1) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red"
    ) +
    theme_minimal() +
    theme(
      legend.position="none",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) +
    labs(
      title=title,
      x = "Attributes",
      y = "Age range"
    ) +
    geom_text(aes(label = label), color = "black", size = 3) +
    scale_y_discrete(position = "right") 
  
  return(position_age_corr_plot)
}

gk_age_corr <- plot_age_corr(ages_overall_corr, "goalkeeper", "Correlation between attributes and Overall Rating for goalkeepers by age range")
defense_age_corr <- plot_age_corr(ages_overall_corr, "defense", "Correlation between attributes and Overall Rating for defense players by age range")
middle_age_corr <- plot_age_corr(ages_overall_corr, "middle", "Correlation between attributes and Overall Rating for middle players by age range")
attack_age_corr <- plot_age_corr(ages_overall_corr, "attack", "Correlation between attributes and Overall Rating for attack players by age range")

gk_age_corr
defense_age_corr
middle_age_corr
attack_age_corr
