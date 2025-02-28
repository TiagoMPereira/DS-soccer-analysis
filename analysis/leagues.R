# Analyzing the leagues
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(tidyr)
library(reshape2)
# install.packages("ggimage")
library(ggimage)

# install.packages("xtable")
library(xtable)
# print(xtable(foreign_players_countries_text), type="html", file="example.html")
library(knitr)
# install.packages("gt")
library(gt)

setwd("./analysis")
players <- read.csv("../data/processed/250015/players.csv")
#61ff00
leagues_colors <- c(
  "ENG-Premier League" = "#ed6b6b",
  "ESP-La Liga" = "#ede96b",
  "ITA-Serie A" = "#6bedc8",
  "GER-Bundesliga" = "#585957",
  "FRA-Ligue 1" = "#6b94ed",
  "USA-Major League Soccer" = "#6F6BED",
  "POR-Primeira Liga" = "#5b824b",
  "SAU-Pro League" = "#72ed6b",
  "NED-Eredivisie" = "#ed9b6b",
  "TUR-Süper Lig" = "#ed8789"
)
region_colors <- c(
  "Europe" = "#F36262",
  "South America" = "#E2F362",
  "Africa" = "#62F3DD",
  "Asia" = "#6297F3",
  "North/Central America" = "#38563A",
  "Oceania" = "#F362CA"
)

# leagues_logos <- c(
#   "ENG-Premier League" = "../data/images/premiere-flag.png",
#   "ESP-La Liga" = "../data/images/la-liga-flag.png",
#   "ITA-Serie A" = "../data/images/serie-a-flag.png",
#   "GER-Bundesliga" = "../data/images/bundesliga-flag.png",
#   "FRA-Ligue 1" = "../data/images/ligue-one-flag.png",
#   "USA-Major League Soccer" = "../data/images/mls-flag.png",
#   "POR-Primeira Liga" = "../data/images/primeira-liga-flag.png",
#   "SAU-Pro League" = "../data/images/rsl-flag.png",
#   "NED-Eredivisie" = "../data/images/eredivisie-flag.png",
#   "TUR-Süper Lig" = "../data/images/super-lig-flag.png"
# )
# league_logos <- data.frame(leagues_logos)
# league_logos <- cbind(league = rownames(league_logos), league_logos)
# rownames(league_logos) <- 1:nrow(league_logos)


## Grouping players
players_league <- players |> dplyr::group_by(league)










## Number of teams and players per league
players_teams_leagues <- players |>
  dplyr::group_by(league) |>
  summarise(`# Players` = n(), `# Teams` = n_distinct(team)) |> 
  mutate(`Average players per team` = round(`# Players` / `# Teams`, 2))

players_teams_leagues$league <- factor(players_teams_leagues$league, levels= players_teams_leagues$league[order(players_teams_leagues$`# Players`)])

players_teams_leagues_melt <- melt(
  players_teams_leagues, id="league"
)

players_teams_leagues_melt |>
  ggplot(aes(x=league, y=value, fill=league)) +
  geom_bar(stat="identity", alpha=0.7) +
  facet_wrap(~variable, ncol=1, strip.position="left", scales="free_y") +
  scale_fill_manual(values = leagues_colors) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size=8, angle=30, hjust = 1)
  ) +
  labs(
    x="League", y=NULL, title="Players and teams informations for leagues"
  ) +
  geom_text(aes(x=league, y=value*1.1, label=value), color="black", size=3)








## Age distribution
plot_age_distribution <- players |> 
  ggplot(aes(x=current_age, fill=league)) +
  geom_histogram(alpha=0.7, binwidth = 2) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.2, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  scale_fill_manual(values = leagues_colors) +
  xlab("") +
  ylab("Players current age") +
  facet_wrap(~league, ncol=5)
plot_age_distribution

### Does `age` follows normal distributions?
alpha <- 0.05

for (l in unique(players$league)){
  players_l <- players |> filter(league==l)
  ages <- players_l$current_age
  ks_test <- ks.test(ages, "pnorm")
  p <- ks_test$p.value
  is_normal <- if (p < alpha) FALSE else TRUE 
  print(paste("League:", l," P-value:", p, " NORMAL:", is_normal))
}

kruskal.test(current_age~league, data=players)

league_names <- unique(players$league)
p_values <- matrix(NA, nrow = length(league_names), ncol=length(league_names))
for(i in 1:length(league_names)){

  ages_i <- players |> filter(league==league_names[i])
  ages_i <- ages_i$current_age

  for(j in i:length(league_names)){
    # if(i==j) next
    
    ages_j <- players |> filter(league==league_names[j])
    ages_j <- ages_j$current_age
    
    ks_test <- ks.test(ages_i, ages_j)
    p <- ks_test$p.value
    
    # p_values[i, j] <- p
    p_values[j, i] <- p
    print(paste(league_names[i], " X ", league_names[j], " P: ", p))
    
  }
}
p_values <- data.frame(p_values)
colnames(p_values) <- league_names
rownames(p_values) <- league_names

p_values <- cbind(league=rownames(p_values), p_values)
p_values <- melt(p_values, index="league") |> tidyr::drop_na(value)
p_values$league <- factor(p_values$league, levels=league_names)
p_values$variable <- factor(p_values$variable, levels=league_names)

p_values |> 
  ggplot(aes(x=league, y=variable, fill=value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(
    low = "white",
    mid = "white",
    high = "red",
    midpoint = alpha
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size=8, angle=30, hjust = 1)
  ) +
  labs(
    title =NULL,
    x = NULL,
    y = NULL
  ) +
  geom_text(aes(label = round(value, 3)), color = "black", size = 3)









## Nationality
# install.packages("countrycode")
library(countrycode)

players$nationality.continent <- countrycode(
  sourcevar = players[, "nationality"],
  origin = "country.name",
  destination = "continent")

unique(
  players |> filter(is.na(nationality.continent)) |> select(nationality))

### Manually setting continents for "failed" countries
### All failed maps are from european countries

players = players |>
  mutate(nationality.continent = replace_na(nationality.continent, "Europe"))

### Regions
middle_east <- c("Lebanon", "Iran", "Kuwait", "Saudi Arabia", "Egypt",
                 "Tunisia", "Bahrain", "Iraq", "Oman", "Yemen", "Israel", "Libya",
                 "Syria", "Jordan", "Qatar", "Morocco", "United Arab Emirates")

south_america <- c(
    "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
    "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"
)

players <- players |> 
  mutate(nationality.region = nationality.continent) |> 
  mutate(
    nationality.region = case_when(
      nationality %in% south_america ~ "South America",
      nationality.continent == "Americas" ~ "North/Central America",
      TRUE ~ nationality.continent
    )
  )


### Keeping only foreign players
foreign_players <- players |> 
  filter(!(nationality=="England" & league=="ENG-Premier League")) |> 
  filter(!(nationality=="Spain" & league=="ESP-La Liga")) |> 
  filter(!(nationality=="Italy" & league=="ITA-Serie A")) |> 
  filter(!(nationality=="Germany" & league=="GER-Bundesliga")) |> 
  filter(!(nationality=="France" & league=="FRA-Ligue 1")) |> 
  filter(!(nationality=="Saudi Arabia" & league=="SAU-Pro League")) |> 
  filter(!(nationality=="Türkiye" & league=="TUR-Süper Lig")) |> 
  filter(!(nationality=="United States" & league=="USA-Major League Soccer")) |> 
  filter(!(nationality=="Netherlands" & league=="NED-Eredivisie")) |> 
  filter(!(nationality=="Portugal" & league=="POR-Primeira Liga"))

foreign_players_conc <- foreign_players |>
  group_by(league, nationality.region) |>
  summarise(n=n()) |>
  ungroup() |>
  group_by(league) |> 
  mutate(t=sum(n)) |> 
  mutate(concentration = n/t) |> 
  ungroup() |> 
  select(league, nationality.region, concentration)

foreign_players_conc$league <- factor(foreign_players_conc$league, levels=league_names)
continents_factor <- c("Europe", "South America", "North/Central America", "Africa", "Asia", "Oceania")
foreign_players_conc$nationality.region <- factor(foreign_players_conc$nationality.region, levels=continents_factor)

foreign_players_conc |> 
  ggplot(aes(x=nationality.region, y=league, fill=concentration)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(
    low = "white",
    high = "red",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=30, hjust = 1)
  ) +
  labs(
    title =NULL,
    x = "Region",
    y = "League"
  ) +
  geom_text(aes(label = paste(round(concentration, 3) * 100, " %")), color = "black", size = 3)






foreign_players_countries <- foreign_players |>
  group_by(league, nationality) |> 
  summarise(n = n(), nationality.region = first(nationality.region)) |> 
  ungroup() |> 
  group_by(league) |> 
  mutate(t=sum(n)) |> 
  mutate(country.concentration = n/t) |> 
  ungroup() |> 
  select(league, nationality, country.concentration, nationality.region)

foreign_players_countries$league <- factor(foreign_players_countries$league, levels=league_names)
foreign_players_countries$nationality.region <- factor(foreign_players_countries$nationality.region, levels=continents_factor)

foreign_players_countries <- foreign_players_countries |> 
  arrange(league, nationality.region, desc(country.concentration), nationality)

foreign_players_countries_text <- foreign_players_countries |> 
  arrange(league, nationality.region, desc(country.concentration), nationality) |> 
  group_by(league, nationality.region) |> 
  slice_head(n = 3) |> 
  mutate(Text = paste0(nationality, " (",round(country.concentration, 4)*100, "%)")) |> 
  summarise(Text = paste(Text, collapse = "<br>"), .groups = "drop") |> 
  pivot_wider(names_from = nationality.region, values_from = Text)
  

foreign_players_countries_text |> 
  mutate(
    Africa = lapply(Africa, gt::html),
    Asia = lapply(Asia, gt::html),
    Europe = lapply(Europe, gt::html),
    `North/Central America` = lapply(`North/Central America`, gt::html),
    Oceania = lapply(Oceania, gt::html),
    `South America` = lapply(`South America`, gt::html),
  ) |> 
  gt() |> 
  tab_header(
    title = "Players by league and region",
    subtitle = "Top 3 nationalities for each region"
  ) |> 
  cols_label(
    league = "Liga"
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "#F5F5F5")
    ),
    locations = cells_body(
      rows = c(2, 4, 6, 8, 10)
    )
  ) |> 
  tab_options(
    table.font.size = 12,
    data_row.padding = px(5)
  )





### Plot the players' countries for specific league
players_countries <- foreign_players |> 
  group_by(league, nationality) |> 
  summarise(n_players = n(), nationality.region = first(nationality.region)) |> 
  filter(n_players >= 5) |> 
  ungroup()

players_countries$league <- factor(players_countries$league, levels=league_names)

players_countries <- players_countries |> 
  arrange(league, desc(n_players))

plot_countries_by_league <- function(players_countries, league_name, top=-1){
  
  if(top>0){
    players_countries <- players_countries |> 
      slice_head(n=top)
  }
  
  players_countries <- players_countries |> 
    mutate(league=as.character(league)) |> 
    filter(league==league_name)
  
  figure <- players_countries |> 
    ggplot(aes(x=reorder(nationality, -n_players), y=n_players, fill=nationality.region)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = region_colors) +
    labs(
      x="Country",
      y="# Players",
      title=league_name,
      fill="Region"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0)) +
    geom_text(aes(x=nationality, y=n_players+1, label=n_players), color="black", size=3)
  
  return(figure)
}
p <- plot_countries_by_league(
  players_countries=players_countries,
  league_name="POR-Primeira Liga")
p




## Wage per league

plot_leagues_wage_no_out <- players |> 
  ggplot(aes(x=reorder(league, wage, na.rm=TRUE), y=wage, fill=league)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(
    limits = quantile(players$wage, c(0.05, 0.9), na.rm=TRUE),
    labels = scales::label_number(scale_cut = cut_short_scale())
  ) +
  scale_fill_manual(values = leagues_colors) +
  coord_flip() +
  theme(legend.position="none") +
  labs(
    title="Wage distribution accross the leagues (€) - Excluding outliers",
    x="League",
    y="Wage (€)"
  )
plot_leagues_wage_no_out


## Value per league

plot_leagues_value_no_out <- players |> 
  ggplot(aes(x=reorder(league, net_worth, na.rm=TRUE), y=net_worth, fill=league)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(
    limits = quantile(players$net_worth, c(0.05, 0.9), na.rm=TRUE),
    labels = scales::label_number(scale_cut = cut_short_scale())
  ) +
  scale_fill_manual(values = leagues_colors) +
  coord_flip() +
  theme(legend.position="none") +
  labs(
    title="Players' value distribution accross the leagues (€)Excluding outliers",
    x="League",
    y="Players' value (€)"
  )
plot_leagues_value_no_out



