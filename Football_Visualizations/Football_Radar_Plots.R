### Football Radars Package
##############################################################################
################ Lets download some data from understat ######################
##############################################################################
### remotes::install_github('ewenme/understatr')
library(understatr)
library(tidyverse)
library(janitor)
### Check currently available leagues/seasons
get_leagues_meta()
### Let's do the Premier League
epl_2015 <- understatr::get_league_teams_stats("EPL", 2000)
epl_2020 <- understatr::get_league_teams_stats("EPL", 2020)
### This function fetches the team data from understat
fetch_understat_data <- function(team = 'Arsenal', year = '2020') {
  understat <- xml2::read_html(glue::glue('https://understat.com/team/{team}/{year}'))
  understat_dat <- understat %>%
    rvest::html_nodes('script') %>%
    as.character() %>%
    stringr::str_subset('playersData') %>%
    stringi::stri_unescape_unicode()  %>%
    stringr::str_extract('\\[.+\\]') %>%
    jsonlite::fromJSON(flatten = T) %>%
    select(team = team_title, player_name, position,
           games_played = games, minutes_played = time, goals,
           non_pen_goals = npg, npxG, assists, xG, xA, xGChain,
           xGBuildup, shots, key_passes, yellow_cards, red_cards) %>%
    mutate(xG90 = ((as.numeric(xG) /
                      (as.numeric(minutes_played) / 90))),
           xA90 = ((as.numeric(xA) /
                      (as.numeric(minutes_played) / 90))),
           NPxG90 = ((as.numeric(npxG) /
                        (as.numeric(minutes_played) / 90))),
           xG90_xA90 = ((as.numeric(xG) /
                           (as.numeric(minutes_played) / 90)) + 
                          (as.numeric(xA) /
                             (as.numeric(minutes_played) / 90))),
           NPxG90_xA90 = ((as.numeric(npxG) /
                             (as.numeric(minutes_played) / 90)) + 
                            (as.numeric(xA) /
                               (as.numeric(minutes_played) / 90))),
           shots_90 = (as.numeric(shots) /
                         (as.numeric(minutes_played) / 90)),
           key_pass_90 = (as.numeric(key_passes) /
                            (as.numeric(minutes_played) / 90)),
           xGChain_90 = (as.numeric(xGChain) /
                           (as.numeric(minutes_played) / 90)),
           xGBuildup_90 = (as.numeric(xGBuildup) /
                             (as.numeric(minutes_played) / 90)))
  return(understat_dat)
}
### For loop with purrr::map to create dataframes of teams
### Only have data starting in 2014
years <- seq(2014, 2020, by = 1)
epl_list_2020 <- list()
epl_list_2020 <- map(.x = team_names, .f = fetch_understat_data, year = 2020)
epl_dataframe_2020 <- do.call(rbind, epl_list_2020)
epl_dataframe_2020 <- cbind(year = rep(2020, nrow(epl_dataframe_2020)), epl_dataframe_2020)
epl_tbl_2020 <- as_tibble(epl_dataframe_2020)
### Fbref data
url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=fb&url=%2Fen%2Fcomps%2F9%2Fstats%2FPremier-League-Stats&div=div_stats_standard"
f <- url %>% 
  xml2::read_html() %>%
  rvest::html_nodes('table') %>%
  rvest::html_table() %>%
  .[[1]]
f <- f[, -ncol(f)]
names(f) <- as.matrix(f[1, ])
f <- f[-1, ]
f[] <- lapply(f, function(x) type.convert(as.character(x)))
# f_new <- f
for (i in seq(length(f) - 4, length(f), by = 1)) {
  colnames(f)[i] <- paste0(names(f[i]), ".90")
}
for (i in seq(19, 24, by = 1)) {
  colnames(f)[i] <- paste0(names(f[i]), ".90")
}
f <- f[!grepl("Rk", f$Rk),]
f <- f[, -1]
f <- f %>%
  mutate(Player = recode(Player, 
                         "AdriÃ¡n" = "Adrian",
                         "Sergio AgÃ¼ero" = "Sergio Aguero",
                         "Rayan AÃ¯t Nouri" = "Rayan Ait Nouri",
                         "Nathan AkÃ©" = "Nathan Ake",
                         "Thiago AlcÃ¡ntara" = "Thiago Alcantara",
                         "RÃºnar Alex RÃºnarsson" = "Runar Alex Runarsson",
                         "Miguel AlmirÃ³n" = "Miguel Almiron",
                         "CÃ©sar Azpilicueta" = "Caesar Azpilicueta",
                         "FabiÃ¡n Balbuena" = "Fabian Balbuena",
                         "HÃ©ctor BellerÃ­n" = "Hector Bellerin",
                         "SaÃ¯d Benrahma" = "Said Benrahma",
                         "JÃ³hann Berg GuÃ°mundsson" = "Johann Berg Gudmundsson",
                         "JoÃ£o Cancelo" = "Joao Cancelo",
                         "SÃ©amus Coleman" = "Saemus Coleman",
                         "HÃ©lder Costa" = "Helder Costa",
                         "VladimÃ­r Coufal" = "Vladimir Coufal",
                         "RÃºben Dias" = "Ruben Dias",
                         "Abdoulaye DoucourÃ©" = "Abdoulaye Doucoure",
                         "Martin DÃºbravka" = "Martin Dubravka",
                         "Åukasz FabiaÅski" = "Lucas Fabianski",
                         "Federico FernÃ¡ndez" = "Frederico Fernandez",
                         "Eric GarcÃ­a" = "Eric Garcia",
                         "AndrÃ© Gomes" = "Andre Gomes",
                         "Pascal GroÃ" = "Pascal Gros",
                         "Ä°lkay GÃ¼ndoÄan" = "Ilkay Gundogan",
                         "SÃ©bastien Haller" = "Sebastien Haller",
                         "Pablo HernÃ¡ndez" = "Pablo Hernandez",
                         "Pierre HÃ¸jbjerg" = "Pierre Emil Hojbjerg",
                         "RaÃºl JimÃ©nez" = "Raul Jimenez",
                         "Willian JosÃ©" = "Willian Jose",
                         "N'Golo KantÃ©" = "Ngolo Kante",
                         "Naby KeÃ¯ta" = "Naby Keita",
                         "CaoimhÃ­n Kelleher" = "Caoimhin Kelleher",
                         "Sead KolaÅ¡inac" = "Sead Kolasinac",
                         "Cheikhou KouyatÃ©" = "Cheikhou Koyate",
                         "Mateo KovaÄiÄ" = "Mateo Kovacic",
                         "Filip KrovinoviÄ" = "Filip Krovinovic",
                         "Ãrik Lamela" = "Erik Lamela",
                         "Victor LindelÃ¶f" = "Victor Lindelof",
                         "Sadio ManÃ©" = "Sadio Mane",
                         "Fernando MarÃ§al" = "Fernando Marcal",
                         "Pablo MarÃ" = "Pablo Mari",
                         "Emiliano MartÃnez" = "Emiliano Martinez",
                         "Nemanja MatiÄ" = "Nemaja Matic",
                         "JoÃ«l Matip" = "Joel Matip",
                         "Luka MilivojeviÄ" = "Luka Milivojevic",
                         "Aleksandar MitroviÄ" = "Aleksandar Mitrovic",
                         "JoÃ£o Moutinho" = "Joao Moutinho",
                         "RÃºben Neves" = "Ruben Neves",
                         "Rui PatrÃcio" = "Rui Patricio",
                         "Nicolas PÃ©pÃ©" = "Nicola Pepe",
                         "Ayoze PÃ©rez" = "Ayoze Perez",
                         "Davy PrÃ¶pper" = "Davy Propper",
                         "Sergio ReguilÃ³n"= "Sergio Reguilon",
                         "JaÃ¯ro Riedewald" = "Jairo Riedewald",
                         "Marek RodÃ¡k" = "Marek Rodak",
                         "James RodrÃ­guez" = "James Rodriguez",
                         "Antonio RÃ¼diger" = "Antonio Rudiger",
                         "Romain SaÃ¯ss" = "Roman Saiss",
                         "Davinson SÃ¡nchez" = "Davinson Sanchez",
                         "Robert SÃ¡nchez" = "Robert Sanchez",
                         "NÃ©lson Semedo" = "Nelson Semedo",
                         "Gylfi SigurÃ°sson" = "Gylfi Sigurdsson",
                         "CÃ©dric Soares" = "Cedric Soares",
                         "TomÃ¡Å¡ SouÄek" = "Tomas Soucek",
                         "ÃaÄlar SÃ¶yÃ¼ncÃ¼" = "Caglar Soyuncu",
                         "FerrÃ¡n Torres" = "Ferran Torres",
                         "Adama TraorÃ©" = "Adama Traore",
                         "Bertrand TraorÃ©" = "Bertrand Traore",
                         "TrÃ©zÃ©guet" = "Trezerguet",
                         "RÃºben Vinagre" = "Ruben Vinagre",
                         "Carlos VinÃ­cius" = "Carlos Vinicius",
                         "JoÃ£o VirgÃnia" = "Joao Virginia",
                         "MatÄj Vydra" = "Matej Vydra"))

### Create a data frame from Messi and AdamaTraore's FIFA 2018 attribute data 
### This is subject to change, hoping to apply data from the internet to this
### radar file.
LionelMessi <- c(Pace = 96, Shooting = 97, Passing = 98,
                 Dribbling = 99, Defending = 45, Physical = 81)
AdamaTraore <- c(Pace = 93, Shooting = 60, Passing = 59,
                 Dribbling = 80, Defending = 24, Physical = 70)
data <- rbind(LionelMessi, AdamaTraore)
Attributes <- colnames(data)
AttNo <- length(Attributes)
data <- cbind(data, data[, 1])

mason_mount <- f[f$Player == "Mason Mount", ] %>%
  select(xG.90.1, xA.90, npxG.90, `npxG+xA.90`)
jack_grealish <- f[f$Player == "Jack Grealish", ] %>%
  select(xG.90.1, xA.90, npxG.90, `npxG+xA.90`)
data <- rbind(mason_mount, jack_grealish)
data <- sapply(data, function(x) {as.numeric(as.character(x))})
data <- data %>%
  data.frame()
data <- data %>%
  rename(npxG_xA_90 = npxG.xA.90, xG_90 = xG.90.1, xA_90 = xA.90, npxG_90 = npxG.90)
Attributes <- colnames(data)
AttNo <- length(Attributes)
data <- data %>%
  as.matrix()
rownames(data) <- c("Mason Mount", "Jack Grealish")
data <- cbind(data, data[, 1])
### Require makes sure ggplot2 is installed and loaded
require(ggplot2)
### Empty plot with size of x = c(-120, 120) and y = c(-120, 150)
init_plot <- ggplot() +
  xlim(c(-1.2, 1.5)) +
  ylim(c(-1.20, 1.50))
### Print initial plot
# init_plot
### Create circle board function to use for ggplot
circle_plot_fn <- function(center = c(0,0), diameter = 1, npoints = 100){
  r <- diameter / 2
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
### Create 4 circles for ggplot
circle1 <- circle_plot_fn(c(0,0), 2, npoints = 100)
circle2 <- circle_plot_fn(c(0,0), 1.5, npoints = 100)
circle3 <- circle_plot_fn(c(0,0), 1, npoints = 100)
circle4 <- circle_plot_fn(c(0,0), 0.5, npoints = 100)
### Add circles to ggplot
### Fill = alternating between white and grey
### colour = colour of the outline
### https://www.colorbook.io/hexcolors/view/F0F0F0
colour_outer <- "black"
colour_inner <- "grey"
fill_1 <- "#F0F0F0"
fill_2 <- "#FFFFFF"
empty_radar_plot <- init_plot +
  geom_polygon(data = circle1, aes(x = x, y = y),
               fill = fill_1, colour = colour_outer) +
  geom_polygon(data = circle2, aes(x = x, y = y),
               fill = fill_2, colour = colour_inner) +
  geom_polygon(data = circle3, aes(x = x, y = y),
               fill = fill_1, colour = colour_inner) +
  geom_polygon(data = circle4, aes(x = x, y = y),
               fill = fill_2, colour = colour_inner) +
  # Removes axes and grid from plot
  theme_void()
empty_radar_plot
### Divide the radians of the circle by number of attributes
angle_spilt <- (2 * pi) / AttNo
angle_spilt_seq <- seq(0, (2 * pi), angle_spilt)
angle_spilt_seq
### Creating empty dataframes to store information then calculate
### x, y positions of our radars segment dividing lines and for
### labelling of attributes and the value key.
# empty dataframes to catch results 
LineData <- data.frame(x = numeric(), y = numeric(), stringsAsFactors = FALSE)
TitlePositioning <- data.frame(title = character(),
                               x = numeric(), y = numeric(), stringsAsFactors = FALSE)

## create plot background construction data  
for (i in 1:NCOL(data)) {
  angle_multiplier <- if(i < NCOL(data)){i}else{1}
  radians_for_segment <- angle_spilt_seq[i]
  
  x <- 1 * cos(radians_for_segment)
  y <- 1 * sin(radians_for_segment)
  temp <- data.frame(x = x, y = y, stringsAsFactors = F)
  LineData <- rbind(temp, LineData)
  
  x <- 1.12 * cos(radians_for_segment)
  y <- 1.12 * sin(radians_for_segment)
  title <- colnames(data)[i]
  temp <- data.frame(title = title, x = x, y = y, stringsAsFactors = F)
  TitlePositioning <- rbind(temp, TitlePositioning)
}

## create the value labellings data 
values <- c(0.15, 0.30, .45, 0.60)
radian_for_values <- angle_spilt / 2
x <- values * cos(radian_for_values)
y <- values * sin(radian_for_values)
ValuePositioning <- data.frame(values = values, x = x, y = y, stringsAsFactors = FALSE)

## Add the origin values for the lines 
LineData$x2 <- 0
LineData$y2 <- 0

## check the data output 
LineData
TitlePositioning
### Now let's add the data to the radar
new_radar_plot <- empty_radar_plot +
  geom_segment(data = LineData, aes(x = x, y = y, xend = x2, yend = y2),
               colour = "grey", linetype = "dashed") +
  annotate("text", x = TitlePositioning$x, y = TitlePositioning$y,
           label = TitlePositioning$title, size = 2.5) +
  annotate("text", x = ValuePositioning$x, y = ValuePositioning$y,
           label = ValuePositioning$values, size = 2.5)
new_radar_plot
### Empty dataframe to update with player data
polydata <- data.frame(player = character(), value = numeric(),
                       radians = numeric(), x = numeric(), y = numeric(),
                       stringsAsFactors = FALSE)
### Create polygon data for the players 
for (i in seq_len(ncol(data))) {
  for (p in seq_len(nrow(data))) {
    
    player2calc <- data[p, ]
    angle_multiplier <- if(i < ncol(data)) {
      i
    } else {
        1
      }
    radians_for_segment <- angle_spilt_seq[i]
    x <- player2calc[i] * cos(radians_for_segment)
    y <- player2calc[i] * sin(radians_for_segment)
    player <- rownames(data)[p]
    temp <- data.frame(player = player, value = player2calc[i],
                       radians = radians_for_segment,
                       x = x, y = y,
                       stringsAsFactors = FALSE)
    polydata <- rbind(temp, polydata)
  }
}

### Split the data up into player 1 and 2
playersDB <- unique(polydata$player)
player1 <- polydata[which(polydata$player == playersDB[1]), ]
player2 <- polydata[which(polydata$player == playersDB[2]), ]

### Apply data into ggplot
### Determine player1 colours
col_player1 <- "#A30026"
### Create the title string for player1
title_player1 <- gsub('([[:upper:]])', ' \\1', playersDB[1])
title_player1 <- trimws(title_player1)
### Determine player2 colours
col_player2 <- "#00B20B"
### Create the title string for player2
title_player2 <- gsub('([[:upper:]])', ' \\1', playersDB[2])
title_player2 <- trimws(title_player2)
### Font
font <- "Helvetica"
### Plot data for both players
new_radar_plot +
  # Add player 1 data 
  geom_polygon(data = player1, aes(x = x, y = y),
               fill = col_player1, colour = col_player1, alpha = 0.3) +
  geom_point(data = player1, aes(x = x, y = y),
             size = 0.3, colour = col_player1) +
  # Add Chart Title
  annotate("text", x = -1.15 , y = 1.3, label = title_player1,
           size = 5, colour = col_player1, family = font,
           fontface = "bold", hjust = 0) + 
  # Add the player 2 polygon and data points
  geom_polygon(data = player2, aes(x = x, y = y),
               fill = col_player2, colour = col_player2, alpha = 0.3) +
  geom_point(data = player2, aes(x = x, y = y),
             size = 0.3, colour = col_player2) +
  # Add the titles for player 2
  annotate("text", x = -1.10 , y = 1.1, label = title_player2, size = 5,
           colour = col_player2, family = font,
           fontface = "bold", hjust = 0) +
  # Add vs. and FIFA 18 Data title. This is subject to change.
  annotate("text", x = -1.10, y = 1.20 , label = "vs.", size = 4,
           colour = "grey", family = font, hjust = 0) +
  annotate("text", x = 1.25 , y = 1.30, label = "FBRef Data",
           size = 4, colour = "grey", family = font,
           fontface = "bold", hjust = 1)

