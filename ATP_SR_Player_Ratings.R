# Load required libraries

library(tidyverse)
library(PlayerRatings)
library(lubridate)
library(reshape2)

### import results from csv (Jan 2000 to Jan 2021)

#players <- read_csv("atp_players.csv", col_names = T)
results <- read_csv("atp_results_db.csv", col_names = T)

unique(results$tourney_level)

str(results$tourney_date)

### filter matches by valid tour levels (ignore exhibitions etc.)
tours = c("G", "M", "A", "F", "D")

results$tourney_date <- ymd(results$tourney_date)

results <- results %>% 
  filter(tourney_level %in% tours)

### Select earliest date in data frame as base date
base_date <- min(results$tourney_date)

head(results)
tail(results)

unique(results$round)

rndLevels <- c("BR","RR","ER","R128","R64","R32","R16","QF","SF","F")

results$round <- factor(results$round, rndLevels, ordered = T)

### Sort by Tournament Date
results_sorted <- results %>% 
  arrange(tourney_date, round)

### Create column which holds week value (from base date)
results_sorted$period <- lubridate::interval(base_date, results_sorted$tourney_date) %/% weeks(1)+1

is.numeric(results_sorted$period)

tail(results_sorted$period)

### Create data frame of four columns for use by PlayerRatings 'steph' function to calculate ratings
res <- results_sorted %>% 
  select(period, winner_name, loser_name)

res$result <- 1

head(res)
tail(res)

### calculate Stephenson ELO Ratings

srtng <- steph(res, init = c(1500,300), history = TRUE)

### Convert steph ratings to data frame

sr_df <- as_tibble(srtng$ratings)

head(sr_df)
tail(sr_df)   

### Remove players absent from tour for approximately 1 year+, players 
### with low ratings and players with insufficient games.

sr_current <- sr_df %>% 
  filter(Lag < 25, Rating >= 1600, Games >= 30)

head(sr_current,10)

### Write ratings to csv file
write_csv(sr_current, "atp_SR_ratings_jan21.csv")

### Convert historical ratings to data frame
sr_timeline <- as_tibble(srtng$history, rownames = "Player")
#view(head(sr_timeline))

### Create variable holding a list of current top 20 ranked players
top <- sr_current$Player[1:20]

### filter historical ratings: Top 20 players, retain only Player and 
### 100 most recent Period rating columns

sr_timeline_current <- sr_timeline %>% 
  filter(Player %in% top) %>% 
  select(Player, ends_with("Rating")) %>% 
  select(Player, last_col(offset = 99):last_col())


sr_timeline_current

### Reshape data frame for plotting
temp <- melt(sr_timeline_current, id.vars = "Player",variable.name = "Time", 
             value.name = "Rating")

head(temp, 10)
tail(temp, 10)

### Create ggplot of top 20 players Rating variations through time

unique(temp$Player)

ggplot(temp,
       aes(x = Time,
           y = Rating,
           col = Player,
           group = Player
       )) +
  labs(title = "ATP Player Ratings Through Time - Current Top 20 Rated Players (Stephenson ELO Variant)", 
       x = "Time", y = "Rating") +
  scale_x_discrete(labels = NULL) +
  ylim(1600,2100) +
  geom_hline(yintercept = c(1700,1800,1900, 2000), col = "grey", lty = 2) +
  geom_line() +
  geom_smooth(lty = 2, lwd = 0.5, col = "red") +
  facet_wrap(~ Player, nrow = 4)
