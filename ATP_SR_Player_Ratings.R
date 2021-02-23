# Load required libraries

library(tidyverse)
library(PlayerRatings)
library(lubridate)
library(reshape2)
library(plotly)
library(crosstalk)
library(RPostgres)
library(DBI)


### import results from wta results database from Jan 2000)

username <- readline(prompt = "Enter postgresql Username: ")
dbpw <- readline(prompt = "Enter postgresql password: ")

con <- dbConnect(RPostgres::Postgres(), dbname = 'atp',
                 host = 'localhost',
                 port = 5432,
                 user = username,
                 password = dbpw)

qry <- "SELECT * from atp_results 
          where extract(year from tourney_date) >= 2000"

dbListFields(con, "atp_results")

results <- as_tibble(dbGetQuery(con, qry))

dbDisconnect(con)

head(results)

unique(results$tourney_level)

### filter matches by valid tour levels (ignore exhibitions etc.)
tour_levels = c("G", "M", "A", "F", "D")

results$tourney_date <- ymd(results$tourney_date)

results <- results %>% 
  filter(tourney_level %in% tour_levels)

### Select earliest date in data frame as base date
base_date <- min(results$tourney_date)

head(results)
tail(results)

unique(results$round)

rndLevels <- c("RR","ER","R128","R64","R32","R16","QF","BR","SF","F")

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
  filter(Lag < 20, Rating >= 1600, Games >= 30)

head(sr_current,10)

### Write ratings to csv file
write_csv(sr_current, "atp_SR_ratings_feb21.csv")

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

p <- ggplot(temp,
       aes(x = Time,
           y = Rating,
           #col = Player,
           group = Player
       )) +
  labs(title = "ATP Player Ratings Through Time - Current Top 20 Rated Players (Stephenson ELO Variant)", 
       x = "Time", y = "Rating") +
  scale_x_discrete(labels = NULL) +
  ylim(1500,2100) +
  geom_hline(yintercept = c(1600,1700,1800,1900, 2000), col = "grey", lty = 2) +
  geom_line(col = "blue") +
  geom_smooth(lty = 2, lwd = 0.2, col = "red") +
  facet_wrap(~ Player, nrow = 4)

p

ggplotly(p, tooltip = c("Player","Rating"))
