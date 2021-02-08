library(tidyverse)
library(PlayerRatings)
library(lubridate)
library(scoring)


#players <- read_csv("wta_player_db.csv", col_names = T)
results <- read_csv("atp_results_db.csv", col_names = T)

results$tourney_date <- ymd(results$tourney_date)

tours = c("G", "M", "A", "F", "D")

results <- results %>% 
  filter(tourney_level %in% tours)

base_date <- min(results$tourney_date)

head(results)
tail(results)

results_sorted <- results %>% 
  arrange(tourney_date)

results_sorted$period <- lubridate::interval(base_date, results_sorted$tourney_date) %/% weeks(1)+1

res <- results_sorted %>% 
  select(period, winner_name, loser_name)

res$result <- 1

head(res)
tail(res) 

train <- res[res$period <= 350,]
test <- res[res$period > 351 & res$period <= 700,]
valid <- res[res$period > 701,]

sRat <- steph(train, init = c(1500,300), history = TRUE)

pred <- NULL

for(i in unique(test$period)) {
  testi <- test[test$period == i,]
  predi <- predict(sRat, testi, trat = c(1500,300), thresh = 0.5)
  pred <- c(pred, predi)
  sRat <- steph(testi, sRat$ratings, init = c(1500, 300))
}

wp <- table(Result = test$result, Predictions=pred)

wp

accuracy_pct <- round(100*(wp[1,2] / (wp[1,1] + wp[1,2])),2) 

accuracy_pct

# combine train and test data to make predictions on validation data 

sRat <- NULL
sRat <- steph(rbind(train, test), init = c(1500,300),history = TRUE)
pred <- NULL

for(i in unique(valid$period)) {
  validi <- valid[valid$period == i,]
  predi <- predict(sRat, validi, trat = c(1500,300))
  pred <- c(pred, predi)
  sRat <- steph(validi, sRat$ratings, init = c(1500, 300))
}

pred
metrics(valid$result, pred)

sRat$ratings

predDF <- tibble(actual = valid$result, predicted = pred)
head(predDF)

brier <- brierscore(actual ~ predicted, data = predDF)
brier

brier_score <- round(sum(brier)/length(brier),4)
brier_score

##################################

sRat <- NULL
sRat <- steph(rbind(train, test), init = c(1500,300),history = TRUE)
pred <- NULL

for(i in unique(valid$period)) {
  validi <- valid[valid$period == i,]
  predi <- predict(sRat, validi, trat = c(1500,300), thresh = 0.5)
  pred <- c(pred, predi)
  sRat <- steph(validi, sRat$ratings, init = c(1500, 300))
}

wp <- table(Result = valid$result, Predictions = pred)
wp

accuracy_pct <- round(100*(wp[1,2] / (wp[1,1] + wp[1,2])),2) 
    
accuracy_pct


