# home.df = data frame of home team data
# away.df = data frame of away team data

## data must be formatted in the following way
## col 1 = unique game identifier
## col 2 = seconds remaining in the game
## col 3 = team 1 score
## col 4 = team 2 score

train <- function(home.df, away.df){

  ## test data format

  names(home.df) <- c("game_id", "secs_remaining", "home_score", "away_score")
  names(away.df) <- c("game_id", "secs_remaining", "home_score", "away_score")

  ## calculate per min stats
  home.param <- home.df %>%
    mutate(min = floor(secs_remaining/60)) %>%
    mutate(total = home_score+away_score) %>%
    group_by(min) %>%
    select(min, total) %>%
    summarise(total = max(total)) %>%
    mutate(ppm = total-lead(total)) %>%
    filter(!is.na(ppm)) %>%
    select(ppm) %>%
    summarise(mean = mean(ppm), var = var(ppm))

  away.param <- away.df %>%
    mutate(min = floor(secs_remaining/60)) %>%
    mutate(total = home_score+away_score) %>%
    group_by(min) %>%
    select(min, total) %>%
    summarise(total = max(total)) %>%
    mutate(ppm = total-lead(total)) %>%
    filter(!is.na(ppm)) %>%
    select(ppm) %>%
    summarise(mean = mean(ppm), var = var(ppm))

  # calculate parameters
  ybar <- (home.param$mean + away.param$mean)/2
  var <- (home.param$var + away.param$var)/2

  # Return
  # ybar - average points per minute
  # v - points per min variance
  return(list(ybar = ybar, var = var))
}
