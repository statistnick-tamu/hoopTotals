#' train
#'
#'@description
#'Reads in two data frames of play-by-play data and calculates the average and variance
#'of the two teams. The results are used in other functions from the hoopTotals package.
#'Providing more than 5 years' worth of play-by-play may impact performance.
#'
#' @param home.df play-by-play data frame for the home team. Data must be formatted in the following way:
#' col 1 = unique game identifier
#' col 2 = seconds remaining in the game
#' col 3 = team 1 score
#' col 4 = team 2 score
#' @param away.df play-by-play data frame for the home team. Data must be formatted in the following way:
#' col 1 = unique game identifier
#' col 2 = seconds remaining in the game
#' col 3 = team 1 score
#' col 4 = team 2 score
#'
#' @return Average & Variance for PPM to use in total, live, and plotTotal functions.
#' @export
#'
#' @examples
#' train(home.df, away.df)

train <- function(home.df, away.df){

  ## data checks
  if(dim(home.df)[2] != 4){
    stop("Please provide a home team data frame with 4 columns")
  }

  if(dim(away.df)[2] != 4){
    stop("Please provide an away team data frame with 4 columns")
  }

  names(home.df) <- c("game_id", "secs_remaining", "home_score", "away_score")
  names(away.df) <- c("game_id", "secs_remaining", "home_score", "away_score")

  if(!is.numeric(home.df$secs_remaining) || !is.numeric(home.df$home_score)
     || !is.numeric(home.df$away_score)){
    stop("Please provide a valid home team data frame")
  }

  if(!is.numeric(away.df$secs_remaining) || !is.numeric(away.df$home_score)
     || !is.numeric(away.df$away_score)){
    stop("Please provide a valid away team data frame")
  }

  if(length(which(home.df$secs_remaining < 0)) > 0 || length(which(home.df$home_score < 0)) > 0
     || length(which(home.df$away_score < 0)) > 0){
    stop("Please remove negative values from home team data frame")
  }

  if(length(which(away.df$secs_remaining < 0)) > 0 || length(which(away.df$home_score < 0)) > 0
     || length(which(away.df$away_score < 0)) > 0){
    stop("Please remove negative values from home team data frame")
  }

  ## calculate per min stats
  home.param <- home.df %>%
    mutate(min = floor(secs_remaining/60)) %>%
    mutate(total = home_score+away_score) %>%
    group_by(min) %>%
    select(min, total) %>%
    summarise(total = max(total)) %>%
    mutate(ppm = total-lead(total)) %>%
    filter(!is.na(ppm)) %>%
    filter(ppm >= 0) %>%
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
    filter(ppm >= 0) %>%
    select(ppm) %>%
    summarise(mean = mean(ppm), var = var(ppm))

  # calculate parameters
  ppm <- (home.param$mean + away.param$mean)/2
  var <- (home.param$var + away.param$var)/2

  return(list(ppm = ppm, var = var))
}
