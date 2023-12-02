# ppm = points per minute combined of the two teams
## defaults to ncaa 21-22 & 22-23 average

# var = ppm variance of the two teams
## defaults to ncaa 21-22 & 22-23 average

# time = time remaining in the game
## defaults to full game remaining

# current = current points scored
# threshold = scoring threshold to meet
# line = betting line (expected points scored)

total <- function(ppm = 3.5, var = 5, time = 40, current = NULL, threshold = NULL, line = NULL){

  ## test data format

  # initialize params
  ybar <- ppm
  v <- var
  t <- time/40
  prob <- NULL

  # calculate parameters
  lambda.hat <- ybar/v
  eta.hat <- 40 * lambda.hat * ybar

  ## expected points based on gamma process model
  exp.pts <- mean(rgamma(1000, rate=lambda.hat, shape=eta.hat * t))

  ## if supplied current and threshold
  if(!is.null(current) && !is.null(threshold)){
    # probability that the final total points are greater than a threshold
    # given the current total points at time t
    tau <- threshold
    h <- current
    prob <- gammainc(eta.hat * (1-t), lambda.hat * (tau - h))[3]
  }

  ## if supplied line
  if(!is.null(line)){
    tot.line <- line
    eta.hat.b <- lambda.hat * tot.line
    exp.pts <- mean(rgamma(1000, rate=lambda.hat, shape=eta.hat.b * t))
    prob <- gammainc(eta.hat.b * (1-t), lambda.hat * (tau - h))[3]
  }

  # Return
  # exp.pts - expected number of points scored
  # prob - probabiltity of scoring higher than supplied threshold
  return(list(exp.pts = exp.pts, prob = prob))
}
