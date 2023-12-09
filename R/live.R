#' live
#'
#'@description
#'Calculates the total expected points for an in progress D1 Men's College Basketball game.
#'Also calcuates the probability the score exceeds a provided threshold and includes a conversion
#'to what an American betting line would be.
#'
#'
#' @param ppm Points per minute combined of the two teams playing. This defaults to the NCAA 2021-22 & 22-23 average.
#' @param var Variance of PPM of the two teams. This defaults to the NCAA 2021-22 & 22-23 average.
#' @param time Time passed in the game. Defaults to half game (20min).
#' @param current current points scored
#' @param threshold scoring threshold to meet
#' @param line Optional - betting line (expected points scored)
#'
#' @return A list with the elements (if provided 'current' and 'threshold')
#'   \item{exp.pts}{the expected points from the game}
#'   \item{prob}{the probability it exceeds the provided threshold}
#'   \item{price}{the estimated betting line}
#' @export
#'
#' @examples
#' live(4.35, 6.9, time = 20, current = 89, threshold = 168.5, line = 150.5)

live <- function(ppm = 3.5, var = 5, time = 20, current, threshold, line = NULL){

  ## data checks

  if(ppm < 0){
    stop("Please pass in valid points per minute")
  }

  if(var < 0){
    stop("Please pass in valid scoring variability")
  }

  if(time < 0 || time > 40){
    stop("Please pass in a valid time")
  }

  if(is.null(current) || current < 0){
    stop("Please pass in a valid current score")
  }

  if(is.null(threshold) || threshold < current){
    stop("Please pass in a valid threshold")
  }

  if(!is.null(line) && line < 0){
    stop("Please pass in a valid O/U line")
  }

  # initialize params
  ybar <- ppm
  v <- var
  t <- time/40
  h <- 0
  prob <- 0
  price <- 0

  # calculate parameters
  lambda.hat <- ybar/v
  eta.hat <- 40 * lambda.hat * ybar

  # expected points based on gamma process model
  exp.pts <- mean(rgamma(1000, rate=lambda.hat, shape=eta.hat * t))

  # probability that the final total points are greater than a threshold
  # given the current total points at time t
  tau <- threshold
  h <- current
  prob <- gammainc(eta.hat * (1-t), lambda.hat * (tau - h))[[3]]
  exp.pts <- exp.pts + h

  # if supplied line
  if(!is.null(line)){
    tot.line <- line
    eta.hat.b <- lambda.hat * tot.line
    exp.pts <- mean(rgamma(1000, rate=lambda.hat, shape=eta.hat.b * t)) + h
    prob <- gammainc(eta.hat.b * (1-t), lambda.hat * (tau - h))[[3]]
  }

  # calculate a price using American odds if there's a probability
  if(prob > .5){
    price <- prob[which(prob > 0.5)] / (1 - prob[which(prob > 0.5)]) * -100
  }
  else if(prob <= .5 && prob > 0){
    price <- (1 - prob[which(prob <= 0.5)]) / prob[which(prob <= 0.5)] * 100
  }
  price <- round(price)

  return(list(exp.pts = exp.pts, prob = prob, price = price))
}
