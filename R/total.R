#' total
#'
#'@description
#'Calculates the total points expected to be scored in a standard D1 Men's College Basketball game.
#'
#' @param ppm Points per minute combined of the two teams playing. This defaults to the NCAA 2021-22 & 22-23 average.
#' @param var Variance of PPM of the two teams. This defaults to the NCAA 2021-22 & 22-23 average.
#' @param line Optional - betting line (expected points scored)
#'
#' @return expected points as a scalar
#'   \item{exp.pts}{the expected points from the game}
#'
#' @export
#'
#' @examples
#' total(4.96, 14.38)
#' total(4.35, 6.9, line = 150.5)

total <- function(ppm = 3.5, var = 5, line = NULL){

  ## data checks

  if(ppm < 0){
    stop("Please pass in valid points per minute")
  }

  if(var < 0){
    stop("Please pass in valid scoring variability")
  }

  if(!is.null(line) && line < 0){
    stop("Please pass in a valid O/U total")
  }

  # initialize params
  ybar <- ppm
  v <- var

  # calculate parameters
  lambda.hat <- ybar/v
  eta.hat <- 40 * lambda.hat * ybar

  # expected points based on gamma process model
  exp.pts <- mean(rgamma(1000, rate=lambda.hat, shape=eta.hat))

  # if supplied line
  if(!is.null(line)){
    tot.line <- line
    eta.hat.b <- lambda.hat * tot.line
    exp.pts <- mean(rgamma(1000, rate=lambda.hat, shape=eta.hat.b))
  }
  return(exp.pts)
}
