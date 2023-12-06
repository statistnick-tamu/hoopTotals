#' plotTotal
#'
#' @param ppm points per minute combined of the two teams
#' @param var ppm variance of the two teams
#' @param threshold scoring threshold to meet
#' @param line betting line (expected points scored)
#'
#' @return prints out a hypothetical scoring flow chart
#' @export
#'
#' @examples
#' plotTotal(4.96, 14.38, 168.5, 150.5)

plotTotal <- function(ppm, var, threshold, line){

  ## data checks

  if(is.null(ppm) || ppm < 0){
    stop("Please pass in valid points per minute")
  }

  if(is.null(var) || var < 0){
    stop("Please pass in valid scoring variability")
  }

  if(is.null(threshold) || threshold < 0){
    stop("Please pass in a valid scoring threshold")
  }

  if(is.null(line) || line < 0){
    stop("Please pass in the O/U line")
  }

  # params
  i <- 1
  est.p <- rep(0,40)
  ybar <- ppm
  v <- var
  lambda.hat <- ybar/v
  eta.hat <- lambda.hat * line

  # generate path
  for(i in i:40){
    est.p[i] <- (rgamma(1, rate=lambda.hat, shape=eta.hat)) * (i/40)
    if(i > 1 && est.p[i] < est.p[i-1]){
      est.p[i] <- est.p[i-1]
    }
  }

  est.df <- data.frame(cbind(1:40,est.p))

  # construct plot
  ggplot2::ggplot(est.df, ggplot2::aes(x = V1, y = est.p)) +
    ggplot2::geom_step(linewidth = 1, aes(colour="black")) +
    ggplot2::geom_hline(aes(yintercept=140, colour = "dodgerblue"), show.legend = FALSE) +
    ggplot2::labs(title = "Hypothetical game flow") +
    ggplot2::labs(x = "Minutes", y = "Points") +
    ggplot2::scale_colour_manual(name='Legend',
                        labels = c("Projection", "Threshold"),
                        values=c("black", "dodgerblue")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
