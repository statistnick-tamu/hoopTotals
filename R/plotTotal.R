# plots a hypothetical path based on time remaining
plotTotal <- function(ppm, var, threshold, line){

  # params
  i <- 1
  est.p <- rep(0,40)
  ybar <- ppm
  v <- var
  lambda.hat <- ybar/v
  eta.hat <- lambda.hat * tot.line

  # generate path
  for(i in i:40){
    est.p[i] <- (rgamma(1, rate=lambda.hat, shape=eta.hat)) * (i/40)
    if(i > 1 && est.p[i] < est.p[i-1]){
      est.p[i] <- est.p[i-1]
    }
  }

  est.df <- data.frame(cbind(1:40,est.p))

  ggplot2::ggplot(est.df, ggplot2::aes(x = V1, y = est.p)) +
    ggplot2::geom_step(size = 1, aes(colour="black")) +
    ggplot2::theme_minimal() +
    ggplot2::geom_hline(aes(yintercept=140, colour = "dodgerblue"), show.legend = FALSE) +
    ggplot2::labs(title = "Hypothetical game flow") +
    ggplot2::labs(x = "Minutes", y = "Points") +
    scale_colour_manual(name='Lines',
                        labels = c("Projection", "Threshold"),
                        values=c("black", "dodgerblue"))
}
