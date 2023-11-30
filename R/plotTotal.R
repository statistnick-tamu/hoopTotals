## create flow chart
set.seed(77843)

i <- 1
est.p <- rep(0,40)
for(i in 1:40){
  est.p[i] <- rgamma(1, rate=lambda.hat, shape=eta.hat * (i/40))
  if(i > 1 && est.p[i] < est.p[i-1]){
    est.p[i] <- est.p[i-1]
  }
}
plot(est.p)
