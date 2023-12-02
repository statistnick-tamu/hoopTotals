library(pracma)
## general gamma

## test data format

## implement w/ ncaa averages from 21-22 & 22-23

# ybar, points scored per minute
ybar <- 3.48

# var, (actual - ybar)^2
var <- 5.02
#var <- 5.02**2

lambda.hat <- ybar/var
eta.hat <- 40 * lambda.hat * ybar

# current time, default to half
t <- .5

## gamma process model
rgamma(1, rate=lambda.hat, shape=eta.hat * t)

# probability that the final total points are greater than a threshold
# given the current total points at time t

# threshold
tau <- 180

# current pts
h <- 75
prob <- gammainc(eta.hat * (1-t), lambda.hat * (tau - h))[3]

## line adjustment if avail
tot.line <- 135.5
eta.hat.b <- lambda.hat * tot.line
rgamma(1, rate=lambda.hat, shape=eta.hat.b * t)

