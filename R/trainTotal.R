## trained gamma process

## test data format

## implement specific
home.df <- tamu.22[,c(1,6,8,12,13)]
away.df <- isu.22[,c(1,6,8,12,13)]

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

ybar <- (home.param$mean + away.param$mean)/2
v <- (home.param$var + away.param$var)/2

lambda.hat <- ybar/v
eta.hat <- 40 * lambda.hat * ybar

# current time, default to half
t <- .5

## gamma process model
rgamma(1, rate=lambda.hat, shape=eta.hat * t)

## line adjustment if avail
