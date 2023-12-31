% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train.R
\name{train}
\alias{train}
\title{train}
\usage{
train(home.df, away.df)
}
\arguments{
\item{home.df}{play-by-play data frame for the home team. Data must be formatted in the following way:
col 1 = unique game identifier
col 2 = seconds remaining in the game
col 3 = team 1 score
col 4 = team 2 score}

\item{away.df}{play-by-play data frame for the home team. Data must be formatted in the following way:
col 1 = unique game identifier
col 2 = seconds remaining in the game
col 3 = team 1 score
col 4 = team 2 score}
}
\value{
Average & Variance for PPM to use in total, live, and plotTotal functions.
}
\description{
Reads in two data frames of play-by-play data and calculates the average and variance
of the two teams. The results are used in other functions from the hoopTotals package.
Providing more than 5 years' worth of play-by-play may impact performance.
}
\examples{
train(home.df, away.df)
}
