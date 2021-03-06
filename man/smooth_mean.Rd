% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_functions.R
\name{smooth_mean}
\alias{smooth_mean}
\title{Smooth the mean values}
\usage{
smooth_mean(.data, key, .col = ".mean", span = 0.5, na.action = "na.exclude")
}
\arguments{
\item{.data}{A data frame from a \code{forecast} object}

\item{key}{Column to group by}

\item{.col}{String with the name of the column to smooth, default is ".mean"}

\item{span}{Parameter which controls the degree of smoothing}

\item{na.action}{Action to be taken with missing values in the response or
predictors. The default is "na.exclude".}
}
\value{
Returns a \code{tibble} object.
}
\description{
Applies the \link[stats]{loess} function to time series values and adds the
fitted values to the data frame.
}
