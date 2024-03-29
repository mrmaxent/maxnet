% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.maxnet.R
\name{plot.maxnet}
\alias{plot.maxnet}
\title{Create response plots for user selected predictors in a maxnet model}
\usage{
\method{plot}{maxnet}(
  x,
  vars = names(x$samplemeans),
  common.scale = TRUE,
  type = c("link", "exponential", "cloglog", "logistic"),
  ylab = NULL,
  plot = TRUE,
  mar = c(5, 5, 4, 2),
  N = 100,
  ...
)
}
\arguments{
\item{x}{an object of class maxnet, i.e., a fitted model.}

\item{vars}{character, vector of predictors for which response plots are desired.}

\item{common.scale}{logical,    if true, all plots use the same scale on the y-axis.}

\item{type}{character, type of response to plot on y-axis.}

\item{ylab}{character, label for y-axis}

\item{plot}{logical, if TRUE render a plot, if FALSE return a list of
data frames with variable and response columns}

\item{mar}{numeric, 4 element value for margins (lines, in order of bottom, left, top, right)
See \code{par} for details.}

\item{N}{numeric, the number of intervals over which to sample the response}

\item{...}{other arguments passed to \code{plot} or \code{barplot}}
}
\value{
if \code{plot} is \code{FALSE} then return a list of data frames
that contain variable and response columns otherwise \code{NULL} invisibly
}
\description{
Create response plots for user selected predictors in a maxnet model
}
\seealso{
\code{\link{response.plot}}
}
