% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/categorical.R, R/hinge.R, R/thresholds.R
\name{categorical}
\alias{categorical}
\alias{hinge}
\alias{thresholds}
\title{Maxent feature classes}
\usage{
categorical(x)

hinge(x, nknots = 50)

thresholds(x, nknots = 50)
}
\arguments{
\item{x}{a predictor: a factor for categorical, otherwise numeric}

\item{nknots}{number of knots}
}
\value{
\code{hinge}, \code{threshold} and \code{categorical}
return a matrix with a column for each feature of the specified type.
}
\description{
Create and evaluate Maxent's feature classes.

These functions are typically called by \code{model.matrix} rather than
directly by a user.

\code{hinge} creates \code{2*nknots-2} hinge features, half with
\code{min=min(x)} and half with \code{max=max(x)}, and knots evenly spaced
between \code{min(x)} and \code{max(x)}.  A hinge feature \code{h(min,knot)}
or \code{h(knot,max)} is 0 if the predictor is below the first argument, 1 if
the predictor is above the second argument, and linearly interpolated
inbetween.

A \code{threshold} feature is 1 if the predictor is above the knot, 0
otherwise.

A \code{categorical} feature is 1 if the predictor matches the category
and 0 otherwise.
}
\examples{
\dontrun{
  library(maxnet)
  data(bradypus)
  hinge(bradypus$tmp6190_ann,nknots=10)
  categorical(bradypus$ecoreg)
 }
}
\author{
Steve Phillips
}
