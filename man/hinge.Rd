\name{hinge}
\alias{hinge}
\alias{thresholds}
\alias{categorical}
\title{Maxent feature classes}
\description{Create and evaluate Maxent's feature classes}
\usage{
hinge(x, nknots = 50)
thresholds(x, nknots=50)
categorical(x)
}
\arguments{
  \item{x}{ a predictor: a factor for categorical, otherwise numeric. }
  \item{nknots}{ number of knots. }
}
\value{
  These functions are typically called by \code{model.matrix} rather than directly by a user.  \code{hinge}, \code{threshold} and \code{categorical} return a matrix with a column for each feature of the specified type.  \code{hinge} creates \code{2*nknots-2} hinge features, half with \code{min=min(x)} and half with \code{max=max(x)}, and knots evenly spaced between \code{min(x)} and \code{max(x)}.  A hinge feature \code{h(min,knot)} or \code{h(knot,max)} is 0 if the predictor is below the first argument, 1 if the predictor is above the second argument, and linearly interpolated inbetween.  A threshold feature is 1 if the predictor is above the knot, 0 otherwise.  A categorical feature is 1 if the predictor matches the category and 0 otherwise.
}
\author{
  Steven Phillips
}
\examples{
library(maxnet)
data(bradypus)
hinge(bradypus$tmp6190_ann,nknots=10)
categorical(bradypus$ecoreg)
}
\keyword{Maxent}
