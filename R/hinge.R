
#' Maxent feature classes
#'
#' @description 
#' Create and evaluate Maxent's feature classes.
#'
#' These functions are typically called by \code{model.matrix} rather than
#' directly by a user.
#' 
#' \code{hinge} creates \code{2*nknots-2} hinge features, half with
#' \code{min=min(x)} and half with \code{max=max(x)}, and knots evenly spaced
#' between \code{min(x)} and \code{max(x)}.  A hinge feature \code{h(min,knot)}
#' or \code{h(knot,max)} is 0 if the predictor is below the first argument, 1 if
#' the predictor is above the second argument, and linearly interpolated
#' inbetween.  
#' 
#' A \code{threshold} feature is 1 if the predictor is above the knot, 0
#' otherwise.  
#' 
#' A \code{categorical} feature is 1 if the predictor matches the category
#' and 0 otherwise.
#' 
#' @author Steve Phillips
#' @export
#' @param x a predictor: a factor for categorical, otherwise numeric
#' @param nknots  number of knots
#' @return \code{hinge}, \code{threshold} and \code{categorical}
#' return a matrix with a column for each feature of the specified type.
#' @examples
#' \dontrun{
#'   library(maxnet)
#'   data(bradypus)
#'   hinge(bradypus$tmp6190_ann,nknots=10)
#'   categorical(bradypus$ecoreg)
#'  }
hinge <-
function(x, nknots=50)
{
   min <- min(x)
   max <- max(x)
   k <- seq(min, max, length=nknots)
   lh <- outer(x, utils::head(k,-1), function(w,h) hingeval(w, h, max))
   rh <- outer(x, k[-1], function(w,h) hingeval(w, min, h))
   colnames(lh) <- paste("", utils::head(k,-1), max, sep=":")
   colnames(rh) <- paste("", min, k[-1], sep=":")
   cbind(lh, rh)
}
