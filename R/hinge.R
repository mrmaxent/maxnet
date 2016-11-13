#' @export
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
