#' Compute and plot a single response variable
#' 
#' @seealso \code{\link{plot.maxnet}}
#' @export
#' @param mod	a fitted model, must be of type maxnet if default values used for other arguments.
#' @param v	charvacter, name of variable to be plotted.
#' @param type character, type of response to plot on y-axis.
#' @param mm	numeric, sample means (or majorities for factors) for predictors; predictors other than v are given these values.
#' @param xlab character, label for x-axis
#' @param ylab character, label for y-axis
#' @param min	numeric, minimum value of v; determines range of x-axis
#' @param max	numeric, maximum value of v; determines range of x-axis
#' @param levels vector, if v is a factor, determines levels to be plotted
#' @param plot logical, if \code{TRUE} render the plot (or barplot) if \code{FALSE}
#'   then compute the response and return a data.frame
#' @param N numeric, the number of intervals over which to sample the response
#' @param ...	other argument passed to plot or barplot
#' @return if \code{plot} is \code{FALSE} a data frame 
#'   that contains variable and response columns otherwise \code{NULL} invisibly
response.plot <-
function(mod, v, type, 
         mm=mod$samplemeans, 
         min=mod$varmin[v], 
         max=mod$varmax[v], 
         levels=unlist(mod$levels[v]), 
         plot=T, 
         xlab=v, 
         ylab=tools::toTitleCase(type), 
         N = 100,
         ...) {
   nr <- if (is.null(levels)) N else length(levels)
   m <- data.frame(matrix(mm,nr,length(mm),byrow=T))
   colnames(m) <- names(mm)
   m[,v] <- if (!is.null(levels)) levels else 
      seq(min - 0.1*(max-min), max+0.1*(max-min), length=N)
   preds <- predict(mod, m, type=type)
   if (plot) {
      if (is.null(levels)) {
         plot(m[,v], preds, xlab=xlab, ylab=ylab, type="l", ...) 
      } else {
         graphics::barplot(as.vector(preds), names.arg=levels, xlab=xlab, ylab=ylab, ...)
      }
   }
   else {
     return(setNames(data.frame(m[, v], preds), c(v, 'pred')))
   }
   invisible(NULL)
}
