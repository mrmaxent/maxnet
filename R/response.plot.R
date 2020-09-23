#' @export
response.plot <-
function(mod, v, type, mm=mod$samplemeans, min=mod$varmin[v], max=mod$varmax[v], levels=unlist(mod$levels[v]), plot=T, xlab=v, ylab=tools::toTitleCase(type), ...) {
   nr <- if (is.null(levels)) 100 else length(levels)
   m <- data.frame(matrix(mm,nr,length(mm),byrow=T))
   colnames(m) <- names(mm)
   m[,v] <- if (!is.null(levels)) levels else 
      seq(min - 0.1*(max-min), max+0.1*(max-min), length=100)
   preds <- predict(mod, m, type=type)
   if (plot) {
      if (is.null(levels)) {
         plot(m[,v], preds, xlab=xlab, ylab=ylab, type="l", ...) 
      } else {
         graphics::barplot(as.vector(preds), names.arg=levels, xlab=xlab, ylab=ylab, ...)
      }
   }
   else return(setNames(data.frame(m[, v], preds), c(v, 'pred')))
}
