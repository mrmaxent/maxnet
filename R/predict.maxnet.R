#' Predict using a maxnet model
#' 
#' Prediction can be on a spatial raster or vector space.
#' 
#' @export
#' @param object an object of class "maxnet", i.e., a fitted model.
#' @param newdata values of predictor variables to predict to, possibly
#'   matrix, data.frame, \code{SpatRaster} or \code{stars} object. 
#' @param clamp logical, f true, predictors and features are restricted to the range seen during model training.
#' @param type character, type of response required. Using \code{lp} for the linear predictor 
#' and \code{entropy} for the entropy of the exponential model over the background data, 
#' the values returned are determined by the value of \code{type}.
#' \itemize{
#'   \item{"link"}{yields \code{lp}}
#'   \item{"exponential"}{yields \code{exp(lp)}}
#'   \item{"cloglog"}{yields \code{1-exp(-exp(entropy+lp))}}
#'   \item{"logistic"}{yields \code{1/(1+exp(-entropy-lp))}}
#' }
#' @param ... not used
#' @return vector with predicted values (one per input row), \code{SpatRaster} or \code{stars} object of predicted values
predict.maxnet <-
  function(object, newdata, clamp=T, type=c("link","exponential","cloglog","logistic"), ...)
  {
    
    na_action <- options("na.action")[[1]]
    on.exit(options(na.action = na_action))
    options(na.action = "na.pass")
    
    newdataframe <- newdata
    if (is_stars <- inherits(newdata, "stars")) {
      if (!requireNamespace("stars", quietly = TRUE)) {
        stop("package stars required, please install it first")
      }
      newdataframe <- as.data.frame(newdata)[, -c(1,2)]
    }
    if (is_spatraster <- inherits(newdata, "SpatRaster")) {
      if (!requireNamespace("terra", quietly = TRUE)) {
        stop("package terra required, please install it first")
      }
      newdataframe <- as.data.frame(newdata, na.rm = FALSE)
    }
    
    
    if (clamp) {
      for (v in intersect(names(object$varmax), names(newdataframe))) {
        newdataframe[,v] <- pmin(pmax(newdataframe[,v], object$varmin[v]), object$varmax[v])
      }
    }
    terms <- sub("hinge\\((.*)\\):(.*):(.*)$", "hingeval(\\1,\\2,\\3)", names(object$betas))
    terms <- sub("categorical\\((.*)\\):(.*)$", "categoricalval(\\1,\"\\2\")", terms)
    terms <- sub("thresholds\\((.*)\\):(.*)$", "thresholdval(\\1,\\2)", terms)
    f <- formula(paste("~", paste(terms, collapse=" + "), "-1"))
    mm <- model.matrix(f, data.frame(newdataframe))
    if (clamp) mm <- t(pmin(pmax(t(mm), object$featuremins[names(object$betas)]), 
                            object$featuremaxs[names(object$betas)]))
    link <- (mm %*% object$betas) + object$alpha
    type <- match.arg(type)
    r <- switch(tolower(type[1]),
                "exponential" = exp(link),
                "cloglog" = 1-exp(0-exp(object$entropy+link)),
                "logistic"= 1/(1+exp(-object$entropy-link)),
                link)
    
    if (is_stars){
      S <- newdata[1]
      names(S) <- "pred"
      S$pred <- r
      return(S)
    } 
    if (is_spatraster){
      S <- newdata[[1]]
      terra::values(S) <- r
      names(S) <- "pred"
      return(S)
   }

return(r)
}