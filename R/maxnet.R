#' Maxent over glmnet
#'
#' @description Maxent species distribution modeling using glmnet for model
#'   fitting
#'
#'   Using \code{lp} for the linear predictor and \code{entropy} for the entropy
#'   of the exponential model over the background data, the values plotted on
#'   the y-axis are:
#'
#'   \itemize{ \item{\code{lp} if \code{type} is "link"}
#'
#'   \item{\code{exp(lp)} if \code{type} is "exponential"}
#'
#'   \item{\code{1-exp(-exp(entropy+lp))} if \code{type} is "cloglog"}
#'
#'   \item{\code{1/(1+exp(-entropy-lp))} if \code{type} is "logistic"} }
#'
#' @export
#' @param p numeric, a vector of 1 (for presence) or 0 (for background)
#' @param data a matrix or data frame of predictor variables
#' @param f formula, determines the features to be used
#' @param regmult numeric, a constant to adjust regularization
#' @param regfun function, computes regularization constant for each feature
#' @param addsamplestobackground logical, if TRUE then add to the background any
#'   presence sample that is not already there
#' @param m a matrix of feature values
#' @param classes charcater, continuous feature classes desired, either
#'   "default" or any subset of "lqpht" (for example, "lh")
#' @param ... not used
#'
#' @return Maxnet returns an object of class \code{maxnet}, which is a list
#'   consisting of a glmnet model with the following elements added:
#'\describe{
#'  \item{betas}{ nonzero coefficients of the fitted model }
#'  \item{alpha}{ constant offset making the exponential model sum to one over the background data }
#'  \item{entropy}{ entropy of the exponential model }
#'  \item{penalty.factor}{ the regularization constants used for each feature }
#'  \item{featuremins}{ minimum of each feature, to be used for clamping }
#'  \item{featuremaxs}{ maximum of each feature, to be used for clamping }
#'  \item{varmin}{ minimum of each predictor, to be used for clamping }
#'  \item{varmax}{ maximum of each predictor, to be used for clamping }
#'  \item{samplemeans}{ mean of each predictor over samples (majority for factors) }
#'  \item{levels}{ levels of each predictor that is a factor }
#'}
#' @author Steve Phillips
#' @examples
#' \dontrun{
#'   library(maxnet)
#'   data(bradypus)
#'   p <- bradypus$presence
#'   data <- bradypus[,-1]
#'   mod <- maxnet(p, data)
#'   plot(mod, type="cloglog")
#'   mod <- maxnet(p, data, maxnet.formula(p, data, classes="lq"))
#'   plot(mod, "tmp6190_ann")
#' }
maxnet <-
function(p, data, f=maxnet.formula(p, data), regmult=1.0, 
         regfun=maxnet.default.regularization, addsamplestobackground=T, ...)
{
   if (anyNA(data)) stop("NA values in data table. Please remove them and rerun.")
   if (!is.vector(p))
       stop("p must be a vector.")
   if (addsamplestobackground) {
       pdata <- data[p==1, , drop = FALSE]
       ndata <- data[p==0, , drop = FALSE]
       # add to the background any presence data that isn't already in the background
       toadd <- dplyr::setdiff(pdata, ndata)
       p <- c(p, rep(0, nrow(toadd)))
       data <- rbind(data, toadd)
   }   
   mm <- model.matrix(f, data)
   reg <- regfun(p,mm) * regmult
   weights <- p+(1-p)*100
   glmnet::glmnet.control(pmin=1.0e-8, fdev=0)  
   model <- glmnet::glmnet(x=mm, y=as.factor(p), family="binomial", standardize=F, penalty.factor=reg, lambda=10^(seq(4,0,length.out=200))*sum(reg)/length(reg)*sum(p)/sum(weights), weights=weights, ...)
   class(model) <- c("maxnet", class(model))
   if (length(model$lambda) < 200) {
        msg <- "Error: glmnet failed to complete regularization path.  Model may be infeasible."
        if (!addsamplestobackground) 
           msg <- paste(msg, " Try re-running with addsamplestobackground=T.")
        stop(msg)
   }
   bb <- model$beta[,200]
   model$betas <- bb[bb!=0]
   model$alpha <- 0
   rr <- predict.maxnet(model, data[p==0, , drop = FALSE], type="exponent", clamp=F)
   raw <- rr / sum(rr)
   model$entropy <- -sum(raw * log(raw))
   model$alpha <- -log(sum(rr))
   model$penalty.factor <- reg
   model$featuremins <- apply(mm, 2, min)
   model$featuremaxs <- apply(mm, 2, max)
   vv <- (sapply(data, class)!="factor")
   model$varmin <- apply(data[,vv, drop = FALSE], 2, min)
   model$varmax <- apply(data[,vv, drop = FALSE], 2, max)
   means <- apply(data[p==1,vv, drop = FALSE], 2, mean)
   majorities <- sapply(names(data)[!vv], 
      function(n) which.max(table(data[p==1,n, drop = FALSE])))
   names(majorities) <- names(data)[!vv]
   model$samplemeans <- unlist(c(means, majorities))
   model$levels <- lapply(data, levels)
   model
}
