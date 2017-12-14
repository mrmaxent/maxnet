#' @import stats
#' @export
maxnet <-
function(p, data, f=maxnet.formula(p, data), regmult=1.0, 
         regfun=maxnet.default.regularization, ...)
{
   mm <- model.matrix(f, data)
   reg <- regfun(p,mm) * regmult
   weights <- p+(1-p)*100
   glmnet::glmnet.control(pmin=1.0e-8, fdev=0)  
   model <- glmnet::glmnet(x=mm, y=as.factor(p), family="binomial", standardize=F, penalty.factor=reg, lambda=10^(seq(4,0,length.out=200))*sum(reg)/length(reg)*sum(p)/sum(weights), weights=weights, ...)
   class(model) <- c("maxnet", class(model))
   if (length(model$beta) < 200) stop("Error: glmnet failed to complete regularization path")
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
