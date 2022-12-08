#' @name maxnet-package
#' @aliases maxnet-package
#' @docType package
#' @title Maxent over glmnet
#' @description Procedures to fit species distributions models from occurrence records and environmental variables, using 'glmnet' for model fitting. Model structure is the same as for the 'Maxent' Java package, version 3.4.0, with the same feature types and regularization options.  See the 'Maxent' website <http://biodiversityinformatics.amnh.org/open_source/maxent> for more details.
#' @references Phillips & Dudik, Fithian & Hastie, glmnet
#' @author Steve Phillips
#' @importFrom stats approx formula model.matrix predict sd setNames
#' @keywords internals
"_PACKAGE"
