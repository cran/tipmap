#' Creates input data frame for construction of MAP prior
#'
#' Assembling information from trials in the source population in a structured way (required as a pre-processing step for MAP prior creation).
#'
#' @param studyLabel An optional vector containing trial labels.
#' @param nTotal A vector containing total sample sizes.
#' @param treatmentEffectEstimate A vector containing treatment effect estimates.
#' @param standardError A vector containing standard errors of the effect estimates.
#'
#' @return A data frame containing data on the trials in the source population.
#' @export
#' @seealso \code{\link[RBesT:gMAP]{RBesT::gMAP}}, \code{\link[RBesT:automixfit]{RBesT::automixfit}}
#' @examples
#' priorData <- createPriorData(
#'   nTotal = c(160, 240, 320),
#'   treatmentEffectEstimate = c(1.23, 1.40, 1.51),
#'   standardError = c(0.4, 0.36, 0.31)
#' )
createPriorData <- function(studyLabel = NULL, nTotal, treatmentEffectEstimate, standardError) {
  if ((any(nTotal != round(nTotal)))) stop("nTotal must be a whole number")
  if ((any(nTotal <= 0))) stop("nTotal must be positive")
  if (!(is.numeric(treatmentEffectEstimate))) stop("treatmentEffectEstimate must be numeric")
  if (!(is.numeric(standardError))) stop("standardError must be numeric")
  if ((any(standardError <= 0))) stop("standardError must be positive")

  if (missing(studyLabel)) {
    studyLabel <- paste("Study", seq(from = 1, to = length(treatmentEffectEstimate), by = 1))
  }

  priorData <- data.frame(studyLabel, nTotal, treatmentEffectEstimate, standardError)
  return(priorData)
}
