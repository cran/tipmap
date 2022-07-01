#' Data on new trial in target population
#'
#' Creates a vector containing data on the new trial in the target population. This may be hypothetical data in the planning stage.
#'
#' @param nTotal The total sample size.
#' @param treatmentEffectEstimate Treatment effect estimate.
#' @param standardError Standard error of the treatment effect estimate.
#'
#' @return A numeric vector with data on the new trial, incl. quantiles of an assumed normal data likelihood.
#' @export
#' @seealso \code{\link{createPosteriorData}}, \code{\link{createTippingPointData}}
#' @examples
#' newTrialData <- createNewTrialData(
#'   nTotal = 30, treatmentEffectEstimate = 1.27,
#'   standardError = 0.95
#' )
createNewTrialData <- function(nTotal, treatmentEffectEstimate, standardError) {
  if (!(nTotal == round(nTotal))) stop("nTotal must be a whole number")
  if (!(is.numeric(treatmentEffectEstimate))) stop("treatmentEffectEstimate must be numeric")
  if (!(is.numeric(standardError))) stop("standardError must be numeric")
  if ((standardError <= 0)) stop("standardError must be positive")
  if ((length(nTotal) != 1) || (length(treatmentEffectEstimate) != 1) || (length(standardError) != 1)) {
    stop("Argments must be of length 1")
  }

  newTrialData <- c(
    nTotal, treatmentEffectEstimate, standardError,
    stats::qnorm(p = defaultQuantiles, mean = treatmentEffectEstimate, sd = standardError)
  )
  names(newTrialData) <- c("nTotal", "Mean", "SE", paste0("q", defaultQuantiles))
  return(newTrialData)
}
