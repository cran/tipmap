#' Summarize expert weights
#'
#' @description
#' Compute min, max, mean and quartiles for expert weights
#'
#' @param expertWeights Expert weights gathered using the roulette method.
#' @param n The number of samples to be drawn when summarizing. Default: 1000.
#' @param weights Weights assigned to each expert. Defaults to uniform.
#'
#' @return A summary of the expert weights
#' @export
#' @seealso \code{\link{drawExpertWeightSample}}
#'
#' @examples
#'
#' getExpertWeightSummary(
#'   rbind(
#'     c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0), c(0, 0, 0, 2, 3, 3, 2, 0, 0, 0),
#'     c(0, 1, 2, 4, 2, 1, 0, 0, 0, 0), c(2, 3, 3, 2, 0, 0, 0, 0, 0, 0),
#'     c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0), c(2, 3, 3, 2, 0, 0, 0, 0, 0, 0)
#'   )
#' )
#'
getExpertWeightSummary <- function(expertWeights, n = 1000, weights = NULL) {
  if (missing(weights)) weights <- rep(1 / nrow(expertWeights), nrow(expertWeights))
  samples <- tipmap::drawExpertWeightSample(expertWeights, n = n, weights = weights)
  summary <- base::summary(samples)
  return(summary)
}
