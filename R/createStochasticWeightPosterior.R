#' Average over multiple posteriors created using different informative component weights.
#'
#' @description
#' Runs Monte Carlo simulations with different weights on the informative component of the MAP prior and averages over them.
#' Weights may be drawn from the pooled distribution of expert-weightings obtained in an expert-elicitation exercise.
#'
#' @param mapPrior The MAP prior to be robustified, created using \code{RBesT::automixfit()}.
#' @param newTrialData A vector summarising the new trial data. See \code{createNewTrialData()}.
#' @param weights A vector containing the weights to be assigned to the informative component.
#' @param nullTreatmentEffect The null treatment effect. Defaults to 0.
#' @param sigma Sampling standard deviation for \code{RBesT::robustify()}.
#' @param mceErrorMargin Proportion of the SD that the Monte Carlo Error should be smaller than. Defaults to 0.05.
#'
#' @return Averaged quantiles of posteriors created with different weights. Monte Carlo error is also returned.
#' @export
#' @seealso \code{\link{createNewTrialData}}, \code{\link[RBesT:automixfit]{RBesT::automixfit}}, \code{\link[RBesT:robustify]{RBesT::robustify}}
#' @examples
#'
#' newTrialData <- createNewTrialData(
#'   nTotal = 30,
#'   treatmentEffectEstimate = 1.27, standardError = 0.95
#' )
#'
#' mapPrior <- loadTipmapData("tipmapPrior.rds")
#'
#' expertWeights <- rbind(
#'   c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'   c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'   c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#' )
#'
#' createStochasticWeightPosterior(
#'   mapPrior = mapPrior, newTrialData = newTrialData,
#'   weights = drawExpertWeightSample(expertWeights, n = 500), sigma = 12
#' )
#'
createStochasticWeightPosterior <- function(mapPrior, newTrialData, weights, nullTreatmentEffect = 0, sigma, mceErrorMargin = 0.05) {
  if (!(is.numeric(weights))) stop("weights must be a numeric vector")
  if ((!(is.numeric(sigma))) || length(sigma) > 1) stop("sigma must be a numeric of length one")
  if ((!is.numeric(mceErrorMargin)) || length(mceErrorMargin) > 1) stop("mceErrorMargin must be a numeric of length one")
  if (mceErrorMargin <= 0 || mceErrorMargin >= 1) stop("mceErrorMargin must be between 0 and 1")

  array <- array(dim = c(length(weights), length(defaultQuantiles)))
  dimnames(array) <- list(paste0("w", weights), paste0("q", defaultQuantiles))
  for (i in 1:length(weights)) {
    robust.mix.prior <- RBesT::robustify(mapPrior, weight = (1 - weights[i]), n = 1, mean = nullTreatmentEffect, sigma = sigma)
    posterior <- RBesT::postmix(priormix = robust.mix.prior, m = newTrialData["Mean"], se = newTrialData["SE"])
    array[i, ] <- RBesT::qmix(posterior, defaultQuantiles)
  }

  posteriorData <- data.frame(array)

  means <- as.numeric(colMeans(posteriorData))
  names(means) <- paste0("q", defaultQuantiles)

  mce <- as.numeric(dplyr::summarise(
    posteriorData,
    dplyr::across(1:13, stats::sd)
  ) / sqrt(length(weights)))

  names(mce) <- paste0("q", defaultQuantiles)

  mceErrorMarginAchieved <-
    as.logical(mce < (dplyr::summarise(posteriorData, dplyr::across(1:13, stats::sd)) * mceErrorMargin))

  names(mceErrorMarginAchieved) <- paste0("q", defaultQuantiles)

  posteriorData <- list(
    Mean = means, MCE = mce,
    MCEErrorMargin = mceErrorMarginAchieved
  )

  return(posteriorData)
}
