#' Creates posterior distributions for a range of weights on the informative component of the robust MAP prior
#'
#' @description
#' Returns a data frame containing the default quantiles of posterior mixture distributions
#' generated with varying weights on the informative component of the MAP prior.
#'
#' @param mapPrior A MAP prior containing information about the trial(s) in the source population, created using \code{RBesT}.
#' @param newTrialData A vector containing information about the new trial. See \code{createNewTrialData()}.
#' @param sigma Standard deviation to be used for the weakly informative component of the MAP prior, recommended to be the unit-information standard deviation.
#' @param nullTreatmentEffect The mean of the robust component of the MAP prior. Defaults to 0.
#'
#' @return A data frame containing posterior distributions for varying weights
#' @export
#' @seealso \code{\link{createNewTrialData}}, \code{\link{createPriorData}}, \code{\link[RBesT:automixfit]{RBesT::automixfit}}
#' @examples
#'
#' # create vector for new observations
#' newTrialData <- createNewTrialData(
#'   nTotal = 30, treatmentEffectEstimate = 1.27,
#'   standardError = 0.95
#' )
#'
#' # read MAP prior created by RBesT
#' mapPrior <- loadTipmapData("tipmapPrior.rds")
#'
#' # create posterior data
#' posteriorData <- createPosteriorData(
#'   mapPrior = mapPrior,
#'   newTrialData = newTrialData, sigma = 12
#' )
#' @references Best, N., Price, R. G., Pouliquen, I. J., & Keene, O. N. (2021).
#' Assessing efficacy in important subgroups in confirmatory trials: An example
#' using Bayesian dynamic borrowing. Pharm Stat, 20(3), 551–562.
#' https://doi.org/10.1002/pst.2093
#'
createPosteriorData <- function(mapPrior, newTrialData, sigma, nullTreatmentEffect = 0) {
  if (!(is.numeric(sigma))) stop("sigma must be numeric")
  if ((sigma <= 0)) stop("sigma must be positive")

  arr <- array(dim = c(length(defaultWeights), length(defaultQuantiles)))
  dimnames(arr) <- list(paste0("w=", defaultWeights), paste0("q", defaultQuantiles))
  for (i in 1:length(defaultWeights)) {
    robust.mix.prior <- RBesT::robustify(mapPrior, weight = (1 - defaultWeights[i]), m = 0, n = 1, sigma = sigma, mean = nullTreatmentEffect)
    posterior <- RBesT::postmix(robust.mix.prior, m = newTrialData["Mean"], se = newTrialData["SE"])
    arr[i, ] <- RBesT::qmix(posterior, defaultQuantiles)
  }
  posteriorData <- data.frame(cbind(weight = defaultWeights, arr))
  return(posteriorData)
}
