#' Get cumulative probabilities for specified chip distribution
#'
#' @description
#' Internal function needed for expert elicitation methods.
#'
#' @param chips A numeric vector representing chip distribution for an expert.
#' Vector must be of length 10 and contents must add up to 10. First column represents weight 0-0.1.
#'
#' @return The cumulative probabilities for the expert.
#'
#' @examples
#' getExpertCumulativeProbs(c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0))
#'
getExpertCumulativeProbs <- function(chips) {
  nChips <- 10
  if (!is.numeric(chips)) chips <- as.numeric(chips)
  if (length(chips) < nChips) stop("Length of chip vector must be 10.")
  if (sum(chips) != nChips) stop("Chips must add up to 10.")

  cumulativeProbs <- cumsum(chips / length(chips))
  binNames <- c()
  for (i in 0:9) {
    binNames <- c(binNames, paste0(c(i * 0.1, (i + 1) * 0.1)))
  }
  # names(cumulativeProbs) <- binNames
  return(cumulativeProbs)
}

#' Transform cumulative probabilities to fit beta distributions.
#'
#' @description
#' Internal function needed for expert elicitation methods.
#
#' @param cumprobs Cumulative probabilities for a single expert. See \code{getExpertCumulativeProbs()}.
#' @param w Weight of bins.
#'
#' @return Data to fit beta distributions.
getModelInputData <- function(cumprobs, w) {
  dat <- dplyr::tibble(w = w, cumprobs = cumprobs)
  dat <- dplyr::filter(dat, cumprobs > 0)
  dat <- dat[match(unique(dat$cumprobs), dat$cumprobs), ]
  return(dat)
}


#' Draw a single sample from a mixture of specified beta distributions.
#'
#' @description
#' Internal function needed for expert elicitation methods.
#'
#' @param betaParameters Parameters for each beta distribution.
#' @param weights Optional vector of weights assigned to experts. Defaults to uniform.
#' @seealso \code{\link{fitExpertBetaDistributions}}
#' @return A random sample from a mixture of specified beta distributions.
rmixbeta <- function(betaParameters, weights = NULL) {
  n <- nrow(betaParameters)
  if (missing(weights)) weights <- rep(1 / n, n)
  sample_component <- sample(
    x = 1:n,
    size = 1,
    prob = weights
  )
  sample_value <- stats::rbeta(
    n = 1,
    shape1 = betaParameters[sample_component, c("shape1")],
    shape2 = betaParameters[sample_component, c("shape2")]
  )
  return(sample_value)
}

#' Draw samples from expert opinions gathered in the roulette method.
#'
#' @description Returns samples drawn from a pooled distribution on specified expert weights.
#'
#' @param expertWeights A data frame or matrix representing expert weights.
#' Rows should represent experts, columns should represent bins / weights.
#' @param n The number of samples to be drawn.
#' @param weights An optional vector containing the weight assigned to each expert. Defaults to uniform.
#'
#' @details
#' The argument \code{expertWeights} must be a data frame or a matrix that represents the assigned
#' expert weights. It should contain a row for each expert and 10 columns for different weights.
#' The first column represents weights 0-0.1, second column represents weights 0.1-0.2 and so on.
#'
#' @return Samples from a pooled distribution of expert opinions.
#' @export
#'
#' @examples
#' drawExpertWeightSample(
#'   expertWeights = rbind(
#'     c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'     c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'     c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#'   ),
#'   n = 100
#' )
drawExpertWeightSample <- function(expertWeights, n, weights = NULL) {
  samples <- numeric(length = n)

  betaParameters <- fitExpertBetaDistributions(expertWeights)

  for (i in 1:length(samples)) {
    samples[i] <- rmixbeta(betaParameters = betaParameters, weights = weights)
  }
  return(samples)
}

#' Fit beta distributions for specified expert weights
#'
#' @description
#' Fits beta distribution to the specified expert weights using \code{SHELF::fitdist}.
#' For each distribution, shape parameters are fitted and returned.
#'
#' @param expertWeights A data frame or matrix containing expert weights
#'
#' @details
#' The argument \code{expertWeights} must be a data frame or a matrix that represents the assigned
#' expert weights. It should contain a row for each expert and 10 columns for different weights.
#' The first column represents weights 0-0.1, second column represents weights 0.1-0.2 and so on.
#'
#' @return Parameters for individual beta distributions
#' @export
#'
#' @examples
#'
#' fitExpertBetaDistributions(
#'   expertWeights =
#'     rbind(
#'       c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'       c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'       c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
#'     )
#' )
fitExpertBetaDistributions <- function(expertWeights) {
  expertsList <- purrr::array_branch(array = expertWeights, margin = 1)

  cumProbs <- purrr::map(.x = expertsList, .f = ~ getModelInputData(cumprobs = getExpertCumulativeProbs(.x), w = 1:10 / 10))

  fits <- purrr::map(.x = cumProbs, .f = ~ SHELF::fitdist(vals = .x$w, probs = .x$cumprobs, lower = 0, upper = 1))

  betaParameters <- purrr::map(fits, "Beta")
  betaParameters <- dplyr::bind_rows(betaParameters)

  return(betaParameters)
}
