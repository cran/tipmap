#' Average over posteriors created using weights randomly drawn from a distribution of weights
#'
#' @description
#' Runs Monte Carlo simulations with different weights on the informative component of the MAP prior and averages over them.
#' Weights may be drawn randomly from the pooled distribution of expert-weightings obtained in an expert elicitation exercise.
#'
#' @param map_prior The MAP prior to be robustified, created using \code{RBesT::automixfit()}.
#' @param new_trial_dat A vector summarising the new trial data. See \code{createNewTrialData()}.
#' @param weights A vector containing the weights to be assigned to the informative component.
#' @param null_effect The null treatment effect. Defaults to 0.
#' @param sigma Unit information standard deviation used by \code{RBesT::robustify()}.
#' @param mc_error_margin Monte Carlo error margin. Defaults to 0.05.
#'
#' @return List object containing averaged estimates of posterior quantiles (obtained with different weights) and data on Monte Carlo error.
#'
#' @seealso \code{\link{create_new_trial_data}}, \code{\link{draw_beta_mixture_nsamples}}.
#'
#' @examples
#'
#' new_trial_dat <- create_new_trial_data(
#'    n_total = 30,
#'    est = 1.27,
#'    se = 0.95
#'    )
#' expert_weights <- rbind(
#'   c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
#'   c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
#'   c(0, 0, 0, 1, 2, 3, 2, 2, 0, 0)
#'   )
#' set.seed(123)
#' get_stochast_weight_posterior(
#'   map_prior = load_tipmap_data("tipmapPrior.rds"),
#'   new_trial_dat = new_trial_dat,
#'   weights = draw_beta_mixture_nsamples(expert_weights, n = 100),
#'   sigma = 12
#' )
#'
get_stochast_weight_posterior <-
  function(map_prior,
           new_trial_dat,
           weights,
           null_effect = 0,
           sigma,
           mc_error_margin = 0.05) {

    # Checks
    if (!(is.numeric(weights)))
      stop("weights must be a numeric vector")
    if ((!(is.numeric(sigma))) ||
        length(sigma) > 1)
      stop("sigma must be a numeric of length 1")
    if ((!is.numeric(mc_error_margin)) ||
        length(mc_error_margin) > 1)
      stop("mc_error_margin must be a numeric of length 1")
    if (mc_error_margin <= 0 ||
        mc_error_margin >= 1)
      stop("mc_error_margin must be between 0 and 1")

    # Obtain posterior
    array <-
      array(dim = c(length(weights), length(default_quantiles)))
    dimnames(array) <-
      list(paste0("w", weights), paste0("q", default_quantiles))
    for (i in 1:length(weights)) {
      robust_mix_prior <-
        RBesT::robustify(
          priormix = map_prior,
          weight = (1 - weights[i]),
          n = 1,
          mean = null_effect,
          sigma = sigma
        )
      posterior <-
        RBesT::postmix(priormix = robust_mix_prior,
                       m = new_trial_dat["mean"],
                       se = new_trial_dat["se"])
      array[i, ] <- RBesT::qmix(posterior, default_quantiles)
    }
    posterior_dat <- data.frame(array)
    means <- as.numeric(colMeans(posterior_dat))
    names(means) <- paste0("q", default_quantiles)

    # Monte Carlo error
    mc_error <- as.numeric(dplyr::summarise(
      posterior_dat,
      dplyr::across(1:13, stats::sd)) / sqrt(length(weights)))
    names(mc_error) <- paste0("q", default_quantiles)
    mc_error_margin_achieved <-
      as.logical(mc_error < (
        dplyr::summarise(
          posterior_dat, dplyr::across(1:13, stats::sd)) * mc_error_margin
      ))
    names(mc_error_margin_achieved) <- paste0("q", default_quantiles)
    posterior_dat <- list(
      posterior_means = means,
      mc_error = mc_error,
      mc_error_margin_achieved = mc_error_margin_achieved
      )

    # Return result
    return(posterior_dat)
  }
