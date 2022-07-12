#' Create data frame ready to use for tipping point analysis
#'
#' Combines new trial data created by \code{createTargetData()}, a posterior distribution created by \code{createPosteriorData()} and a
#' robust MAP prior using \code{RBesT::automixfit()} and an optional meta-analysis created using the \code{meta} package into a data frame
#' needed for the functions \code{tippingPointPlot()} and \code{getTippingPoint()}.
#'
#' @param newTrialData A data frame containing information about the target population. See \code{createNewTrialData()}.
#' @param posterior A mixture combining MAP prior and target population. See \code{createPosteriorData()}.
#' @param mapPrior A robust MAP prior created by \code{RBesT::automixfit()}.
#' @param metaAnalysis A data frame containing a meta-analysis of trial(s) to be borrowed from. See \code{createPriorData()}.
#'
#' @return A data frame ready to be used for \code{tippingPointPlot()} and \code{getTippingPoint()}
#' @export
#' @seealso \code{\link{createNewTrialData}}, \code{\link{createPosteriorData}}, \code{\link[RBesT:automixfit]{RBesT::automixfit}},
#' \code{\link{tippingPointPlot}}, \code{\link{getTippingPoint}}
#' @examples
#'
#' # specify new trial data
#' newTrialData <- createNewTrialData(nTotal = 30, treatmentEffectEstimate = 1.5, standardError = 2.1)
#'
#' # read MAP prior
#' mapPrior <- loadTipmapData("tipmapPrior.rds")
#'
#' # read posterior
#' posterior <- loadTipmapData("tipPost.rds")
#'
#' tipdat <- createTippingPointData(
#'   newTrialData = newTrialData, posterior = posterior,
#'   mapPrior = mapPrior
#' )
#'
createTippingPointData <- function(newTrialData, posterior, mapPrior, metaAnalysis = NULL) {
  if (!(is.data.frame(posterior))) stop("posterior must be a data frame. Use createPosteriorData()")

  mapPrior <- summary(mapPrior, probs = defaultQuantiles)
  names(mapPrior) <- c("Mean", "SE", paste0("q", defaultQuantiles))

  if (!(missing(metaAnalysis))) {
    plotData <- data.frame(
      # Graphical parameters
      x.at = c(-0.15, defaultWeights, 1.15, 1.35),
      # defines if data points are from target population, mix or prior
      x.col = factor(c("new.obs", rep("post", length(defaultWeights)), rep("prior", 2))),
      # treatment effect estimates for target population, posterior and prior
      t.est = c(
        newTrialData["Mean"], unlist(posterior["q0.5"]),
        mapPrior["Mean"], metaAnalysis$TE.fixed
      ),
      t.0.025 = c(
        newTrialData["q0.025"], unlist(posterior["q0.025"]),
        mapPrior["q0.025"], metaAnalysis$lower.fixed
      ),
      t.0.05 = c(
        NA, unlist(posterior["q0.05"]),
        mapPrior["q0.05"], NA
      ),
      t.0.1 = c(
        NA, unlist(posterior["q0.1"]),
        mapPrior["q0.1"], NA
      ),
      t.0.2 = c(
        NA, unlist(posterior["q0.2"]),
        mapPrior["q0.2"], NA
      ),
      t.0.8 = c(
        NA, unlist(posterior["q0.8"]),
        mapPrior["q0.8"], NA
      ),
      t.0.9 = c(
        NA, unlist(posterior["q0.9"]),
        mapPrior["q0.9"], NA
      ),
      t.0.95 = c(
        NA, unlist(posterior["q0.95"]),
        mapPrior["q0.95"], NA
      ),
      t.0.975 = c(
        newTrialData["q0.975"], unlist(posterior["q0.975"]),
        mapPrior["q0.975"], metaAnalysis$upper.fixed
      )
    )
  } else {
    plotData <- data.frame(
      # Graphical parameters
      x.at = c(-0.15, defaultWeights, 1.15),
      # defines if data points are from target population, mix or prior
      x.col = factor(c("new.obs", rep("post", length(defaultWeights)), "prior")),
      # treatment effect estimates for target population, posterior and prior
      t.est = c(
        newTrialData["Mean"], unlist(posterior["q0.5"]),
        mapPrior["Mean"]
      ),
      t.0.025 = c(
        newTrialData["q0.025"], unlist(posterior["q0.025"]),
        mapPrior["q0.025"]
      ),
      t.0.05 = c(
        newTrialData["q0.05"], unlist(posterior["q0.05"]),
        mapPrior["q0.05"]
      ),
      t.0.1 = c(
        newTrialData["q0.1"], unlist(posterior["q0.1"]),
        mapPrior["q0.1"]
      ),
      t.0.2 = c(
        newTrialData["q0.2"], unlist(posterior["q0.2"]),
        mapPrior["q0.2"]
      ),
      t.0.8 = c(
        newTrialData["q0.8"], unlist(posterior["q0.8"]),
        mapPrior["q0.8"]
      ),
      t.0.9 = c(
        newTrialData["q0.9"], unlist(posterior["q0.9"]),
        mapPrior["q0.9"]
      ),
      t.0.95 = c(
        newTrialData["q0.95"], unlist(posterior["q0.95"]),
        mapPrior["q0.95"]
      ),
      t.0.975 = c(
        newTrialData["q0.975"], unlist(posterior["q0.975"]),
        mapPrior["q0.975"]
      )
    )
  }
  rownames(plotData) <- as.character(seq(1, length(plotData$t.est)))
  return(plotData)
}
