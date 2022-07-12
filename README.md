# tipmap

The `tipmap` package aims to facilitate the planning and analysis of partial extrapolation studies in pediatric drug development. It provides an implementation of a Bayesian tipping point approach based on robust meta-analytic predictive (MAP) priors, with further functions facilitating expert elicitation of a primary weight of the informative component of the prior.

## Installation

To install `tipmap` use:

``` r
install.packages("tipmap")
```

## Example

Suppose that data from three clinical trials in adults is to be combined with data from a small pediatric trial using a robust MAP prior approach.

``` r
library(tipmap)

pediatricTrial <- createNewTrialData(
  nTotal = 30,
  treatmentEffectEstimate = 1.27,
  standardError = 0.95
)

uisd <- sqrt(pediatricTrial["nTotal"]) * pediatricTrial["SE"]

priorData <- createPriorData(
  nTotal = c(160, 240, 320),
  treatmentEffectEstimate = c(1.23, 1.40, 1.51),
  standardError = c(0.4, 0.36, 0.31)
)

gMap <-
  RBesT::gMAP(
    formula = cbind(treatmentEffectEstimate, standardError) ~ 1 |
      studyLabel,
    data = priorData,
    family = gaussian,
    weights = nTotal,
    tau.dist = "HalfNormal",
    tau.prior = cbind(0, uisd / 16),
    beta.prior = cbind(0, uisd)
  )
                        
mapPrior <-
  RBesT::automixfit(
    sample = gMap,
    Nc = seq(1, 4),
    k = 6,
    thresh = -Inf
  )

posterior <- createPosteriorData(mapPrior = mapPrior,
                                 newTrialData = pediatricTrial,
                                 sigma = uisd)

tippingPointData <-
  createTippingPointData(newTrialData = pediatricTrial,
                         posterior = posterior,
                         mapPrior = mapPrior)

tippingPointPlot(tippingPointData = tippingPointData)

getTippingPoint(tippingPointData = tippingPointData,
                quantile = c(0.05, 0.025))
```
