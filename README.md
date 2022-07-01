
# tipmap

The `tipmap` package aims to facilitate the planning and analysis of Bayesian extrapolation studies by incorporating a tipping point approach for the selection of informative weight for robust meta-analytic predictive priors.


## Installation

You can install the development version of `tipmap` like so:

``` r
install.packages("tipmap")
```

## Example

Suppose that data from three clinical trials in adults is to be combined with data from a small pediatric trial using a robust meta-analytic predictive prior.

``` r
library(tipmap)

pediatricTrial <- createNewTrialData(
  nTotal = 30, treatmentEffectEstimate = 1.27,
  standardError = 0.95
)

uisd <- sqrt(pediatricTrial$nTotal) * pediatricTrial$standardError

priorData <- createPriorData(
  nTotal = c(160, 240, 320),
  treatmentEffectEstimate = c(1.23, 1.40, 1.51),
  standardError = c(0.4, 0.36, 0.31)
)

gMap <- RBesT::gMAP(formula=cbind(treatmentEffectEstimate, standardError) ~ 1 | studyLabel,
                        data = priorData, family = gaussian, weights = nTotal,
                        tau.dist = "HalfNormal", tau.prior = cbind(0, uisd/16),
                        beta.prior = cbind(0, uisd))
mapPrior <- RBesT::automixfit(sample = map_mcmc, Nc = seq(1,4), k = 6, thresh = -Inf)

posterior <- createPosteriorData(mapPrior = mapPrior,
  newTrialData = pediatricTrial, sigma = uisd)

tippingPointData <- createTippingPointData(newTrialData = newTrialData,
  posterior = posterior, mapPrior = mapPrior)

tippingPointPlot(tippingPointData = tippingPointData)
getTippingPoint(tippingPointData = tippingPointData, quantile = c(0.05, 0.025))

```
