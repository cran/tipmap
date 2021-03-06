% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTippingPoint.R
\name{getTippingPoint}
\alias{getTippingPoint}
\title{Identify tipping point for a specific quantile.}
\usage{
getTippingPoint(tippingPointData, quantile, nullTreatmentEffect = 0)
}
\arguments{
\item{tippingPointData}{A data frame created by \code{createTippingPointData()}.}

\item{quantile}{The quantile or quantiles of the tipping point. Possible values are 0.025, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95 and 0.975.}

\item{nullTreatmentEffect}{The null treatment effect. Defaults to 0.}
}
\value{
The weight closest to the tipping point for the specified quantile
}
\description{
Given a tipping point data frame, identifies the weight closest to tipping point for one or multiple specified quantiles.
}
\examples{
tipdat <- loadTipmapData("tipdat.rds")

getTippingPoint(tipdat, quantile = 0.025)
getTippingPoint(tipdat, quantile = c(0.025, 0.05, 0.1, 0.2), nullTreatmentEffect = 0.1)
}
\seealso{
\code{\link{createTippingPointData}}
}
