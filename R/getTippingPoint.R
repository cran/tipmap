#' Identify tipping point for a specific quantile.
#'
#' @description
#' Given a tipping point data frame, identifies the weight closest to tipping point for one or multiple specified quantiles.
#'
#' @param tippingPointData A data frame created by \code{createTippingPointData()}.
#' @param quantile The quantile or quantiles of the tipping point. Possible values are 0.025, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95 and 0.975.
#' @param nullTreatmentEffect The null treatment effect. Defaults to 0.
#'
#' @return The weight closest to the tipping point for the specified quantile
#' @export
#' @seealso \code{\link{createTippingPointData}}
#' @examples
#' tipdat <- loadTipmapData("tipdat.rds")
#'
#' getTippingPoint(tipdat, quantile = 0.025)
#' getTippingPoint(tipdat, quantile = c(0.025, 0.05, 0.1, 0.2), nullTreatmentEffect = 0.1)
getTippingPoint <- function(tippingPointData, quantile, nullTreatmentEffect = 0) {
  if (!(is.numeric(quantile))) stop("quantile must be numeric and in c(0.025, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.975)")
  if (!(all(quantile %in% defaultQuantiles[-7]))) stop("quantile must be in c(0.025, 0.05, 0.1, 0.2, 0.8, 0.9, 0.95, 0.975)")
  if (!(is.numeric(nullTreatmentEffect))) stop("nullTreatmentEffect must be numeric")
  if (!(is.data.frame(tippingPointData))) stop("tippingPointData must be a data frame. See createTippingPointData()")

  column <- character(length = 1)
  tp <- numeric(length = length(quantile))

  for (i in 1:length(tp)) {
    column <- paste0("t.", as.character(quantile[i]))
    tp[i] <- as.numeric(
      (tippingPointData[which(abs(tippingPointData[column] - nullTreatmentEffect) == min(abs(tippingPointData[column] - nullTreatmentEffect),
        na.rm = TRUE
      )), ]["x.at"])
    )
  }

  for (i in 1:length(tp)) {
    if (tp[i] == 0) {
      warning(paste0("Weight 0 identified for tipping point of quantile ", quantile[i]))
    } else if (tp[i] == 1) {
      warning(paste0("Weight 1 identified for tipping point of quantile ", quantile[i]))
    }
  }
  names(tp) <- paste0("q", quantile)
  return(tp)
}
