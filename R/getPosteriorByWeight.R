#' Filter posterior by given weights
#'
#' @description
#' Returns quantiles for the given posterior filtered by specified weights.
#'
#' @param posterior The posterior to be filtered (see \code{createPosteriorData()}).
#' @param weight The weight(s) to be filtered by.
#'
#' @return The filtered posterior values
#' @export
#' @seealso \code{\link{createPosteriorData}}
#' @examples
#' getPosteriorByWeight(
#'   posterior = loadTipmapData("tipPost.rds"),
#'   weight = c(0.05, 0.1)
#' )
#'
getPosteriorByWeight <- function(posterior, weight) {
  if (!(is.numeric(weight))) stop("Weight must be numeric")
  weights <- weight
  posteriorFiltered <- dplyr::filter(posterior, weight %in% weights)
  posteriorFiltered <- dplyr::select(posteriorFiltered, -weight)
  return(posteriorFiltered)
}
