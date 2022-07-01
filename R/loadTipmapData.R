#' Load pre-specified data sets for examples
#'
#' @description
#' Loads one of three pre-specified data sets ready to be used in functions
#'
#' @param file The dataset to be loaded
#'
#' @return A pre-saved dataset
#'
#' @examples
#'
#' loadTipmapData(file = "tipdat.rds")
#'
#' loadTipmapData(file = "tipmapPrior.rds")
#'
#' loadTipmapData(file = "tipPost.rds")
#'
loadTipmapData <- function(file) {
  path <- system.file("extdata", file, package = "tipmap")
  data <- readRDS(path)
  return(data)
}
