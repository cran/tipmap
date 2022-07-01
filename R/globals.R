#' default quantiles
defaultQuantiles <- c(0.01, 0.025, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.975, 0.99)
#' default weights
defaultWeights <- seq(0, 1, by = 0.005)

# Colors
#' custom dark blue
tipmapDarkBlue <- grDevices::rgb(0, 102, 153, max = 255)
#' custom light red
tipmapLightRed <- grDevices::rgb(204, 51, 51, max = 255)

utils::globalVariables(c("defaultQuantiles", "defaultWeights", "tipmapDarkBlue", "tipmapLightRed"))
utils::globalVariables(c("t.0.025", "t.0.05", "t.0.1", "t.0.2", "t.0.8", "t.0.9", "t.0.95", "t.0.975", "t.est", "x.at", "x.col"))
