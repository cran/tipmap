test_that("return prior data", {
  prior <- createPriorData(
    nTotal = c(200, 250, 190),
    treatmentEffectEstimate = c(1.6, 1.55, 1.4),
    standardError = c(0.7, 0.5, 0.9)
  )
  expect_equal(prior, data.frame(
    studyLabel = c("Study 1", "Study 2", "Study 3"),
    nTotal = c(200, 250, 190),
    treatmentEffectEstimate = c(1.6, 1.55, 1.4),
    standardError = c(0.7, 0.5, 0.9)
  ))
})
