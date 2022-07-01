test_that("return new trial data", {
  expect_equal(
    unname(
      createNewTrialData(
        nTotal = 30, treatmentEffectEstimate = 1.27,
        standardError = 0.95
      )
    ),
    c(30, 1.27, 0.95, qnorm(p = defaultQuantiles, mean = 1.27, sd = 0.95))
  )
})
