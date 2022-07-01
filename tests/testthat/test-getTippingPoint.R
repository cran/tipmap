test_that("return tipping point", {
  expect_equal(
    unname(getTippingPoint(
      tippingPointData = loadTipmapData("tipdat.rds"),
      quantile = c(0.025, 0.05, 0.1, 0.2)
    )),
    c(0.750, 0.475, 0.220, 0.040)
  )
})
