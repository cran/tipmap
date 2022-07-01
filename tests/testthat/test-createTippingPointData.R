test_that("return tipping point data", {
  tipdat <- createTippingPointData(
    newTrialData = createNewTrialData(30, 1.4, 2.0),
    posterior = loadTipmapData("tipPost.rds"),
    mapPrior = loadTipmapData("tipmapPrior.rds")
  )
  expect_equal(tipdat, loadTipmapData("tipdat.rds"))
})
