test_that("return posterior data", {
  newTrial <- createNewTrialData(30, 1.4, 2.0)
  post <- createPosteriorData(
    mapPrior = loadTipmapData("tipmapPrior.rds"),
    newTrialData = newTrial, sigma = 12
  )
  expect_equal(post, loadTipmapData("tipPost.rds"), tolerance = 1e-3)
})
