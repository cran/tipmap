test_that("stochastic posterior works", {
  stochPost <- createStochasticWeightPosterior(
    mapPrior = loadTipmapData("tipmapPrior.rds"),
    newTrialData = createNewTrialData(30, 1.4, 2.0),
    sigma = 12, weights = c(0.1, 0.15, 0.2, 0.13)
  )
  expect_length(stochPost, 3)
  expect_equal(unname(unlist(stochPost["Mean"])),
    c(
      -2.6994460, -1.8955773, -1.1854153, -0.3530752,
      0.5444183, 0.8015384, 1.4880758, 2.1164420,
      2.3310441, 3.1240281, 3.9229698, 4.6248538, 5.4252311
    ),
    tolerance = 1e-3
  )
  expect_equal(unname(unlist(stochPost["MCE"])),
    c(
      0.06801411, 0.0793849, 0.09165378, 0.10205, 0.07243725,
      0.05116118, 0.004654271, 0.03156069, 0.04842516,
      0.08838457, 0.08775882, 0.07800568, 0.06759929
    ),
    tolerance = 1e-3
  )
})
