test_that("expert weight summary works", {
  summary <- getExpertWeightSummary(
    rbind(
      c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0), c(0, 0, 0, 2, 3, 3, 2, 0, 0, 0),
      c(0, 1, 2, 4, 2, 1, 0, 0, 0, 0), c(2, 3, 3, 2, 0, 0, 0, 0, 0, 0),
      c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0), c(2, 3, 3, 2, 0, 0, 0, 0, 0, 0)
    ),
    n = 50000
  )
  expect_equal((as.numeric(unname(summary["1st Qu."]))), 0.2425842, tolerance = 1e-2)
  expect_equal((as.numeric(unname(summary["Median"]))), 0.4023139, tolerance = 1e-2)
  expect_equal((as.numeric(unname(summary["Mean"]))), 0.4034246, tolerance = 1e-2)
  expect_equal((as.numeric(unname(summary["3rd Qu."]))), 0.5568222, tolerance = 1e-2)
})
