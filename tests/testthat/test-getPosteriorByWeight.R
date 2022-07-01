test_that("filter posterior by weight", {
  filtered <- getPosteriorByWeight(
    posterior = loadTipmapData("tipPost.rds"),
    weight = c(0.4, 0.5)
  )
  expected <- data.frame(
    q0.01 = c(-1.916890, -1.599103),
    q0.025 = c(-0.9783855, -0.6309729),
    q0.05 = c(-0.1976421, 0.06094811),
    q0.1 = c(0.4642938, 0.58516616),
    q0.2 = c(0.9375285, 0.98143010),
    q0.25 = c(1.0691476, 1.09999518),
    q0.5 = c(1.5136884, 1.51701593),
    q0.75 = c(1.9476713, 1.92709568),
    q0.8 = c(2.0711345, 2.04057194),
    q0.9 = c(2.4924470, 2.40645067),
    q0.95 = c(3.0613084, 2.86904035),
    q0.975 = c(3.7581780, 3.46938256),
    q0.99 = c(4.6579968, 4.36022790),
    row.names = c("w=0.4", "w=0.5")
  )
  expect_equal(filtered, expected, tolerance = 1e-4)
})
