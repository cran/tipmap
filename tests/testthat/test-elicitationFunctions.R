test_that("cumulative probs works", {
  expect_equal(
    getExpertCumulativeProbs(
      chips =
        c(0, 0, 0, 0, 2, 3, 2, 2, 1, 0)
    ),
    c(0.0, 0.0, 0.0, 0.0, 0.2, 0.5, 0.7, 0.9, 1.0, 1.0)
  )
})

test_that("model input data works", {
  expect_equal(
    nrow(getModelInputData(
      cumprobs =
        getExpertCumulativeProbs(
          chips = c(0, 0, 0, 0, 2, 3, 2, 2, 1, 0)
        ), w = 1
    )),
    5
  )
  expect_equal(
    ncol(getModelInputData(
      cumprobs =
        getExpertCumulativeProbs(
          chips = c(0, 0, 0, 0, 2, 3, 2, 2, 1, 0)
        ), w = 1
    )),
    2
  )
  expect_equal(
    getModelInputData(
      cumprobs =
        getExpertCumulativeProbs(
          chips = c(0, 0, 0, 0, 2, 3, 2, 2, 1, 0)
        ), w = 1
    )$cumprobs,
    c(0.2, 0.5, 0.7, 0.9, 1)
  )
})

test_that("drawing samples works", {
  expertWeights <- rbind(
    c(0, 0, 0, 0, 2, 3, 3, 2, 0, 0),
    c(0, 0, 0, 1, 2, 4, 2, 1, 0, 0),
    c(0, 0, 0, 2, 2, 2, 2, 2, 0, 0)
  )
  expect_length(drawExpertWeightSample(expertWeights, n = 100), 100)
  expect_lte(sum(drawExpertWeightSample(expertWeights, n = 100)), 100)
})
