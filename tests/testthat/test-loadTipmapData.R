test_that("load MAP prior", {
  expect_type(loadTipmapData("tipmapPrior.rds"), "double")
  expect_length(loadTipmapData("tipmapPrior.rds"), 9)
})

test_that("load posterior", {
  expect_type(loadTipmapData("tipPost.rds"), "list")
  expect_length(loadTipmapData("tipPost.rds"), 14)
})

test_that("load TP data", {
  expect_type(loadTipmapData("tipdat.rds"), "list")
  expect_length(loadTipmapData("tipdat.rds"), 11)
})
