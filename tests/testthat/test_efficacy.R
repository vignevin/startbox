test_that("efficacy calculates correctly", {
  # Test with normal values
  expect_equal(efficacy(50, 100), 50)  # check efficacy calculation
  expect_equal(efficacy(25, 100), 75)  # other case

  # Test with negative results
  expect_equal(efficacy(150, 100), -50)  # should return 0

  # chek if value = 0
  expect_equal(efficacy(0, 100), 100)  # chek if value = 0

  # chek if value = value
  expect_equal(efficacy(100, 100), 0)  # should return 0
})
