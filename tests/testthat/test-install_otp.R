# Calling install_otp() with out supplying the argument dir throws an error

test_that("install_otp() throws an error", {
  expect_error(install_otp())
})
