test_that("ll_check_api_key", {
  expect_message(ll_check_api_key())
})

test_that("ll_check_connection", {
  expect_no_error(ll_check_connection())
})
