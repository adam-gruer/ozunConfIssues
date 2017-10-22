context("retrieve issues")

test_that("got api response", {
  x <- get_api_response()
  expect_is(x,"response")
  expect_type(x,"list")
  expect_equal(x$status_code, 200L)

  x <- get_json_data()
  expect_type(x, "character")

  x <- parse_json()
  expect_type(x, "list")
  expect_is(x, "data.frame")

})
