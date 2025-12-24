test_that("client() initializes", {
  old <- Sys.getenv("LICHESS_TOKEN")
  Sys.setenv(LICHESS_TOKEN = "")
  on.exit(Sys.setenv(LICHESS_TOKEN = old), add = TRUE)

  cl <- client(quiet = TRUE)
  expect_s3_class(cl, "horsey_client")
  expect_null(cl$token)
  expect_equal(cl$base_url, "https://lichess.org")
})

test_that("client() uses the user provided token and base url", {
  cl <- client(token = "abc123", base_url = "https://example.com")

  expect_s3_class(cl, "horsey_client")
  expect_equal(cl$token, "abc123")
  expect_equal(cl$base_url, "https://example.com")
})

test_that("client() warns when no token is available", {
  old <- Sys.getenv("LICHESS_TOKEN")
  Sys.setenv(LICHESS_TOKEN = "")
  on.exit(Sys.setenv(LICHESS_TOKEN = old), add = TRUE)

  expect_warning(client(), "No token found")
})
