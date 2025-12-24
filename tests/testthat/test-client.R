test_that("client() initializes", {
  old <- Sys.getenv("LICHESS_TOKEN")
  Sys.setenv(LICHESS_TOKEN = "")
  on.exit(Sys.setenv(LICHESS_TOKEN = old), add = TRUE)

  cl <- client(quiet = TRUE)
  expect_s3_class(cl, "horsey_client")
  expect_null(cl$token)
  expect_equal(cl$base_url, "https://lichess.org")
  expect_true(cl$quiet)
})

test_that("client() correct behavior for quiet", {
  old <- Sys.getenv("LICHESS_TOKEN")
  Sys.setenv(LICHESS_TOKEN = "")
  on.exit(Sys.setenv(LICHESS_TOKEN = old), add = TRUE)

  expect_warning(cl <- client(quiet = FALSE))
  expect_false(cl$quiet)
})

test_that("client() correct default for quiet", {
  old <- Sys.getenv("LICHESS_TOKEN")
  Sys.setenv(LICHESS_TOKEN = "")
  on.exit(Sys.setenv(LICHESS_TOKEN = old), add = TRUE)

  expect_warning(cl <- client())
  expect_false(cl$quiet)
})

test_that("client() uses the user provided token and base url", {
  cl <- client(token = "abc123", base_url = "https://example.com")

  expect_s3_class(cl, "horsey_client")
  expect_equal(cl$token, "abc123")
  expect_equal(cl$base_url, "https://example.com")
})

test_that("client() warns about no token found and tells user that some endpoints won't work", {
  expect_warning(cl <- client())
})

test_that("client() warns when no token is available", {
  old <- Sys.getenv("LICHESS_TOKEN")
  Sys.setenv(LICHESS_TOKEN = "")
  on.exit(Sys.setenv(LICHESS_TOKEN = old), add = TRUE)

  expect_warning(client(), "No token found")
})
