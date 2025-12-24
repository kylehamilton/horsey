test_that("client() initializes", {
  old <- Sys.getenv("LICHESS_TOKEN")
  Sys.setenv(LICHESS_TOKEN = "")
  on.exit(Sys.setenv(LICHESS_TOKEN = old), add = TRUE)

  cl <- client(quiet = TRUE)
  expect_s3_class(cl, "horsey_client")
  expect_null(cl$token)
  expect_equal(cl$base_url, "https://lichess.org")
})
