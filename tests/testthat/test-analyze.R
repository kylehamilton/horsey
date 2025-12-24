test_that("result_summary computes W/L/D", {
  g <- tibble::tibble(
    white_user = c("Juniper", "Kyle", "Juniper"),
    black_user = c("Kyle", "Juniper", "Kat"),
    winner = c("white", NA, "black"),
    opening_name = c("Caro-Kann Defense", NA, "Sicilian Defense"),
    opening_eco = c("B10", NA, "B20")
  )

  rs <- result_summary(g, player = "Juniper")
  expect_equal(rs$games, 3)
  expect_equal(rs$wins, 1)
  expect_equal(rs$losses, 1)
  expect_equal(rs$draws, 1)

  os <- opening_summary(g, top_n = 10)
  expect_true(nrow(os) >= 1)
})

