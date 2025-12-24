test_that("result_summary computes W/L/D", {
  g <- tibble::tibble(
    white_user = c("Juniper", "Kyle", "Juniper"),
    black_user = c("Kyle", "Juniper", "Kat"),
    winner = c("white", NA, "black"),
    opening_name = c("Caro-Kann Defense", NA, "Sicilian Defense"),
    opening_eco = c("B10", NA, "B20")
  )

  rs <- result_summary(g, player = "Juniper") # If anyone reads this Juniper is my cat -wkh
  expect_equal(rs$games, 3)
  expect_equal(rs$wins, 1)
  expect_equal(rs$losses, 1)
  expect_equal(rs$draws, 1)

  os <- opening_summary(g, top_n = 10)
  expect_true(nrow(os) >= 1)
})

test_that("winrate calculates overall and player-specific rates", {
  g <- tibble::tibble(
    white_user = c("Juniper", "Kyle", "Juniper"),
    black_user = c("Kyle", "Juniper", "Kat"),
    winner = c("white", "black", NA_character_)
  )

  expect_equal(winrate(g), 2 / 3)
  expect_equal(winrate(g, player = "Juniper"), 2 / 3)
  expect_true(is.na(winrate(g, player = "Nobody")))
})

test_that("opening_summary filters missing and limits results", {
  g <- tibble::tibble(
    opening_eco = c("B10", "C20", "C20", NA, ""),
    opening_name = c(
      "Caro-Kann Defense",
      "King's Pawn Game",
      "King's Pawn Game",
      "French Defense",
      "Ruy Lopez"
    )
  )

  top <- opening_summary(g, top_n = 1)
  expect_equal(nrow(top), 1)
  expect_equal(top$opening_eco, "C20")
  expect_equal(top$n, 2)
})

test_that("result_summary returns zeros when player has no games", {
  g <- tibble::tibble(
    white_user = c("Juniper", "Kyle"),
    black_user = c("Kyle", "Kat"),
    winner = c("white", "black")
  )

  rs <- result_summary(g, player = "Mr. Ham") # Mr. Ham is our barn cat, he would fair poorly in a chess game -wkh
  expect_equal(rs$games, 0)
  expect_equal(rs$wins, 0)
  expect_equal(rs$losses, 0)
  expect_equal(rs$draws, 0)
})

#                       /^--^\     /^--^\     /^--^\
#                       \____/     \____/     \____/
#                      /      \   /      \   /      \
# KAT                 |        | |        | |        |
#                      \__  __/   \__  __/   \__  __/
# |^|^|^|^|^|^|^|^|^|^|^|^\ \^|^|^|^/ /^|^|^|^|^\ \^|^|^|^|^|^|^|^|^|^|^|^|
# | | | | | | | | | | | | |\ \| | |/ /| | | | | | \ \ | | | | | | | | | | |
# ########################/ /######\ \###########/ /#######################
# | | | | | | | | | | | | \/| | | | \/| | | | | |\/ | | | | | | | | | | | |
# |_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|

test_that("opening_summary orders by frequency and uses default top_n", {
  g <- tibble::tibble(
    opening_eco = c("A00", "B01", "B01", "C02", "C02", "C02"),
    opening_name = c(
      "Anderssen Opening",
      "Scandinavian Defense",
      "Scandinavian Defense",
      "French Defense: Advance Variation",
      "French Defense: Advance Variation",
      "French Defense: Advance Variation"
    )
  )

  top <- opening_summary(g)

  expect_equal(nrow(top), 3)
  expect_equal(top$n, c(3, 2, 1))
  expect_equal(top$opening_eco, c("C02", "B01", "A00"))
})
