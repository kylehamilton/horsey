#' Compute win rate from exported games
#'
#' Computes a win rate from the tibble returned by [export_games()].
#'
#' @param games Tibble returned by `export_games()`.
#' @param player Optional username to compute perspective-correct results.
#'   If NULL, uses `winner` only and treats non-win as non-win.
#' @return Win rate.
#' @export
winrate <- function(games, player = NULL) {
  stopifnot(is.data.frame(games))

  if (is.null(player)) {
    wins <- sum(games$winner %in% c("white", "black"), na.rm = TRUE) # not perfect but good enough -wkh
    return(wins / nrow(games))
  }

  p <- tolower(player)
  w <- tolower(games$white_user)
  b <- tolower(games$black_user)

  is_white <- !is.na(w) & w == p
  is_black <- !is.na(b) & b == p

  wins <- (is_white & games$winner == "white") | (is_black & games$winner == "black")
  denom <- sum(is_white | is_black, na.rm = TRUE)
  if (denom == 0) return(NA_real_)
  sum(wins, na.rm = TRUE) / denom
}

#' Summarize results (W/L/D) for a player
#'
#' Produces win/loss/draw counts for a specified player
#'
#' @param games Tibble returned by `export_games()`.
#' @param player Username (required for accurate W/L/D).
#' @return A tibble with one row containing the player name plus totals for
#'   games, wins, losses, and draws.
#' @export
result_summary <- function(games, player) {
  stopifnot(is.data.frame(games))
  p <- tolower(player)
  w <- tolower(games$white_user)
  b <- tolower(games$black_user)

  is_white <- !is.na(w) & w == p
  is_black <- !is.na(b) & b == p

  in_scope <- is_white | is_black
  if (sum(in_scope, na.rm = TRUE) == 0) {
    return(tibble::tibble(player = player, games = 0L, wins = 0L, losses = 0L, draws = 0L))
  }

  res <- dplyr::case_when(
    games$winner == "white" & is_white ~ "win",
    games$winner == "black" & is_black ~ "win",
    games$winner == "white" & is_black ~ "loss",
    games$winner == "black" & is_white ~ "loss",
    TRUE ~ "draw"
  )

  tibble::tibble(
    player = player,
    games = sum(in_scope, na.rm = TRUE),
    wins = sum(in_scope & res == "win", na.rm = TRUE),
    losses = sum(in_scope & res == "loss", na.rm = TRUE),
    draws = sum(in_scope & res == "draw", na.rm = TRUE)
  )
}

#' Summarize openings from exported games
#'
#' Counts openings from [export_games()] output.
#' To include opening information, `opening = TRUE` in
#' [export_games()].
#'
#' Two columns are produced by [export_games()] when
#' `opening = TRUE`:
#' \describe{
#'   \item{opening_name}{An opening name provided by Lichess (for example,
#'   "Sicilian Defense" or "Queen's Gambit Declined").}
#'   \item{opening_eco}{The ECO code associated with the opening (for example, "B20" or "D30").
#'   ECO (Encyclopaedia of Chess Openings) codes are a standardized classification system
#'   for chess openings.}
#' }
#' @param games Tibble returned by `export_games()`.
#' @param top_n Number of top openings to return.
#' @return A tibble of the most common openings with columns for the ECO code,
#'   opening name, and count of games.
#' @importFrom rlang .data
#' @export

# Maybe I'll expand this convenience function later -wkh
opening_summary <- function(games, top_n = 20) {
  stopifnot(is.data.frame(games))
  games |>
    dplyr::filter(!is.na(.data$opening_name) & .data$opening_name != "") |>
    dplyr::count(.data$opening_eco, .data$opening_name, sort = TRUE) |>
    dplyr::slice_head(n = top_n)
}

#‿︵‿︵‿\o/︵‿︵‿︵‿︵‿︵◢‿︵‿︵‿︵‿
