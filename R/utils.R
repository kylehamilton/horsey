# Internal utilities

`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else
    x
}

#' @export
print.horsey_client <- function(x, ...) {
  cli::cli_text("<horsey_client>")
  cli::cli_text("  base_url: {x$base_url}")
  cli::cli_text("  auth: {if (is.null(x$token)) 'none' else 'bearer token'}")
  invisible(x)
}

#' @keywords internal
"_PACKAGE"
#' @import stringr
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr filter count slice_head case_when
#' @importFrom cli cli_text cli_warn
#' @importFrom rlang abort
#' @importFrom jsonlite fromJSON
#' @importFrom httr2 request req_method req_user_agent req_timeout req_headers
#'   req_url_query req_perform resp_status resp_body_string resp_body_raw
NULL


#' @keywords internal
#' @noRd
hv_assert_data_frame <- function(x, arg = deparse(substitute(x))) {
  if (!is.data.frame(x)) {
    rlang::abort(paste0("`", arg, "` must be a data frame or tibble."))
  }
}

#' @keywords internal
#' @noRd
hv_assert_columns <- function(data, required, arg = deparse(substitute(data))) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    rlang::abort(paste0(
      "`",
      arg,
      "` is missing required columns: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }
}

#' @keywords internal
#' @noRd
hv_match_player_games <- function(games, player) {
  hv_assert_data_frame(games, "games")
  hv_assert_columns(
    games,
    c(
      "white_user",
      "black_user",
      "winner",
      "white_rating",
      "black_rating",
      "perf",
      "createdAt"
    ),
    arg = "games"
  )

  if (!is.character(player) ||
      length(player) != 1L || is.na(player) || !nzchar(player)) {
    rlang::abort("`player` must be a single non-empty character string.")
  }

  player_lower <- tolower(player)
  white_lower <- tolower(as.character(games$white_user))
  black_lower <- tolower(as.character(games$black_user))

  is_white <- !is.na(white_lower) & white_lower == player_lower
  is_black <- !is.na(black_lower) & black_lower == player_lower
  keep <- is_white | is_black

  out <- games[keep, , drop = FALSE]
  if (nrow(out) == 0) {
    rlang::abort("No games found for the supplied `player` in `games`.")
  }

  out$color <- ifelse(is_white[keep], "White", "Black")
  out$opponent <- ifelse(is_white[keep], out$black_user, out$white_user)
  out$player_rating <- ifelse(is_white[keep], out$white_rating, out$black_rating)
  out$opponent_rating <- ifelse(is_white[keep], out$black_rating, out$white_rating)
  out$rating_gap <- out$player_rating - out$opponent_rating

  out$result <- ifelse(
    (is_white[keep] &
       out$winner == "white") |
      (is_black[keep] & out$winner == "black"),
    "Win",
    ifelse(is.na(out$winner) | out$winner == "", "Draw", "Loss")
  )

  out$result <- factor(out$result, levels = c("Loss", "Draw", "Win"))
  out$result_points <- dplyr::case_when(out$result == "Win" ~ 1, out$result == "Draw" ~ 0.5, TRUE ~ 0)

  out
}

#' @keywords internal
#' @noRd
hv_floor_date <- function(x, unit = c("week", "month")) {
  unit <- match.arg(unit)
  x <- as.Date(x)

  if (unit == "month") {
    return(as.Date(format(x, "%Y-%m-01")))
  }

  weekday <- as.POSIXlt(x)$wday
  week_start <- x - ((weekday + 6) %% 7)
  as.Date(week_start, origin = "1970-01-01")
}

#' @keywords internal
#' @noRd
hv_day_matrix <- function(dates) {
  d <- as.Date(dates)
  counts <- as.data.frame(table(d), stringsAsFactors = FALSE)
  names(counts) <- c("date", "games")
  counts$date <- as.Date(counts$date)

  counts$weekday_num <- ((as.POSIXlt(counts$date)$wday + 6) %% 7) + 1
  counts$weekday <- factor(
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[counts$weekday_num],
    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  )
  counts$week <- hv_floor_date(counts$date, "week")
  counts
}
