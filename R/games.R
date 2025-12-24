#' Export games for a player
#'
#' Downloads games from `/api/games/user/{username}` and requests NDJSON.
#' Returns a tibble of commonly-used analysis fields, plus a `raw` list-column
#' containing each full game object as returned by the API.
#'
#' @param cl A `horsey_client`.
#' @param username Lichess username.
#' @param max Maximum number of games to request (default 100).
#' @param since Optional epoch milliseconds (inclusive).
#' @param until Optional epoch milliseconds (exclusive).
#' @param perfType Optional perf filter (e.g., "blitz", "rapid").
#' @param rated Optional logical filter.
#' @param opening If TRUE, ask Lichess to include opening info when available.
#' @return A tibble with selected metadata columns and a `raw` list-column.
#' @export
export_games <- function(cl, username, max = 100, since = NULL, until = NULL,
                         perfType = NULL, rated = NULL, opening = TRUE) {

  query <- list(max = max)
  if (!is.null(since)) query$since <- since # I need to redo this, I don't like it but it works for now -wkh
  if (!is.null(until)) query$until <- until
  if (!is.null(perfType)) query$perfType <- perfType
  if (!is.null(rated)) query$rated <- tolower(as.character(rated))
  if (!is.null(opening)) query$opening <- tolower(as.character(opening))

  lines <- horsey_get_ndjson_lines(cl, paste0("/api/games/user/", username), query = query)
  objs <- horsey_parse_ndjson(lines)

  # There has to be a better way of doing this but it works and I'm not going to break it -wkh
  tibble::tibble(
    id = vapply(objs, function(x) x$id %||% NA_character_, character(1)),
    rated = vapply(objs, function(x) x$rated %||% NA, logical(1)),
    speed = vapply(objs, function(x) x$speed %||% NA_character_, character(1)),
    perf = vapply(objs, function(x) x$perf %||% NA_character_, character(1)),
    status = vapply(objs, function(x) x$status %||% NA_character_, character(1)),
    createdAt = as.POSIXct(vapply(objs, function(x) (x$createdAt %||% NA_real_) / 1000, numeric(1)),
                          origin = "1970-01-01", tz = "UTC"),
    lastMoveAt = as.POSIXct(vapply(objs, function(x) (x$lastMoveAt %||% NA_real_) / 1000, numeric(1)),
                           origin = "1970-01-01", tz = "UTC"),
    white_user = vapply(objs, function(x) x$players$white$user$name %||% x$players$white$user$id %||% NA_character_, character(1)),
    black_user = vapply(objs, function(x) x$players$black$user$name %||% x$players$black$user$id %||% NA_character_, character(1)),
    white_rating = vapply(objs, function(x) x$players$white$rating %||% NA_integer_, integer(1)),
    black_rating = vapply(objs, function(x) x$players$black$rating %||% NA_integer_, integer(1)),
    winner = vapply(objs, function(x) x$winner %||% NA_character_, character(1)),
    opening_eco = vapply(objs, function(x) x$opening$eco %||% NA_character_, character(1)),
    opening_name = vapply(objs, function(x) x$opening$name %||% NA_character_, character(1)),
    raw = objs
  )
}

