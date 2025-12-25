#' Get a user's public profile
#'
#' Retrieves a public user profile from the Lichess API.
#'
#' @param cl A `horsey_client`.
#' @param username Lichess username.
#' @return A tibble with selected fields, plus a `raw` list-column.
#' @export
user <- function(cl, username) {
  dat <- horsey_get_json(cl, paste0("/api/user/", username))

  tibble::tibble(
    id = dat$id %||% NA_character_,
    username = dat$username %||% NA_character_,
    title = dat$title %||% NA_character_,
    createdAt = as.POSIXct((dat$createdAt %||% NA_real_) / 1000,
                           origin = "1970-01-01",
                           tz = "UTC"
    ),
    seenAt = as.POSIXct((dat$seenAt %||% NA_real_) / 1000,
                        origin = "1970-01-01",
                        tz = "UTC"
    ),
    following = dat$following %||% NA_integer_,
    followers = dat$followers %||% NA_integer_,
    raw = list(dat)
  )
}

#' Get a user's rating history
#'
#' Returns rating history for each game type (e.g., blitz, rapid).
#'
#' @param cl A `horsey_client`.
#' @param username A Lichess username.
#' @return A tibble with columns perf, rating, date.
#' @export
rating_history <- function(cl, username) {
  req <- horsey_build_req(cl,
                          paste0("/api/user/", username, "/rating-history"),
                          accept = "application/json")
  resp <- horsey_perform(req, quiet = cl$quiet)
  txt <- httr2::resp_body_string(resp)

  rh <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  rows <- list()
  idx <- 0L

  for (perf in rh) {
    perf_name <- perf$name %||% NA_character_
    points <- perf$points %||% list()
    if (length(points) == 0)
      next

    for (pt in points) {
      if (length(pt) < 4)
        next

      idx <- idx + 1L
      rows[[idx]] <- tibble::tibble(
        perf = perf_name,
        rating = as.integer(pt[[4]]),
        date = as.Date(
          sprintf(
            "%04d-%02d-%02d",
            as.integer(pt[[1]]),
            as.integer(pt[[2]]) + 1L,
            as.integer(pt[[3]])
          )
        )
      )
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      perf = character(),
      rating = integer(),
      date = as.Date(character())
    ))
  }

  dplyr::bind_rows(rows)
}


#' Get a user's activity feed
#'
#' Retrieves the activity feed for a user. This wrapper intentionally returns
#' a minimal summary and includes the raw activity objects in a list-column.
#'
#' @param cl A `horsey_client`.
#' @param username Lichess username.
#' @return A tibble with one row per activity entry and a `raw` column.
#' @export
user_activity <- function(cl, username) {
  dat <- horsey_get_json(cl, paste0("/api/user/", username, "/activity"))
  tibble::tibble(username = username,
                 n = length(dat),
                 raw = list(dat))
}
