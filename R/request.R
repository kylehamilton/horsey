#' Build Lichess API Request
#'
#' Constructs an httr2 request object for the Lichess API with authentication
#' and headers.
#'
#' @param cl A horsey_client object
#' @param path Character. API endpoint path
#' @param query Named list. Query parameters (optional)
#' @param accept Character. Accept header value (optional)
#'
#' @return An httr2 request object
#' @keywords internal
#' @noRd
horsey_build_req <- function(cl,
                             path,
                             query = NULL,
                             accept = NULL) {
  stopifnot(inherits(cl, "horsey_client"))
  url <- paste0(cl$base_url, path)

  req <- httr2::request(url) |>
    httr2::req_method("GET") |>
    httr2::req_user_agent("horsey (R; analysis/export)") |>
    httr2::req_timeout(60)

  if (!is.null(query))
    req <- httr2::req_url_query(req, !!!query)
  if (!is.null(accept))
    req <- httr2::req_headers(req, Accept = accept)

  if (!is.null(cl$token)) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", cl$token))
  }

  req
}

#' Perform Lichess API Request
#'
#' Executes an httr2 request with retry logic for rate limiting and error handling.
#'
#' @param req An httr2 request object
#' @param quiet Logical. Suppress warning messages
#' @param max_tries Integer. Maximum retry attempts for rate limiting
#'
#' @return An httr2 response object
#' @keywords internal
#' @noRd
horsey_perform <- function(req,
                           quiet = FALSE,
                           max_tries = 2L) {
  tries <- 0L
  repeat {
    tries <- tries + 1L
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)

    if (status == 429 && tries < max_tries) {
      if (!quiet)
        cli::cli_warn("Rate limited (HTTP 429). Waiting 60 seconds, then retrying once.")
      Sys.sleep(60)
      next
    }

    if (status >= 400) {
      body <- tryCatch(
        httr2::resp_body_string(resp),
        error = function(e)
          ""
      )
      rlang::abort(paste0("Lichess API error ", status, if (nzchar(body))
        paste0(": ", body)
        else
          ""))
    }

    return(resp)
  }
}

#' Get JSON from Lichess API
#'
#' Makes a GET request to the Lichess API and parses the JSON response.
#'
#' @param cl A horsey_client object
#' @param path Character. API endpoint path
#' @param query Named list. Query parameters (optional)
#'
#' @return Parsed JSON as R object (typically list or data frame)
#' @keywords internal
#' @noRd
horsey_get_json <- function(cl, path, query = NULL) {
  req <- horsey_build_req(cl, path, query = query, accept = "application/json")
  resp <- horsey_perform(req, quiet = cl$quiet)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
}

#' Get NDJSON Lines from Lichess API
#'
#' Makes a GET request to the Lichess API and returns newline-delimited JSON
#' as individual lines.
#'
#' @param cl A horsey_client object
#' @param path Character. API endpoint path
#' @param query Named list. Query parameters (optional)
#'
#' @return Character vector with one JSON object per element
#' @keywords internal
#' @noRd
horsey_get_ndjson_lines <- function(cl, path, query = NULL) {
  req <- horsey_build_req(cl, path, query = query, accept = "application/x-ndjson")
  resp <- horsey_perform(req, quiet = cl$quiet)
  txt <- httr2::resp_body_string(resp)
  lines <- unlist(strsplit(txt, "\n", fixed = FALSE), use.names = FALSE)
  lines[nzchar(lines)]
}

#' Parse NDJSON Lines
#'
#' Parses newline-delimited JSON lines into a list of R objects.
#'
#' @param lines Character vector. Each element is a JSON string
#'
#' @return List of parsed JSON objects
#' @keywords internal
#' @noRd
horsey_parse_ndjson <- function(lines) {
  # Returns list of parsed objects (one per line)
  lapply(lines, function(line)
    jsonlite::fromJSON(line, simplifyVector = TRUE))
}
