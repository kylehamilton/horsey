# Internal

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

horsey_get_json <- function(cl, path, query = NULL) {
  req <- horsey_build_req(cl, path, query = query, accept = "application/json")
  resp <- horsey_perform(req, quiet = cl$quiet)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
}

horsey_get_ndjson_lines <- function(cl, path, query = NULL) {
  req <- horsey_build_req(cl, path, query = query, accept = "application/x-ndjson")
  resp <- horsey_perform(req, quiet = cl$quiet)
  txt <- httr2::resp_body_string(resp)
  lines <- unlist(strsplit(txt, "\n", fixed = FALSE), use.names = FALSE)
  lines[nzchar(lines)]
}

horsey_parse_ndjson <- function(lines) {
  # Returns list of parsed objects (one per line)
  lapply(lines, function(line)
    jsonlite::fromJSON(line, simplifyVector = TRUE))
}
