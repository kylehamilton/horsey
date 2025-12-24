#' Create a Lichess API client for analysis/export workflows
#'
#' `horsey` is designed for read-only access and data export. Provide a token for
#' endpoints that require authentication. If `token` is `NULL`, `client()` will
#' use `Sys.getenv("LICHESS_TOKEN")`.
#'
#' @param token Bearer token (personal API token or OAuth access token). If NULL,
#'   uses `Sys.getenv("LICHESS_TOKEN")`.
#' @param base_url API base URL. Defaults to `https://lichess.org`.
#' @param quiet Suppress informational messages.
#' @return An object of class `horsey_client`.
#' @export
client <- function(token = NULL, base_url = "https://lichess.org", quiet = FALSE) {
  tok <- token %||% Sys.getenv("LICHESS_TOKEN", unset = "")
  if (!nzchar(tok)) tok <- NULL

  if (!quiet && is.null(tok)) {
    cli::cli_warn(c(
      "No token found. Public endpoints will work. Some endpoints may require authentication.",
      "i" = "Set Sys.setenv(LICHESS_TOKEN = 'lip_...') or pass token = 'lip_...'"
    ))
  }

  structure(
    list(
      token = tok,
      base_url = base_url, # I added this just in case something changes later -wkh
      quiet = quiet
    ),
    class = "horsey_client"
  )
}


