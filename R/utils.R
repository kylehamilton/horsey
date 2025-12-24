# Internal utilities

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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
