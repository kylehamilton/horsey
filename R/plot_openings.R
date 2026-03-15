#' Plot most common openings
#'
#' Builds a horizontal bar chart of the most frequent openings found in
#' `horsey::export_games()` output when `opening = TRUE` was requested.
#'
#' @param games A tibble returned by `horsey::export_games()`.
#' @param top_n Number of openings to show.
#' @param include_eco Logical. If `TRUE`, prepend ECO code to the label.
#'
#' @return A `ggplot2` object.
#' @import dplyr ggplot2
#' @export
plot_opening_counts <- function(games,
                                top_n = 10,
                                include_eco = TRUE) {
  hv_assert_data_frame(games, "games")
  hv_assert_columns(games, c("opening_name", "opening_eco"), arg = "games")

  opening_df <- games |>
    dplyr::filter(!is.na(.data$opening_name), .data$opening_name != "") |>
    dplyr::count(.data$opening_eco,
                 .data$opening_name,
                 sort = TRUE,
                 name = "games") |>
    utils::head(top_n)

  if (nrow(opening_df) == 0) {
    rlang::abort("No opening information found. Re-export games with `opening = TRUE`.")
  }

  opening_df$label <- if (isTRUE(include_eco)) {
    paste0(opening_df$opening_eco, " - ", opening_df$opening_name)
  } else {
    opening_df$opening_name
  }

  opening_df$label <- stats::reorder(opening_df$label, opening_df$games)

  ggplot2::ggplot(opening_df, ggplot2::aes(x = .data$games, y = .data$label)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Most Common Openings", x = "Games", y = NULL) +
    ggplot2::theme_minimal()
}
