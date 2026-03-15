#' Plot result mix by time control
#'
#' @param games A tibble returned by `horsey::export_games()`.
#' @param player Lichess username to evaluate from that player's perspective.
#'
#' @return A `ggplot2` object.
#' @import dplyr ggplot2
#' @export
plot_time_control_results <- function(games, player) {
  player_games <- hv_match_player_games(games, player)

  result_df <- player_games |>
    dplyr::count(.data$perf, .data$result, name = "games") |>
    dplyr::group_by(.data$perf) |>
    dplyr::mutate(prop = .data$games / sum(.data$games)) |>
    dplyr::ungroup()

  ggplot2::ggplot(result_df,
                  ggplot2::aes(
                    x = .data$perf,
                    y = .data$prop,
                    fill = .data$result
                  )) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = c(
      Loss = "#B22222",
      Draw = "#808080",
      Win = "#228B22"
    )) +
    ggplot2::labs(
      title = paste("Results by Time Control -", player),
      x = "Perf",
      y = "Share of games",
      fill = "Result"
    ) +
    ggplot2::theme_minimal()
}

#' Plot rolling results over time
#'
#' @param games A tibble returned by `horsey::export_games()`.
#' @param player Lichess username to evaluate from that player's perspective.
#' @param by Aggregate by `"week"` or `"month"`.
#'
#' @return A `ggplot2` object.
#' @import dplyr ggplot2
#' @export
plot_results_over_time <- function(games, player, by = c("week", "month")) {
  by <- match.arg(by)
  player_games <- hv_match_player_games(games, player)

  trend_df <- player_games |>
    dplyr::mutate(period = hv_floor_date(.data$createdAt, unit = by)) |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(
      games = dplyr::n(),
      score = mean(.data$result_points, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot2::ggplot(trend_df, ggplot2::aes(x = .data$period, y = .data$score)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                limits = c(0, 1)) +
    ggplot2::labs(
      title = paste("Average Score Over Time -", player),
      subtitle = paste("Grouped by", by),
      x = NULL,
      y = "Average score"
    ) +
    ggplot2::theme_minimal()
}
