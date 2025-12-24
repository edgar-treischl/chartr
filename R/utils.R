
#' Evaluate plot variables
#'
#' @param data Data frame
#' @param x X variable
#' @param y Y variable
#' @param group Grouping variable
#' @param facet Faceting variable
#'
#' @returns Data frame with evaluated variables
#'
eval_plot_vars <- function(data, x, y, group = NULL, facet = NULL) {

  df <- data |>
    dplyr::mutate(
      .x = !!x,
      .y = !!y
    )

  if (!rlang::quo_is_null(group)) {
    df <- df |>
      dplyr::mutate(.group = !!group)
  }

  if (!rlang::quo_is_null(facet)) {
    df <- df |>
      dplyr::mutate(.facet = !!facet)
  }

  df
}

#' Summarize plot data for X-Y plots
#'
#' @param df Data frame with .x, .y, .group, .facet columns
#' @param stat Statistic to summarize by: "mean", "sum", "count"
#' @returns Summarized data frame
summarize_xy <- function(df, stat = "mean") {

  group_vars <- c(".x")
  if (".group" %in% names(df)) group_vars <- c(group_vars, ".group")
  if (".facet" %in% names(df)) group_vars <- c(group_vars, ".facet")

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      .y = switch(
        stat,
        mean  = mean(.y, na.rm = TRUE),
        sum   = sum(.y, na.rm = TRUE),
        count = dplyr::n(),
        rlang::abort("Unknown stat")
      ),
      .groups = "drop"
    )
}


#' Build base ggplot
#'
#' @param df Data frame with .x, .y, .group columns
#' @param legend_title Legend title
#'
#' @returns ggplot object

build_base_plot <- function(df, legend_title) {

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .x,
      y = .y,
      fill = if (".group" %in% names(df)) .data$.group else NULL
    )
  ) +
    ggplot2::labs(fill = legend_title)
}



#' Add facets to ggplot
#'
#' @param p ggplot object
#' @param df A data frame
#' @param scales Scales for facets
#' @param ncol Number of columns for facets
#'
add_facets <- function(p, df, scales, ncol) {

  if (".facet" %in% names(df)) {
    p + ggplot2::facet_wrap(~.facet, scales = scales, ncol = ncol)
  } else {
    p
  }
}



#' Add common theme elements to ggplot
#'
#' @param p ggplot object
#' @param title A title
#' @param x_label X axis label
#' @param y_label Y axis label
#' @param note Caption / Note
#' @param legend_position Legend position
#' @param legend_direction Legend direction
#'
add_common_theme <- function(
    p,
    title,
    x_label,
    y_label,
    note,
    legend_position,
    legend_direction
) {

  p +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label,
      caption = note
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = legend_position,
      legend.direction = legend_direction
    )
}










