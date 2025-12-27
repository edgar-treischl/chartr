#' Bar Position
#'
#' @param stacked Stacked bars
#' @param horizontal Horizontal bars
#'
bar_position <- function(stacked, horizontal) {
  if (stacked) {
    ggplot2::position_stack()
  } else {
    ggplot2::position_dodge(width = if (horizontal) 0.8 else 0.9)
  }
}


#' Bar Labels
#'
#' @param p ggplot object
#' @param df Data frame with .y column
#' @param stacked Logical, whether bars are stacked
#' @param label_size Label size
#'
bar_labels <- function(p, df, stacked, label_size) {

  if (stacked) {
    p + ggplot2::geom_text(
      data = df,
      ggplot2::aes(label = round(.y, 1)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = label_size
    )
  } else {
    p + ggplot2::geom_text(
      data = df,
      ggplot2::aes(label = round(.y, 1)),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = -0.3,
      size = label_size
    )
  }
}

#' Summarize plot data for Bars
#'
#' @param df Data frame with .x, .y, .group, .facet columns
#'
#' @returns Summarized data frame
bar_summarize_data <- function(df) {

  group_vars <- c(".x")
  if (".group" %in% names(df)) group_vars <- c(group_vars, ".group")
  if (".facet" %in% names(df)) group_vars <- c(group_vars, ".facet")

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      .y = mean(.y, na.rm = TRUE),
      .groups = "drop"
    )
}


#' Build base ggplot
#'
#' @param df Data frame with .x, .y, .group columns
#' @param legend_title Legend title
#'
#' @returns ggplot object
bar_build_base_plot <- function(df, legend_title) {

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


#' Add bar labels to ggplot
#'
#' @param p ggplot object
#' @param df Data frame with .y column
#' @param stacked Logical, whether bars are stacked
#' @param label_size Numeric, size of the labels
#'
bar_add_labels <- function(p, df, stacked, label_size) {

  if (stacked) {
    p + ggplot2::geom_text(
      data = df,
      ggplot2::aes(label = round(.y, 1)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = label_size
    )
  } else {
    p + ggplot2::geom_text(
      data = df,
      ggplot2::aes(label = round(.y, 1)),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = -0.3,
      size = label_size
    )
  }
}











