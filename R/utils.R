
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




#' Summarize plot data for Bars
#'
#' @param df Data frame with .x, .y, .group, .facet columns
#'
#' @returns Summarized data frame
summarize_bar_data <- function(df) {

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




#' Build base ggplot
#'
#' @param df Data frame with .x, .y, .group columns
#' @param legend_title Legend title
#'
#' @returns ggplot object
build_bar_base_plot <- function(df, legend_title) {

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
add_bar_labels <- function(p, df, stacked, label_size) {

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



#' Create a geom component function
#'
#' @param geom_layer A ggplot2 geom layer function
#' @param summarize_fn A function to summarize data
#' @param position_fn A function to create position adjustments
#' @param label_fn A function to add labels
#' @param supports_stack Logical, whether the geom supports stacking
#'
#' @export
geom_component <- function(
    geom_layer,
    summarize_fn = summarize_xy,
    position_fn = NULL,
    label_fn = NULL,
    supports_stack = FALSE
) {

  function(
    data,
    x,
    y,
    group = NULL,
    facet = NULL,
    stat = "mean",
    stacked = FALSE,
    horizontal = FALSE,
    title = NULL,
    x_label = NULL,
    y_label = NULL,
    legend_title = NULL,
    legend_position = "right",
    legend_direction = "vertical",
    note = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    show_labels = FALSE,
    label_size = 3.5
  ) {

    # ---- Capture once ----
    x_quo <- rlang::enquo(x)
    y_quo <- rlang::enquo(y)
    group_quo <- rlang::enquo(group)
    facet_quo <- rlang::enquo(facet)

    if (rlang::quo_is_null(x_quo)) rlang::abort("`x` is required")
    if (rlang::quo_is_null(y_quo)) rlang::abort("`y` is required")

    if (stacked && !supports_stack) {
      rlang::abort("This geom does not support stacking")
    }

    # ---- Evaluate ----
    df <- eval_plot_vars(
      data,
      x = x_quo,
      y = y_quo,
      group = group_quo,
      facet = facet_quo
    )

    # ---- Summarize ----
    plot_df <- summarize_fn(df, stat = stat)

    # ---- Position ----
    position <- if (!is.null(position_fn)) {
      position_fn(stacked, horizontal)
    } else {
      ggplot2::position_identity()
    }

    # ---- Base plot ----
    p <- build_base_plot(plot_df, legend_title) +
      geom_layer(position = position)

    # ---- Labels ----
    if (show_labels && !is.null(label_fn)) {
      p <- label_fn(p, plot_df, stacked, label_size)
    }

    # ---- Facets ----
    p <- add_facets(p, plot_df, facet_scales, facet_ncol)

    # ---- Theme ----
    p <- add_common_theme(
      p,
      title,
      x_label,
      y_label,
      note,
      legend_position,
      legend_direction
    )

    # ---- Legend cleanup ----
    if (!(".group" %in% names(plot_df))) {
      p <- p + ggplot2::guides(fill = "none")
    }

    # ---- Orientation ----
    if (horizontal) p <- p + ggplot2::coord_flip()

    p
  }
}






