#' Build base line plot
#'
#' @param df Data frame
#' @param x_quo X aesthetic
#' @param y_quo Y aesthetic
#' @param group_quo Group aesthetic
#' @param line_size Line size
#'
#' @returns A ggplot object
line_build_base_plot <- function(df, x_quo, y_quo, group_quo, line_size) {

  aes_mapping <- if (rlang::quo_is_null(group_quo)) {
    ggplot2::aes(
      x = !!x_quo,
      y = !!y_quo,
      group = 1
    )
  } else {
    ggplot2::aes(
      x = !!x_quo,
      y = !!y_quo,
      group = !!group_quo,
      color = !!group_quo
    )
  }

  ggplot2::ggplot(df, aes_mapping) +
    ggplot2::geom_line(linewidth = line_size)
}


#' Add points
#'
#' @param p A ggplot object
#' @param df Data frame
#' @param group_quo Group aesthetic
#' @param point_shape Point shape
#' @param point_size Point size
#'
#' @returns A ggplot object
line_add_points <- function(p, df, group_quo, point_shape, point_size) {
  if (!is.null(point_shape) && !rlang::quo_is_null(group_quo)) {
    p + ggplot2::geom_point(ggplot2::aes(shape = !!group_quo), size = point_size) +
      ggplot2::scale_shape_manual(values = point_shape)
  } else {
    p + ggplot2::geom_point(size = point_size)
  }
}


#' Add last point labels
#'
#' @param p A ggplot object
#' @param df Data frame
#' @param x_quo X aesthetic
#' @param group_quo Group aesthetic
#' @param facet_quo Facet aesthetic
#' @param label_size Label size
#'
#' @returns A ggplot object
line_add_last_point_labels <- function(p, df, x_quo, group_quo, facet_quo, label_size) {
  last_points <- if (!rlang::quo_is_null(facet_quo)) {
    df %>%
      dplyr::group_by(!!group_quo, !!facet_quo) %>%
      dplyr::slice_max(!!x_quo, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
  } else {
    df %>%
      dplyr::group_by(!!group_quo) %>%
      dplyr::slice_max(!!x_quo, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
  }

  p + ggrepel::geom_text_repel(
    data = last_points,
    ggplot2::aes(label = !!group_quo),
    size = label_size,
    nudge_x = 0.2,
    show.legend = FALSE
  )
}
