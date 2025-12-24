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





