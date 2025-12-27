library(testthat)
library(ggplot2)
library(dplyr)

data_line <- mtcars %>%
  mutate(
    cyl = as.factor(cyl),
    gear = as.factor(gear),
    am = as.factor(am)
  )

group_opts <- list(NULL, quote(gear), quote(as.factor(gear)))
facet_opts <- list(NULL, quote(am), quote(as.factor(am)))
show_points_opts <- c(TRUE, FALSE)
show_labels_opts <- c(TRUE, FALSE)
point_shape_opts <- list(NULL, c(16, 17, 15))

edge_cases <- expand.grid(
  group = seq_along(group_opts),
  facet = seq_along(facet_opts),
  show_points = show_points_opts,
  show_labels = show_labels_opts,
  point_shape = seq_along(point_shape_opts),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(edge_cases))) {

  case <- edge_cases[i, ]

  group_expr <- group_opts[[case$group]]
  facet_expr <- facet_opts[[case$facet]]
  show_points <- case$show_points
  show_labels <- case$show_labels
  point_shape <- point_shape_opts[[case$point_shape]]

  # Create a descriptive test name
  test_name <- paste(
    "line_chart edge case", i,
    "group =", if (is.null(group_expr)) "NULL" else deparse(group_expr),
    "facet =", if (is.null(facet_expr)) "NULL" else deparse(facet_expr),
    "show_points =", show_points,
    "show_labels =", show_labels,
    "point_shape =", if (is.null(point_shape)) "NULL" else "custom"
  )

  # Wrap each edge case in its own test_that()
  test_that(test_name, {
    expect_silent(
      line_chart(
        data = data_line,
        x = wt,
        y = mpg,
        group = if (!is.null(group_expr)) eval(group_expr, data_line) else NULL,
        facet = if (!is.null(facet_expr)) eval(facet_expr, data_line) else NULL,
        show_points = show_points,
        show_labels = show_labels,
        point_shape = point_shape,
        title = "Line Chart Stress Test"
      )
    )
  })
}
