library(testthat)
library(ggplot2)
library(dplyr)

# -----------------------------
# Prepare test data
# -----------------------------
data_bar <- mtcars %>%
  mutate(
    cyl = as.factor(cyl),
    gear = as.factor(gear),
    am = as.factor(am)
  )

# -----------------------------
# Define edge-case options
# -----------------------------
stacked_opts <- c(TRUE, FALSE)
horizontal_opts <- c(TRUE, FALSE)
group_opts <- list(NULL, quote(gear), quote(as.factor(gear)))
facet_opts <- list(NULL, quote(am), quote(as.factor(am)))
show_labels_opts <- c(TRUE, FALSE)

# Generate all combinations
edge_cases <- expand.grid(
  stacked = stacked_opts,
  horizontal = horizontal_opts,
  group = seq_along(group_opts),
  facet = seq_along(facet_opts),
  show_labels = show_labels_opts,
  stringsAsFactors = FALSE
)

# -----------------------------
# Run tests
# -----------------------------
for (i in seq_len(nrow(edge_cases))) {

  case <- edge_cases[i, ]

  stacked <- case$stacked
  horizontal <- case$horizontal
  group_expr <- group_opts[[case$group]]
  facet_expr <- facet_opts[[case$facet]]
  show_labels <- case$show_labels

  # Descriptive test name
  test_name <- paste(
    "bar_chart edge case", i,
    "stacked =", stacked,
    "horizontal =", horizontal,
    "group =", if (is.null(group_expr)) "NULL" else deparse(group_expr),
    "facet =", if (is.null(facet_expr)) "NULL" else deparse(facet_expr),
    "show_labels =", show_labels
  )

  test_that(test_name, {
    expect_silent(
      bar_chart(
        data = data_bar,
        x = cyl,
        y = mpg,
        group = if (!is.null(group_expr)) eval(group_expr, data_bar) else NULL,
        facet = if (!is.null(facet_expr)) eval(facet_expr, data_bar) else NULL,
        stacked = stacked,
        horizontal = horizontal,
        show_labels = show_labels,
        title = "Bar Chart Stress Test"
      )
    )
  })
}
