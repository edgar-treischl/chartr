# library(ggplot2)
# library(rlang)
# library(dplyr)
#
#
#
# # =======================
# # Pipe-friendly usage
# # =======================
#
# mtcars |>
#   bar_chart(
#     x = as.factor(cyl),
#     y = mpg,
#     group = as.factor(gear),
#     stacked = FALSE,
#     horizontal = FALSE,
#     show_labels = TRUE,
#     title = "Average MPG by Cylinder and Gear",
#     x_label = "Cylinders",
#     y_label = "Miles per Gallon",
#     legend_title = "Gears"
#   )
#
#
# mtcars |>
#   bar_chart(
#     x = as.factor(cyl),
#     y = mpg,
#     group = as.factor(gear),
#     stacked = FALSE,
#     horizontal = FALSE,
#     show_labels = FALSE
#   )
#
#
#
# mtcars |>
#   bar_chart(
#     x = as.factor(cyl),
#     y = mpg,
#     group = as.factor(gear),
#     stacked = TRUE,
#     horizontal = FALSE,
#     show_labels = TRUE
#   )
#
#
# # All the defaults
#
# library(ggplot2)
# library(dplyr)
#
# # Example using all parameters
# mtcars |>
#   bar_chart(
#     # Data
#     x = as.factor(cyl),
#     y = mpg,
#     # Grouping & Faceting
#     group = as.factor(gear),
#     facet = as.factor(am),
#     # Bar styling
#     stacked = TRUE,
#     horizontal = FALSE,
#     # Labels
#     show_labels = TRUE,
#     label_size = 3.5,
#     # Titles and captions
#     title = "Average MPG by Cylinder and Gear",
#     x_label = "Cylinders",
#     y_label = "Miles per Gallon",
#     legend_title = "Gears",
#     note = "Important note",
#     # Legend styling
#     legend_position = "right",
#     legend_direction = "vertical",
#     # Facet options
#     facet_scales = "fixed",   # fixed, free, free_x, free_y
#     facet_ncol = 2
#   )
#
#
#
