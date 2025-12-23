# library(ggplot2)
# library(dplyr)
# library(magrittr)
#
# # Assuming bar_chart() is already defined
#
# # -----------------------------
# # Define options for testing
# # -----------------------------
# stacked_opts <- c(TRUE, FALSE)
# horizontal_opts <- c(TRUE, FALSE)
# group_opts <- list(NULL, quote(gear), quote(as.factor(gear)))
# facet_opts <- list(NULL, quote(am), quote(as.factor(am)))
# show_labels_opts <- c(TRUE, FALSE)
#
# # Generate all combinations
# edge_cases <- expand.grid(
#   stacked = stacked_opts,
#   horizontal = horizontal_opts,
#   group = seq_along(group_opts),
#   facet = seq_along(facet_opts),
#   show_labels = show_labels_opts,
#   stringsAsFactors = FALSE
# )
#
# # Initialize results list
# results <- vector("list", nrow(edge_cases))
#
# # -----------------------------
# # Run tests
# # -----------------------------
# for (i in seq_len(nrow(edge_cases))) {
#   case <- edge_cases[i, ]
#
#   stacked <- case$stacked
#   horizontal <- case$horizontal
#   group_expr <- group_opts[[case$group]]
#   facet_expr <- facet_opts[[case$facet]]
#   show_labels <- case$show_labels
#
#   # Label for this test
#   test_label <- paste(
#     "Test", i,
#     "stacked =", stacked,
#     "horizontal =", horizontal,
#     "group =", if(is.null(group_expr)) "NULL" else deparse(group_expr),
#     "facet =", if(is.null(facet_expr)) "NULL" else deparse(facet_expr),
#     "show_labels =", show_labels
#   )
#
#   # Run the test and catch errors
#   res <- tryCatch({
#     p <- bar_chart(
#       data = mtcars,
#       x = as.factor(cyl),
#       y = mpg,
#       group = if (!is.null(group_expr)) eval(group_expr, mtcars) else NULL,
#       facet = if (!is.null(facet_expr)) eval(facet_expr, mtcars) else NULL,
#       stacked = stacked,
#       horizontal = horizontal,
#       show_labels = show_labels,
#       title = "Test Plot"
#     )
#     # Optional: print(p)  # Comment out if too many plots
#     list(success = TRUE, error = NA)
#   }, error = function(e) {
#     list(success = FALSE, error = e$message)
#   })
#
#   results[[i]] <- c(test_label, res)
# }
#
# # -----------------------------
# # Summarize results
# # -----------------------------
# summary_df <- do.call(rbind, lapply(results, function(x) {
#   data.frame(
#     test = x[[1]],
#     success = x$success,
#     error = ifelse(is.na(x$error), "", x$error),
#     stringsAsFactors = FALSE
#   )
# }))
#
# # Print summary of failed tests only
# failed <- summary_df %>% filter(!success)
# if (nrow(failed) == 0) {
#   cat("✅ All tests passed!\n")
# } else {
#   cat("❌ Some tests failed:\n")
#   print(failed)
# }
