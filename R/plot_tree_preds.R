#' Scatter plot matrix of predictions from two trees showing where they disagree
#'
#' Given two tree IDs, the function creates a scatter plot matrix of the
#' predictor variables with the points colored by whether the predictions
#' from the two tree agree.
#'
#' @export plot_tree_preds
#'
#' @importFrom dplyr %>% bind_cols mutate rename_all select
#' @importFrom GGally ggpairs
#' @importFrom ggplot2 aes geom_point ggplot
#' @importFrom stringr str_replace
#'
#' @param rf randomForest object from which to compute similarities between trees
#' @param data data frame with predictor variables used to fit the model (does
#'        not need to be the training data)
#' @param t1 tree ID for first tree in comparison
#' @param t2 tree ID for second tree in comparison

plot_tree_preds <- function(rf, data, t1, t2) {

  # Return predictions from all trees in the forest
  all_pred <- randomForest:::predict.randomForest(rf, data, predict.all = TRUE)

  plot_data <-
    data %>%
    bind_cols(
      data.frame(all_pred$individual[, c(t1, t2)]) %>% rename_all(
        .funs = function(x)
          stringr::str_replace(x, "X", "t")
      )
    ) %>%
    mutate(agree = t1 == t2) %>%
    mutate(t1 = factor(t1), t2 = factor(t2)) %>%
    select(agree, everything())

  GGally::ggpairs(data, ggplot2::aes(color = plot_data$agree)) +
    labs(x = paste("Tree", t1), y = paste("Tree", t2))

}

