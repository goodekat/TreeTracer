#' Computes fit metric between two trees
#'
#' Function for computing the fit metric from \insertCite{chipman:1998;textual}{TreeTracer}.
#'
#' @references{
#'   \insertRef{chipman:1998}{TreeTracer}
#' }
#'
#' @export compute_fit_metric
#'
#' @importFrom utils combn
#'
#' @param rf randomForest object from which to compute similarities between trees
#' @param data data frame with predictor variables used to fit the model (does
#'        not need to be the training data)
#'
#' @examples
#'
#' # Load packages
#' library(palmerpenguins)
#'
#' # Load the Palmer penguins data
#' penguins <- na.omit(penguins)
#'
#' # Fit a random forest
#' set.seed(71)
#' penguin_rf <-
#'   randomForest::randomForest(
#'     species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'     data = penguins,
#'     ntree = 10
#'   )
#'
#' # Compute fit metrics between all trees
#' compute_fit_metric(penguin_rf, penguins)

compute_fit_metric <- function(rf, data) {

  # Return predictions from all trees in the forest
  all_pred <- randomForest:::predict.randomForest(rf, data, predict.all = TRUE)

  # Create a matrix with all pairs of trees
  tree_pairs = combn(1:rf$ntree, 2)

  # Compute the Chipman, George, and McCulloch (1998) fit metric for all pairs of trees
  purrr::map_df(
    .x = 1:dim(tree_pairs)[2],
    .f = function(index) {
      t1 = tree_pairs[1,index]
      t2 = tree_pairs[2,index]
      if (rf$type == "classification") {
        data.frame(t1 = t1, t2 = t2, similarity = fit_metric_class(all_pred, t1, t2))
      } else if (rf$type == "regression") {
        data.frame(t1 = t1, t2 = t2, similarity = fit_metric_reg(all_pred, t1, t2))
      }
    }
  )

}

fit_metric_class <- function(all_pred, t1, t2) {
  mean(all_pred$individual[, t1] == all_pred$individual[, t2])
}

fit_metric_reg <- function(all_pred, t1, t2) {
  mean((all_pred$individual[, t1] - all_pred$individual[, t2])^2)
}

#' Computes metric comparing covariates from two trees
#'
#' Function for computing the fit metric from \insertCite{banerjee:2012;textual}{TreeTracer}.
#'
#' @references{
#'   \insertRef{banerjee:2012}{TreeTracer}
#' }
#'
#' @export compute_covariate_metric
#'
#' @importFrom dplyr %>% filter mutate_all
#' @importFrom purrr map_df
#' @importFrom tidyr pivot_wider
#' @importFrom utils combn
#'
#' @param rf randomForest object from which to compute similarities between trees
#' @param max_depth an option to set the maximum tree depth to consider when
#'        comparing trees (set to NULL by default)
#' @examples
#'
#' # Load packages
#' library(palmerpenguins)
#'
#' # Load the Palmer penguins data
#' penguins <- na.omit(penguins)
#'
#' # Fit a random forest
#' set.seed(71)
#' penguin_rf <-
#'   randomForest::randomForest(
#'     species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'     data = penguins,
#'     ntree = 5
#'   )
#'
#' # Compute fit metrics between all trees
#' compute_covariate_metric(penguin_rf)

compute_covariate_metric <- function(rf, max_depth = NULL) {

  # Determine the number of covariates in the random forest
  k = length(rf$forest$xlevels)

  # Get tree data for all trees in the RF
  all_trees_df = purrr::map_df(
    .x = 1:rf$ntree,
    .f = function(t) {
      get_tree_data(rf = rf, k = t) %>%
        filter(.data$tree_level <= ifelse(is.null(max_depth), max(.data$tree_level), max_depth)) %>%
        select(.data$tree, .data$split_var) %>%
        distinct()
    }
  )

  # Create a data frame of indicators for whether a variable is used in a tree or not
  var_indicators <-
    all_trees_df %>%
    mutate(ind = 1) %>%
    pivot_wider(names_from = .data$split_var, values_from = .data$ind) %>%
    mutate_all(.funs = function(x) ifelse(is.na(x), 0, x))

  # Create a matrix with all pairs of trees
  tree_pairs = combn(1:rf$ntree, 2)

  # Compute metric d0 from Banerjee, Ding, and Noone
  purrr::map_df(
    .x = 1:dim(tree_pairs)[2],
    .f = function(index) {
      t1 = tree_pairs[1, index]
      t2 = tree_pairs[2, index]
      data.frame(
        t1 = t1,
        t2 = t2,
        similarity = sum(var_indicators[t1, -1] == var_indicators[t2, -1]) / k
      )
    }
  )

}
