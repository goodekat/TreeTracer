#' Compute fit metric between two trees
#'
#' The function computes the fit metric from \insertCite{chipman:1998;textual}{TreeTracer}
#' between two trees (classification or regression).
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
#' penguin.rf <-
#'   randomForest::randomForest(
#'     species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'     data = penguins,
#'     ntree = 10
#'   )
#'
#' # Compute fit metrics between all trees
#' compute_fit_metric(penguin.rf, penguins)

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



