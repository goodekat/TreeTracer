#' Get a prediction using a specified tree from a random forest
#'
#' @export get_all_tree_preds
#'
#' @param data data frame with feature values for observations to obtain predictions
#' @param rf random forest model fit using randomForest
#'
#' @examples
#'
#' # Load the Palmer penguins data
#' library(palmerpenguins)
#' penguins <- na.omit(penguins)
#'
#' # Fit a random forest
#' set.seed(71)
#' penguin_rf <-
#'   randomForest::randomForest(
#'     species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'     data = penguins
#'   )
#'
#' # Extract tree data from the first tree in the random forest
#' get_tree_pred(penguins[1,], penguin_rf, 1)

get_all_tree_preds <- function(data, rf) {

  # Determine the number of observations and trees
  nobs = dim(data)[1]
  ntrees = rf$ntree

  # Compute and join individual tree prediction for
  # each tree in the random forest and given observation
  purrr::map2_df(
    .x = rep(1:nobs, ntrees),
    .y = rep(1:ntrees, each = nobs),
    .f = function(obs, tree) {
      get_tree_pred(data[obs, ], rf, tree) %>%
        mutate(obs_id = obs, tree_id = tree) %>%
        select(.data$obs_id, .data$tree_id, everything())
    }
  )

}
