#' Get predictions from the individual trees in a random forest
#'
#' @export get_tree_preds
#'
#' @importFrom furrr future_map2_dfr
#' @importFrom future availableCores multisession plan
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
#'     data = penguins,
#'     ntree = 10
#'   )
#'
#' # Extract tree data corresponding to the first five
#' # observations in the data
#' get_tree_preds(penguins[1,], penguin_rf)

get_tree_preds <- function(data, rf) {

  # Determine the number of observations and trees
  nobs = dim(data)[1]
  ntrees = rf$ntree

  # Compute and join individual tree prediction for
  # each tree in the random forest and given observation
  no_cores <- future::availableCores() - 1
  future::plan(future::multisession, workers = no_cores)
  furrr::future_map2_dfr(
    .x = rep(1:nobs, ntrees),
    .y = rep(1:ntrees, each = nobs),
    .f = function(obs, tree) {
      get_one_pred(data[obs, ], rf, tree) %>%
        mutate(obs_id = obs, tree_id = tree) %>%
        select(.data$obs_id, .data$tree_id, everything())
    }
  )

}

# Function for getting a prediction given one tree and one observation
get_one_pred <- function(obs, rf, k) {

  # Extract the tree from the random forest and add a node number
  tree <- randomForest::getTree(rf, k = k, labelVar = TRUE) %>% janitor::clean_names()
  tree$node <- 1:nrow(tree)

  # Add leaf numbers
  tree = tree %>%
    left_join(
      tree %>%
        filter(.data$status == -1) %>%
        select(.data$status, .data$node) %>%
        mutate(leaf_number = 1:n()),
      by = c("status", "node")
    )

  # Determine the prediction and node number associated with obs
  depth_or_pred_df = 1
  while (is.numeric(depth_or_pred_df)) {
    depth_or_pred_df = get_next_depth(obs, tree, k, depth_or_pred_df)
  }

  # Return data frame with predictions
  if (is.data.frame(depth_or_pred_df)) {
    return(depth_or_pred_df)
  } else {
    stop("something is wrong...")
  }

}

# Function for determining the next depth in the tree to move to
get_next_depth <- function(obs, tree, k, current_depth) {
  depth_data = tree[current_depth,]
  if (depth_data$status == -1) {
    data.frame(
      tree_id = k,
      leaf_number = depth_data$leaf_number,
      prediction = depth_data$prediction
    )
  } else if (obs %>% pull(depth_data$split_var) <= depth_data$split_point) {
    depth_data$left_daughter
  } else {
    depth_data$right_daughter
  }
}
