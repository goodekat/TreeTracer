#' Convert tree information from a random forest to the structure for a trace plot
#'
#' @export get_trace_data
#'
#' @importFrom dplyr %>% everything group_by left_join mutate summarise
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @param rf random forest model
#' @param tree_data data.frame obtained using get_tree_data
#' @param train features used to train the random forest which the tree is from
#' @param width specifies the width of the horizontal feature lines in a trace plot
#'              (a number between 0 and 1; default is 0.8)
#' @param split_var_order order of the split variables on the x-axis (left to right) specified
#'              either manually as a vector of variable names or as "rf_vi" to indicate that
#'              the variables should be ordered by random forest variable importance
#'              (default is "rf_vi")
#'
#' @examples
#'
#' # Load packages
#' library(dplyr)
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
#'     data = penguins
#'   )
#'
#' # Extract tree data from the first tree in the random forest
#' tree_data <- get_tree_data(penguin_rf, 1)
#'
#' # Obtain the trace data for the first tree in the random forest
#' get_trace_data(
#'   tree_data = tree_data,
#'   rf = penguin_rf,
#'   train = penguins %>%
#'     select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
#'   )

get_trace_data <- function(tree_data, rf, train, width = 0.8, split_var_order = "rf_vi") {

  # tree_data: output from get_tree_data function
  # train: data frame with the variables used to train the model
  # width: value between 0 and 1 that determines the width of variable segments

  # Get the order of feature importance
  if ("rf_vi" %in% split_var_order) {
    if (rf$type == "classification") {
      feat_import <-
        rf$importance %>%
        data.frame() %>%
        arrange(desc(.data$MeanDecreaseGini)) %>%
        rownames()
    } else if (rf$type == "regression") {
      feat_import <-
        rf$importance %>%
        data.frame() %>%
        arrange(desc(.data$IncNodePurity)) %>%
        rownames()
    }
  }

  # Specify the split variables
  if ("rf_vi" %in% split_var_order) {
    split_vars = feat_import
  } else {
    split_vars = split_var_order
  }

  # Determine the number of levels and variables used for splitting
  # in the tree(s)
  n_levels = length(unique(tree_data$tree_level))
  n_vars = length(split_vars)

  # Create the variable segments for the trace plot (includes all
  # possible tree levels and variables but may be reduced based on
  # those actually used by the tree(s))
  trace_grid <-
    data.frame(
      tree_level = unique(tree_data$tree_level),
      split_var = rep(split_vars, each = n_levels),
      seg_xmid = rep(1:n_vars, each = n_levels),
      seg_xmin = rep(1:n_vars + (width / 2), each = n_levels),
      seg_xmax = rep(1:n_vars - (width / 2), each = n_levels)
    )

  # Compute the maximum and minimum var values from the
  # training data
  split_var_max_min <-
    train %>%
    tidyr::pivot_longer(names_to = "split_var", cols = everything()) %>%
    group_by(.data$split_var) %>%
    summarise(
      split_var_max = max(.data$value),
      split_var_min = min(.data$value),
      .groups = "drop"
    )

  # Create the data to be used by the trace plot by joining the
  # corresponding rows of the trace_grid with the tree_data and
  # scaling the variable split points to the trace grid
  trace_data <-
    left_join(x = tree_data,
              y = trace_grid,
              by = c("tree_level", "split_var")) %>%
    left_join(y = split_var_max_min, by = "split_var") %>%
    group_by(.data$split_var) %>%
    mutate(n_splits = length(unique(.data$split_point))) %>%
    mutate(split_scaled = ifelse(
      .data$n_splits == 1,
      (.data$seg_xmax + .data$seg_xmin) / 2,
      (.data$seg_xmax - .data$seg_xmin) / (.data$split_var_max - .data$split_var_min) *
        (.data$split_point - .data$split_var_max) + .data$seg_xmax
    ))

  # Return the data to be used in the trace plot
  return(trace_data)

}
