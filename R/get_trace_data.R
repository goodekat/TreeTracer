#' Convert tree information from a random forest to the structure for a trace plot
#'
#' @export get_trace_data
#'
#' @importFrom dplyr %>% everything group_by left_join mutate summarise
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @param tree_data data.frame obtained using get_tree_data
#' @param train features used to train the random forest which the tree is from
#' @param width specifies the width of the horizontal feature lines in a trace plot
#'              (a number between 0 and 1; default is 0.8)
#'
#' @examples
#'
#' # Fit a random forest using the iris data
#' set.seed(71)
#' iris.rf <- randomForest::randomForest(Species ~ ., data = iris)
#'
#' # Extract tree data from the first tree in the random forest
#' tree_data <- get_tree_data(iris.rf, 1)
#'
#' # Obtain the trace data for the first tree in the random forest
#' get_trace_data(tree_data, iris[,-5])

get_trace_data <- function(tree_data, train, width = 0.8) {

  # tree_data: output from get_tree_data function
  # train: data frame with the variables used to train the model
  # width: value between 0 and 1 that determines the width of variable segments

  # Determine the number of levels and variables used for splitting
  # in the tree(s)
  n_levels = length(unique(tree_data$tree_level))
  n_vars = length(unique(tree_data$split_var))

  # Create the variable segments for the trace plot (includes all
  # possible tree levels and variables but may be reduced based on
  # those actually used by the tree(s))
  trace_grid <-
    data.frame(
      tree_level = unique(tree_data$tree_level),
      split_var = rep(unique(tree_data$split_var), each = n_levels),
      seg_xmid = rep(1:n_vars, each = n_levels),
      seg_xmin = rep(1:n_vars + (width / 2), each = n_levels),
      seg_xmax = rep(1:n_vars - (width / 2), each = n_levels)
    )

  # Compute the maximum and minimum var values from the
  # training data
  split_var_max_min <-
    train %>%
    pivot_longer(names_to = "split_var", cols = everything()) %>%
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
