#' Extract and organize information from a random forest tree
#'
#' @export get_tree_data
#'
#' @importFrom dplyr %>% arrange filter left_join rename select
#' @importFrom janitor clean_names
#' @importFrom randomForest getTree
#' @importFrom rlang .data
#'
#' @param rf random forest model fit using randomForest
#' @param k number identifying the tree in the random forest from which
#'          to extract information
#'
#' @examples
#'
#' # Load the Palmer penguins data
#' library(palmerpenguins)
#' penguins <- na.omit(penguins)
#'
#' # Fit a random forest
#' set.seed(71)
#' penguin.rf <-
#'   randomForest::randomForest(
#'     species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'     data = penguins
#'   )
#'
#' # Extract tree data from the first tree in the random forest
#' get_tree_data(penguin.rf, 1)

get_tree_data <- function(rf, k) {

  # Extract the tree from the random forest and add a parent number
  tree <- randomForest::getTree(rf, k = k, labelVar = TRUE) %>% janitor::clean_names()
  tree$parent <- 1:nrow(tree)

  # Remove the tree leaves
  splits <- tree %>% filter(.data$status != -1)

  # Determine the tree level of each split
  splits$tree_level <- sapply(splits$parent, function(i) get_tree_level(splits, i))

  # Determine child and parent nodes
  segments <-
    rbind(
      data.frame(
        node_child = splits$left_daughter,
        node_parent = splits$parent
      ),
      data.frame(
        node_child = splits$right_daughter,
        node_parent = splits$parent
      )
    )

  # Add split variable, split point, and tree level information for
  # both child and parent nodes
  segments <-
    segments %>%
    left_join(
      splits %>% select(.data$parent, .data$split_var, .data$split_point, .data$tree_level),
      by = c("node_child" = "parent")) %>%
    rename(
      split_var_child = .data$split_var,
      split_point_child = .data$split_point,
      tree_level_child = .data$tree_level
    ) %>%
    left_join(
      splits %>% select(.data$parent, .data$split_var, .data$split_point, .data$tree_level),
      by = c("node_parent" = "parent")) %>%
    rename(
      split_var_parent = .data$split_var,
      split_point_parent = .data$split_point,
      tree_level_parent = .data$tree_level
    )

  # Number the branches
  segments$branch_num <- 1:nrow(segments)

  # Convert to a long data frame and add a tree number
  with(segments, data.frame(
    tree = k,
    tree_level = c(tree_level_child, tree_level_parent),
    tree_branch = c(branch_num, branch_num),
    split_var = c(as.character(split_var_child), as.character(split_var_parent)),
    split_point = c(split_point_child, split_point_parent)
  )) %>%
    arrange(.data$tree, .data$tree_branch, .data$tree_level) %>%
    filter(!is.na(.data$split_var))

}
