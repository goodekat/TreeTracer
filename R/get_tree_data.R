#' Extract and organize information from a random forest tree
#'
#' @export get_tree_data
#'
#' @importFrom dplyr %>% arrange filter left_join rename select
#' @importFrom janitor clean_names
#' @importFrom randomForest getTree
#' @importFrom rlang .data
#' @importFrom stats na.omit
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
#' penguin_rf <-
#'   randomForest::randomForest(
#'     species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'     data = penguins
#'   )
#'
#' # Extract tree data from the first tree in the random forest
#' get_tree_data(penguin_rf, 1)

get_tree_data <- function(rf, k) {

  # Extract the tree from the random forest and add a node number
  tree <- randomForest::getTree(rf, k = k, labelVar = TRUE) %>% janitor::clean_names()
  tree$node <- 1:nrow(tree)

  # Remove the tree leaves
  splits <- tree %>% filter(.data$status != -1)

  # Determine the tree depth of each split
  splits$node_depth <- sapply(splits$node, function(i) get_node_depth(splits, i))

  # If the tree only has one split, create the tree data differently
  if (dim(splits)[1] == 1) {
    with(
      splits,
      data.frame(
        tree = k,
        node_depth = node_depth,
        tree_branch = 1,
        split_var = split_var,
        split_point = split_point
      )
    )
  } else {
    # Determine child and parent nodes
    segments <-
      with(splits, rbind(
        data.frame(node_child = left_daughter,
                   node_parent = node),
        data.frame(node_child = right_daughter,
                   node_parent = node)
      ))

    # Add split variable, split point, and tree depth information for
    # both child and parent nodes
    segments <-
      segments %>%
      left_join(
        splits %>% select(.data$node, .data$split_var, .data$split_point, .data$node_depth),
        by = c("node_child" = "node")) %>%
      rename(
        split_var_child = .data$split_var,
        split_point_child = .data$split_point,
        node_depth_child = .data$node_depth
      ) %>%
      left_join(
        splits %>% select(.data$node, .data$split_var, .data$split_point, .data$node_depth),
        by = c("node_parent" = "node")) %>%
      rename(
        split_var_parent = .data$split_var,
        split_point_parent = .data$split_point,
        node_depth_parent = .data$node_depth
      ) %>%
      na.omit()

    # Number the branches
    segments$branch_num <- 1:nrow(segments)

    # Convert to a long data frame and add a tree number
    with(segments, data.frame(
      tree = k,
      node_depth = c(node_depth_child, node_depth_parent),
      tree_branch = c(branch_num, branch_num),
      split_var = c(as.character(split_var_child), as.character(split_var_parent)),
      split_point = c(split_point_child, split_point_parent)
    )) %>%
      arrange(.data$tree, .data$tree_branch, .data$node_depth) #%>%
      #filter(!is.na(.data$split_var))
  }

}
