#' @export get_tree_data
#'
#' @importFrom dplyr %>% filter left_join rename select
#' @importFrom janitor clean_names
#' @importFrom randomForest getTree

get_tree_data <- function(rf, k) {

  # rf: randomforest
  # k: index

  # Extract the tree from the random forest and add a parent number
  tree <- randomForest::getTree(rf, k = k, labelVar = TRUE) %>% janitor::clean_names()
  tree$parent <- 1:nrow(tree)

  # Remove the tree leaves
  splits <- tree %>% filter(status != -1)

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
      splits %>% select(parent, split_var, split_point, tree_level),
      by = c("node_child" = "parent")) %>%
    rename(
      split_var_child = split_var,
      split_point_child = split_point,
      tree_level_child = tree_level
    ) %>%
    left_join(
      splits %>% select(parent, split_var, split_point, tree_level),
      by = c("node_parent" = "parent")) %>%
    rename(
      split_var_parent = split_var,
      split_point_parent = split_point,
      tree_level_parent = tree_level
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
    arrange(tree, tree_branch, tree_level) %>%
    filter(!is.na(split_var))

}
