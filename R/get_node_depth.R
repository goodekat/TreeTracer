#' Determines the depth of each node in a tree
#'
#' @param df data frame of tree information generated with get_tree_data
#' @param node node number

get_node_depth <- function(df, node) {
  # very recursive
  if (node == 1) return(1)
  idx <- which(df$left_daughter == node | df$right_daughter == node)
  1 + get_node_depth(df, df$node[idx])
}
