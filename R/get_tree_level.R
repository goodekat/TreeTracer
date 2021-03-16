#' Determines the level of each node in a tree
#'
#' @param df data frame of tree information generated with get_tree_data
#' @param node node number

get_tree_level <- function(df, node) {
  # very recursive
  if (node == 1) return(1)
  idx <- which(df$left_daughter == node | df$right_daughter == node)
  1 + get_tree_level(df, df$node[idx])
}
