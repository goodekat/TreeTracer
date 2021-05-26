
# get_subset_tree <- function(rf, k, max_depth) {
#
#   # Extract the tree from the random forest and add a node number
#   tree <- randomForest::getTree(rf, k = k, labelVar = TRUE) %>% janitor::clean_names()
#   tree$node <- 1:nrow(tree)
#
#   # Remove the tree leaves
#   splits <- tree %>% filter(.data$status != -1)
#
#   # Determine the tree depth of each split
#   splits$node_depth <- sapply(splits$node, function(i) get_node_depth(splits, i))
#
#   # Only keep the depths below the max
#   splits %>%
#     filter(node_depth <= max_depth) %>%
#     mutate(check_left = left_daughter %in% node)
#
# }

#
# trace_plot(
#   rf = penguin_rf,
#   train = penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
#   tree_ids = 1,
#   max_depth = 1
# )
#
# get_subset_tree(penguin_rf, 1, 1)
#
# max_depth = 1
#
# get_one_subset_tree_pred <- function(obs, rf, k, max_depth) {
#
#   # Extract the tree from the random forest and add a node number
#   tree <- get_subset_tree(rf, k, max_depth)
#
#   # Determine the prediction and node number associated with obs
#   depth_or_pred_df = 1
#   while (is.numeric(depth_or_pred_df)) {
#     depth_or_pred_df = get_next_depth(obs, tree, k, depth_or_pred_df)
#   }
#
#   # Return data frame with predictions
#   if (is.data.frame(depth_or_pred_df)) {
#     return(depth_or_pred_df)
#   } else {
#     stop("something is wrong...")
#   }
#
# }
#
# # Function for determining the next depth in the tree to move to
# get_next_depth <- function(obs, tree, k, current_depth) {
#   depth_data = tree[current_depth,]
#   if (depth_data$status == -1) {
#     data.frame(
#       tree_id = k,
#       leaf_number = depth_data$leaf_number,
#       prediction = depth_data$prediction
#     )
#   } else if (obs %>% pull(depth_data$split_var) <= depth_data$split_point) {
#     depth_data$left_daughter
#   } else {
#     depth_data$right_daughter
#   }
# }
