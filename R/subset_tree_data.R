
# get_subset_tree <- function(rf, k, max_depth) {
#
#   # Extract the tree from the random forest and add a node number
#   tree <- randomForest::getTree(rf, k = k, labelVar = TRUE) %>% janitor::clean_names()
#   tree$node <- 1:nrow(tree)
#
#   # Remove the tree leaves
#   splits <- tree %>% filter(.data$status != -1)
#
#   # Determine the tree level of each split
#   splits$tree_level <- sapply(splits$node, function(i) get_tree_level(splits, i))
#
#   # Add the tree level to the
#   splits %>%
#     filter(tree_level <= max_depth) %>%
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
#   level_or_pred_df = 1
#   while (is.numeric(level_or_pred_df)) {
#     level_or_pred_df = get_next_level(obs, tree, k, level_or_pred_df)
#   }
#
#   # Return data frame with predictions
#   if (is.data.frame(level_or_pred_df)) {
#     return(level_or_pred_df)
#   } else {
#     stop("something is wrong...")
#   }
#
# }
#
# # Function for determining the next level in the tree to move to
# get_next_level <- function(obs, tree, k, current_level) {
#   level_data = tree[current_level,]
#   if (level_data$status == -1) {
#     data.frame(
#       tree_id = k,
#       leaf_number = level_data$leaf_number,
#       prediction = level_data$prediction
#     )
#   } else if (obs %>% pull(level_data$split_var) <= level_data$split_point) {
#     level_data$left_daughter
#   } else {
#     level_data$right_daughter
#   }
# }
