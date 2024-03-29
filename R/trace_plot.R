#' Create a trace plot of trees from a random forest
#'
#' Trace plots are useful tools for visually comparing trees from a random
#' forest. This functions creates a trace plot given a set of trees from a
#' random forest fit using the randomForest package. For more information on
#' trace plots, see \insertCite{urbanek:2008;textual}{TreeTracer}.
#'
#' @references{
#'   \insertRef{urbanek:2008}{TreeTracer}
#' }
#'
#' @export trace_plot
#'
#' @importFrom dplyr %>% arrange desc distinct select
#' @importFrom ggplot2 aes element_blank facet_wrap ggplot geom_line geom_point geom_segment geom_text labs scale_x_continuous scale_x_discrete theme
#' @importFrom purrr map_df
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#'
#' @param rf random forest model fit using randomForest
#' @param train features used to train the random forest which the tree is from
#' @param tree_ids vector of numbers specifying the trees to include in the trace plot
#' @param width specifies the width of the horizontal feature lines in a trace plot
#'              (a number between 0 and 1; default is 0.8)
#' @param alpha alpha value for the lines in the trace plot (a number between 0
#'              and 1; default is 0.5)
#' @param tree_color color of the traces (default is "black")
#' @param color_by_id should the trace lines be colored by the tree IDs? (default if FALSE)
#' @param facet_by_id should the traces be faceted by tree IDs? (default if FALSE)
#' @param id_order order trees should be arranged by if facet_by_id is TRUE (optional)
#' @param split_var_order order of the split variables on the x-axis (left to right) specified
#'              either manually as a vector of variable names or as "rf_vi" to indicate that
#'              the variables should be ordered by random forest variable importance
#'              (default is "rf_vi")
#' @param cont_var continuous variable associated with the trees which can be used to
#'              color them (must be in the same order as tree_ids) (optional)
#' @param nrow number of rows if facet_by_id is TRUE (otherwise ignored)
#' @param max_depth the deepest depth to include in the trace plot (set to NULl by default)
#' @param rep_tree option to add a "representative tree" on top of the trace plot by providing
#'              a data frame with the structure of the get_tree_data function (NULL by default)
#' @param rep_tree_size line size of "representative tree" (1 by default)
#' @param rep_tree_color line color of "representative tree" ("blue" by default)
#' @param rep_tree_alpha line alpha of "representative tree" (1 by default)
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
#' # Generate a trace plot of the first 10 trees in the forest
#' trace_plot(
#'  rf = penguin_rf,
#'  train = penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
#'  tree_ids = 1:10
#' )

trace_plot <- function(rf,
                       train,
                       tree_ids,
                       width = 0.8,
                       alpha = 0.5,
                       tree_color = "black",
                       color_by_id = FALSE,
                       facet_by_id = FALSE,
                       id_order = NULL,
                       split_var_order = "rf_vi",
                       cont_var = NULL,
                       nrow = NULL,
                       max_depth = NULL,
                       rep_tree = NULL,
                       rep_tree_size = 1,
                       rep_tree_color = "blue",
                       rep_tree_alpha = 1) {

  # Obtain the trace data from the specified trees
  if (is.null(rep_tree)) {
    trace_data <-
      purrr::map_df(
        .x = tree_ids,
        .f = function(id)
          get_tree_data(rf = rf , k = id)
      ) %>%
      get_trace_data(rf = rf, train = train, width = width, split_var_order = split_var_order)
  } else {
    trace_data <-
      purrr::map_df(
        .x = tree_ids,
        .f = function(id)
          get_tree_data(rf = rf , k = id)
      ) %>%
      mutate(tree = as.character(.data$tree)) %>%
      bind_rows(rep_tree %>% mutate(tree = as.character("rep"))) %>%
      get_trace_data(rf = rf, train = train, width = width, split_var_order = split_var_order)
  }

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

  # Extract the depths that correspond to a tree
  trees = sort(unique(trace_data$tree))
  tree_branches = sort(unique(trace_data$tree_branch))
  node_depths = sort(unique(trace_data$node_depth), decreasing = TRUE)
  if ("rf_vi" %in% split_var_order) {
    split_vars = feat_import
  } else {
    split_vars = split_var_order
  }

  # Keep only a subset of tree depths if requested
  if (!is.null(max_depth)) {
    trace_data <- trace_data %>% filter(.data$node_depth <= max_depth)
  }

  # Convert categorical variables to factors
  trace_data <-
    trace_data %>%
    mutate(
      tree = factor(.data$tree, levels = trees),
      tree_branch = factor(.data$tree_branch, levels = tree_branches),
      node_depth = factor(.data$node_depth, levels = node_depths),
      split_var = factor(.data$split_var, levels = split_vars)
    )

  # Extract the split variables to use as labels in the trace plot
  trace_labels <-
    trace_data %>%
    select(.data$node_depth, .data$split_var, .data$seg_xmid) %>%
    distinct()

  # Order the tree if specified
  if (!is.null(id_order)) {
    trace_data <-
      trace_data %>%
      mutate(tree = factor(.data$tree, levels = id_order))
  }

  # Attach the continuous variable to the tree data frame
  if (!is.null(cont_var)) {
    if (is.null(id_order)) {id_order = trees} else {id_order = id_order}
    cont_var_df <- data.frame(cont_var = cont_var) %>% mutate(tree = factor(tree_ids, levels = id_order))
    trace_data <- trace_data %>% left_join(cont_var_df, by = "tree")
  }

  # Create a trace plot
  trace_plot <-
    ggplot(trace_data) +
    geom_segment(
      mapping = aes(
        x = .data$seg_xmin,
        xend = .data$seg_xmax,
        y = .data$node_depth,
        yend = .data$node_depth
      )
    ) +
    scale_x_continuous(breaks = 1:length(split_vars), labels = split_vars, limits = c(0.5,length(split_vars) + .5))

  # Add color to the plot
  if (color_by_id == TRUE) {
    trace_plot <-
      trace_plot +
      geom_line(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$node_depth,
          group = .data$tree:.data$tree_branch,
          color = .data$tree
        ),
        alpha = alpha
      ) +
      geom_point(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$node_depth,
          color = .data$tree
        ),
        shape = 124
      ) +
      labs(color = "Tree ID")
  } else if (!is.null(cont_var)) {
    trace_plot <-
      trace_plot +
      geom_line(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$node_depth,
          group = .data$tree:.data$tree_branch,
          color = .data$cont_var
        ),
        alpha = alpha
      ) +
      geom_point(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$node_depth,
          color = .data$cont_var
        ),
        shape = 124
      ) +
      labs(color = "Added Variable \n(adjust as needed)")
  } else {
    trace_plot <-
      trace_plot +
      geom_line(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$node_depth,
          group = factor(.data$tree):factor(.data$tree_branch)
        ),
        alpha = alpha,
        color = tree_color
      ) +
      geom_point(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(x = .data$split_scaled, y = .data$node_depth),
        shape = 124,
        color = tree_color)
  }

  # Add representative tree (if given)
  if (!is.null(rep_tree)) {
    trace_plot <-
      trace_plot +
      geom_line(
        data = trace_data %>% filter(.data$tree == "rep"),
        aes(
          x = .data$split_scaled,
          y = .data$node_depth,
          group = .data$tree_branch
        ),
        size = rep_tree_size,
        color = rep_tree_color,
        alpha = rep_tree_alpha
      ) +
      geom_point(
        data = trace_data %>% filter(.data$tree == "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$node_depth,
          color = .data$tree
        ),
        size = rep_tree_size * 2,
        color = rep_tree_color,
        shape = 124
      )
  }

  # Facet by tree ID if requested
  if (facet_by_id == TRUE) {
    trace_plot <- trace_plot + facet_wrap(. ~ .data$tree, nrow = nrow)
  }

  # Format trace plot
  trace_plot +
    #scale_x_discrete(drop = FALSE) +
    labs(
      x = "Split variable \n(ordered by random forest importance from left to right)",
      y = "Node depth"
    )

}
