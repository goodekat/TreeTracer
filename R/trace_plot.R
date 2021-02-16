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
#' @importFrom dplyr %>% distinct select
#' @importFrom ggplot2 aes element_blank facet_wrap ggplot geom_line geom_point geom_segment geom_text labs scale_x_continuous theme
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
#' @param color_by_id should the trace lines be colored by the tree IDs? (default if FALSE)
#' @param facet_by_id should the traces be faceted by tree IDs? (default if FALSE)
#' @param nrow number of rows if facet_by_id is TRUE (othewise ignored)
#' @param max_depth the deepest level to include in the trace plot (set to NULl by default)
#' @param rep_tree option to add a "representative tree" on top of the trace plot by providing
#'                 a data frame with the structure of the get_tree_data function (NULL by default)
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
#' penguin.rf <-
#'   randomForest::randomForest(
#'     species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'     data = penguins
#'   )
#'
#' # Generate a trace plot of the first 10 trees in the forest
#' trace_plot(
#'  rf = penguin.rf,
#'  train = penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
#'  tree_ids = 1:10
#' )

trace_plot <- function(rf,
                       train,
                       tree_ids,
                       width = 0.8,
                       alpha = 0.5,
                       color_by_id = FALSE,
                       facet_by_id = FALSE,
                       nrow = NULL,
                       max_depth = NULL,
                       rep_tree = NULL,
                       rep_tree_size = 1,
                       rep_tree_color = "blue",
                       rep_tree_alpha = 1) {


  # trace_data: output from get_trace_data function
  # alpha: alpha to use for the lines in the plot

  # Obtain the trace data from the specified trees
  if (is.null(rep_tree)) {
    trace_data <-
      purrr::map_df(
        .x = tree_ids,
        .f = function(id)
          get_tree_data(rf = rf , k = id)
      ) %>%
      get_trace_data(train = train, width = width)
  } else {
    trace_data <-
      purrr::map_df(
        .x = tree_ids,
        .f = function(id)
          get_tree_data(rf = rf , k = id)
      ) %>%
      mutate(tree = as.character(.data$tree)) %>%
      bind_rows(rep_tree %>% mutate(tree = as.character("rep"))) %>%
      get_trace_data(train = train, width = width)
  }

  # Extract the levels that correspond to a tree
  trees = sort(unique(trace_data$tree))
  tree_branches = sort(unique(trace_data$tree_branch))
  tree_levels = sort(unique(trace_data$tree_level), decreasing = TRUE)
  split_vars = unique(trace_data$split_var)

  # Keep only a subset of tree levels if requested
  if (!is.null(max_depth)) {
    trace_data <- trace_data %>% filter(.data$tree_level <= max_depth)
  }

  # Convert categorical variables to factors
  trace_data <-
    trace_data %>%
    mutate(
      tree = factor(.data$tree, levels = trees),
      tree_branch = factor(.data$tree_branch, levels = tree_branches),
      tree_level = factor(.data$tree_level, levels = tree_levels),
      split_vars = factor(.data$split_var, levels = split_vars)
    )

  # Extract the split variables to use as labels in the trace plot
  trace_labels <-
    trace_data %>%
    select(.data$tree_level, .data$split_var, .data$seg_xmid) %>%
    distinct()

  # Create a trace plot
  trace_plot <-
    ggplot(trace_data) +
    geom_segment(
      mapping = aes(
        x = .data$seg_xmin,
        xend = .data$seg_xmax,
        y = .data$tree_level,
        yend = .data$tree_level
      )
    ) +
    scale_x_continuous(breaks = 1:length(split_vars), labels = split_vars)

  # Add color to the plot
  if (color_by_id == TRUE) {
    trace_plot <-
      trace_plot +
      geom_line(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$tree_level,
          group = .data$tree:.data$tree_branch,
          color = .data$tree
        ),
        alpha = alpha
      ) +
      geom_point(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$tree_level,
          color = .data$tree
        ),
        shape = 124
      ) +
      labs(color = "Tree ID")
  } else {
    trace_plot <-
      trace_plot +
      geom_line(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(
          x = .data$split_scaled,
          y = .data$tree_level,
          group = factor(.data$tree):factor(.data$tree_branch)
        ),
        alpha = alpha
      ) +
      geom_point(
        data = trace_data %>% filter(.data$tree != "rep"),
        mapping = aes(x = .data$split_scaled, y = .data$tree_level), shape = 124)
  }

  # # Add text to plot
  # trace_plot <-
  #   trace_plot +
  #   geom_text(
  #     data = trace_labels,
  #     mapping = aes(x = .data$seg_xmid, y = .data$tree_level, label = .data$split_var),
  #     nudge_y = -0.1
  #   )

  # Add representative tree (if given)
  if (!is.null(rep_tree)) {
    trace_plot <-
      trace_plot +
      geom_line(
        data = trace_data %>% filter(.data$tree == "rep"),
        aes(
          x = .data$split_scaled,
          y = .data$tree_level,
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
          y = .data$tree_level,
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
    labs(x = "Split variable", y = "Tree level")

}
