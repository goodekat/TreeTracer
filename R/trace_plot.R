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
#' @importFrom ggplot2 aes element_blank ggplot geom_line geom_point geom_segment geom_text theme
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

trace_plot <- function(rf, train, tree_ids, width = 0.8, alpha = 0.5, color_by_id = FALSE) {

  # trace_data: output from get_trace_data function
  # alpha: alpha to use for the lines in the plot

  # Obtain the trace data from the specified trees
  trace_data <-
    purrr::map_df(
      .x = tree_ids,
      .f = function(id)
        get_tree_data(rf = rf , k = id)
    ) %>%
    get_trace_data(train = train, width = width)

  # Extract the levels that correspond to a tree
  trees = sort(unique(trace_data$tree))
  tree_branches = sort(unique(trace_data$tree_branch))
  tree_levels = sort(unique(trace_data$tree_level), decreasing = TRUE)

  # Convert categorical variables to factors
  trace_data <-
    trace_data %>%
    mutate(
      tree = factor(.data$tree, levels = trees),
      tree_branch = factor(.data$tree_branch, levels = tree_branches),
      tree_level = factor(.data$tree_level, levels = tree_levels)
    )

  # Extract the split variables to use as labels in the trace plot
  trace_labels <-
    trace_data %>%
    select(.data$tree_level, .data$split_var, .data$seg_xmid) %>%
    distinct()

  # Create a trace plot
  trace_plot <-
    trace_data %>%
    ggplot() +
    geom_segment(
      mapping = aes(
        x = .data$seg_xmin,
        xend = .data$seg_xmax,
        y = .data$tree_level,
        yend = .data$tree_level,
        group = .data$tree_level:factor(.data$split_var)
      )
    )

  # Add color to the plot
  if (color_by_id == TRUE) {
    trace_plot <-
      trace_plot +
      geom_line(
        mapping = aes(
          x = .data$split_scaled,
          y = .data$tree_level,
          group = factor(.data$tree):factor(.data$tree_branch),
          color = factor(.data$tree)
        ),
        alpha = alpha
      )
  } else {
    trace_plot <-
      trace_plot +
      geom_line(
        mapping = aes(
          x = .data$split_scaled,
          y = .data$tree_level,
          group = factor(.data$tree):factor(.data$tree_branch)
        ),
        alpha = alpha
      )
  }

  # Finish traceplot
  trace_plot +
    geom_point(mapping = aes(x = .data$split_scaled, y = .data$tree_level),
               shape = 3) +
    geom_text(
      data = trace_labels,
      mapping = aes(x = .data$seg_xmid, y = .data$tree_level, label = .data$split_var),
      nudge_y = -0.1
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )

}
