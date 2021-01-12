#' @export trace_plot
#'
#' @importFrom dplyr %>% distinct select
#' @importFrom ggplot2 aes element_blank ggplot geom_line geom_point geom_segment geom_text theme

trace_plot <- function(trace_data, alpha = 0.5) {

  # trace_data: output from get_trace_data function
  # alpha: alpha to use for the lines in the plot

  # Extract the levels that correspond to a tree
  trees = sort(unique(trace_data$tree))
  tree_branches = sort(unique(trace_data$tree_branch))
  tree_levels = sort(unique(trace_data$tree_level), decreasing = TRUE)

  # Convert categorical variables to factors
  trace_data <-
    trace_data %>%
    mutate(tree = factor(tree, levels = trees),
           tree_branch = factor(tree_branch, levels = tree_branches),
           tree_level = factor(tree_level, levels = tree_levels))

  # Extract the split variables to use as labels in the trace plot
  trace_labels <-
    trace_data %>%
    select(tree_level, split_var, seg_xmid) %>%
    distinct()

  # Create a trace plot
  trace_data %>%
    ggplot() +
    geom_segment(
      mapping = aes(
        x = seg_xmin,
        xend = seg_xmax,
        y = tree_level,
        yend = tree_level,
        group = tree_level:factor(split_var)
      )
    ) +
    geom_line(
      mapping = aes(
        x = split_scaled,
        y = tree_level,
        group = factor(tree):factor(tree_branch)
      ),
      alpha = alpha
    ) +
    geom_point(mapping = aes(x = split_scaled, y = tree_level),
               shape = 3) +
    geom_text(
      data = trace_labels,
      mapping = aes(x = seg_xmid, y = tree_level, label = split_var),
      nudge_y = -0.1
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )

}
