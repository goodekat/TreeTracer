#' Convert data frame of distances from compute_fit_metric to distance matrix
#'
#' The function converts the data frame of distances computed using
#' compute_fit_metric to a matrix of distances between all trees. Helpful to
#' use for clustering.
#'
#' @export get_dist_matrix
#'
#' @importFrom dplyr %>% arrange bind_rows mutate rename select
#' @importFrom stats as.dist
#' @importFrom textshape column_to_rownames
#' @importFrom tidyr pivot_wider
#'
#' @param fit_metrics output object from compute_fit_metric
#' @examples
#'
#' # Load packages
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
#'     data = penguins,
#'     ntree = 10
#'   )
#'
#' # Compute fit metrics between all trees
#' fit_metrics <- compute_fit_metric(penguin_rf, penguins)
#'
#' # Obtain the distance matrix
#' get_dist_matrix(fit_metrics)

get_dist_matrix <- function(fit_metrics) {

  # Set the fit metrics to the "original" ones
  orig <- fit_metrics

  # Create a data frame with the "reverse" of the original metrics
  rev <- fit_metrics %>%
    mutate(t1_temp = .data$t2, t2_temp = .data$t1) %>%
    select(-.data$t1,-.data$t2) %>%
    rename(t1 = .data$t1_temp, t2 = .data$t2_temp)

  # Grab the tree ids
  tree_ids = unique(c(fit_metrics$t1, fit_metrics$t2))

  # Create a matrix with the "identity" metrics
  iden <- data.frame(t1 = tree_ids, t2 = tree_ids, distance = 1)

  # Join all metrics
  full <- bind_rows(orig, rev, iden)

  # Convert the metrics to distances and put in a "dist" matrix
  dist_matrix <-
    full %>%
    mutate(dist = .data$distance) %>%
    select(-.data$distance) %>%
    mutate(t2 = paste0("tree", .data$t2)) %>%
    pivot_wider(names_from = .data$t2, values_from = .data$dist) %>%
    arrange(.data$t1) %>%
    mutate(t1 = paste0("tree", .data$t1)) %>%
    textshape::column_to_rownames(loc = "t1") %>%
    select(paste0("tree", 1:length(tree_ids))) %>%
    as.dist()

  # Return the distance matrix
  return(dist_matrix)

}
