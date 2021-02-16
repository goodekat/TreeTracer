#' Computes metric comparing covariates from two trees
#'
#' Function for computing the fit metric from \insertCite{banerjee:2012;textual}{TreeTracer}.
#'
#' @references{
#'   \insertRef{banerjee:2012}{TreeTracer}
#' }
#'
#' @export compute_covariate_metric
#'
#' @importFrom dplyr %>% filter
#' @importFrom purrr map_df
#' @importFrom utils combn
#'
#' @param rf randomForest object from which to compute similarities between trees
#'
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
#' penguin.rf <-
#'   randomForest::randomForest(
#'     species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
#'     data = penguins,
#'     ntree = 10
#'   )
#'
#' # Compute fit metrics between all trees
#' compute_covariate_metric(penguin.rf)

compute_covariate_metric <- function(rf) {

  # Determine the number of covariates in the random forest
  k = length(rf$forest$xlevels)

  # Get tree data for all trees in the RF
  all_trees_df = purrr::map_df(
    .x = 1:rf$ntree,
    .f = function(t) {
      get_tree_data(rf = rf, k = t) %>% select(tree, split_var) %>% distinct()
    })

  # Create a data frame of indicators for whether a variable is used in a tree or not
  var_indicators <-
    all_trees_df %>%
    mutate(ind = 1) %>%
    pivot_wider(names_from = split_var, values_from = ind) %>%
    mutate_all(.funs = function(x) ifelse(is.na(x), 0, x))

  # Create a matrix with all pairs of trees
  tree_pairs = combn(1:rf$ntree, 2)

  # Compute metric d0 from Banerjee, Ding, and Noone
  purrr::map_df(
    .x = 1:dim(tree_pairs)[2],
    .f = function(index) {
      t1 = tree_pairs[1,index]
      t2 = tree_pairs[2,index]
      data.frame(t1 = t1, t2 = t2, similarity = sum(var_indicators[t1,-1] == var_indicators[t2,-1]) / k)
    }
  )

}
