
# TreeTracer 🌴 🖊

The beginnings…

TreeTracer is an R package for creating trace plots of trees from random
forests fit using the randomForest R package. Trace plots are useful
tools for visually comparing trees from a random forest. See Urbanek
(2008) for additional information about trace plots.

Examples:

``` r
# Load the package
library(TreeTracer)
```

``` r
# Load other packages
library(dplyr)
library(ggpcp)
library(ggplot2)
```

``` r
# Load the Palmer penguins data
penguins <- na.omit(palmerpenguins::penguins)
```

``` r
# Create a parallel coordinate plot of features that will be used to fit
# a random forest colored by the variable of interest to predict (species)
penguins %>%
  ggplot(aes(color = species)) +
  geom_pcp(aes(
    vars = vars(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
  ), alpha = 0.5) + 
  scale_color_brewer(palette = "Paired")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Fit a random forest
set.seed(71)
penguin.rf <-
  randomForest::randomForest(
    species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
    data = penguins, 
    ntree = 50
  )
```

``` r
# Generate a trace plot of the first 10 trees in the forest
trace_plot(
  rf = penguin.rf,
  train = penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
  tree_ids = 1:10
)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Plot all trees in the forest and adjust alpha
trace_plot(
  rf = penguin.rf,
  train = penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
  tree_ids = 1:penguin.rf$ntree,
  alpha = 0.05
)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Fit metric compares the predictions of the individual trees:

``` r
# Compute fit metrics between all trees
fit_metrics = compute_fit_metric(penguin.rf, penguins)
head(fit_metrics)
```

    ##   t1 t2 similarity
    ## 1  1  2   0.984985
    ## 2  1  3   0.981982
    ## 3  1  4   0.972973
    ## 4  1  5   0.981982
    ## 5  1  6   0.969970
    ## 6  1  7   0.981982

``` r
# Get the distance matrix based on the fit metrics
dist_matrix <- get_dist_matrix(fit_metrics)
```

Hierarchical clustering:

``` r
stree <- hclust(dist_matrix, method = "single")
ctree <- hclust(dist_matrix, method = "complete")
atree <- hclust(dist_matrix, method = "average")

par(mfcol=c(1,3))
plot(stree, ylab = "Distance", main = "Single linkage")
plot(ctree, ylab = "Distance", main = "Complete linkage")
plot(atree, ylab = "Distance", main = "Average linkage")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Classic MDS:

``` r
mds_res <-
  cmdscale(dist_matrix) %>% 
  data.frame() %>%
  rename("Coordinate 1" = "X1", "Coordinate 2" = "X2") %>%
  tibble::rownames_to_column("Tree")

ggplot(mds_res, aes(x = `Coordinate 1`, y = `Coordinate 2`)) + 
  geom_text(aes(label = Tree))
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Plot tree 8 versus some others to see if any noticeable differences
trace_plot(
  rf = penguin.rf,
  train = penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
  tree_ids = c(12, 27, 3, 10), 
  color_by_id = TRUE
) + 
  scale_color_manual(values = c(rep("grey", 2), "blue", "grey"))
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Plot tree 8 versus some others to see if any noticeable differences
trace_plot(
  rf = penguin.rf,
  train = penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
  tree_ids = 1:penguin.rf$ntree,
  color_by_id = TRUE,
  alpha = 0.25
) +
  scale_color_manual(values = c(
    rep("black", 11),
    "blue",
    rep("black", 19),
    "blue",
    rep("black", 18)
  )) +
  theme(legend.position = "none")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## References

<div id="refs" class="references">

<div id="ref-urbanek:2008">

Urbanek, Simon. 2008. “Visualizing Trees and Forests.” In *Handbook of
Data Visualization*, edited by Chun-houh Chen, Wolfgang Härdle, and
Antony Unwin, 3:243–66. Berlin, Germany: Springer-Verlag.
<https://doi.org/10.1007/978-3-540-33037-0>.

</div>

</div>
