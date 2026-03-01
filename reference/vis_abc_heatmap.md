# Create a heatmap of ABC connections

This function creates a heatmap visualization of ABC connections using
base R graphics.

## Usage

``` r
vis_abc_heatmap(
  abc_results,
  top_n = 25,
  min_score = 0.1,
  show_labels = TRUE,
  title = "ABC Connections Heatmap"
)
```

## Arguments

- abc_results:

  A data frame containing ABC results from apply_abc_model().

- top_n:

  Number of top results to visualize.

- min_score:

  Minimum score threshold for including connections.

- show_labels:

  Logical. If TRUE, shows labels on the tiles.

- title:

  Plot title.

## Value

NULL invisibly. The function creates a plot as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
vis_abc_heatmap(abc_results, top_n = 20)
} # }
```
