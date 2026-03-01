# Visualize ABC model results as a network

Create a network visualization of ABC connections using base R graphics.

## Usage

``` r
vis_abc_network(
  abc_results,
  top_n = 25,
  min_score = 0.1,
  node_size_factor = 3,
  edge_width_factor = 1,
  color_by = "type",
  title = "ABC Model Network"
)
```

## Arguments

- abc_results:

  A data frame containing ABC results from apply_abc_model().

- top_n:

  Number of top results to visualize.

- min_score:

  Minimum score threshold for including connections.

- node_size_factor:

  Factor for scaling node sizes.

- edge_width_factor:

  Factor for scaling edge widths.

- color_by:

  Column to use for node colors. Default is 'type'.

- title:

  Plot title.

## Value

NULL invisibly. The function creates a plot as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a network visualization of ABC model results
vis_abc_network(abc_results, top_n = 20)
} # }
```
