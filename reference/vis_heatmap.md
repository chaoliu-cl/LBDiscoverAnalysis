# Create an enhanced heatmap of ABC connections

This function creates an improved heatmap visualization of ABC
connections that can display entity type information when available,
without enforcing type constraints.

## Usage

``` r
vis_heatmap(
  abc_results,
  top_n = 25,
  min_score = 0.1,
  show_significance = TRUE,
  color_palette = "blues",
  title = "ABC Connections Heatmap",
  show_entity_types = TRUE
)
```

## Arguments

- abc_results:

  A data frame containing ABC results.

- top_n:

  Number of top results to visualize.

- min_score:

  Minimum score threshold for including connections.

- show_significance:

  Logical. If TRUE, marks significant connections.

- color_palette:

  Character. Color palette to use for the heatmap.

- title:

  Plot title.

- show_entity_types:

  Logical. If TRUE, includes entity types in axis labels.

## Value

NULL invisibly. The function creates a plot as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
vis_heatmap(abc_results, top_n = 20, show_significance = TRUE)
} # }
```
