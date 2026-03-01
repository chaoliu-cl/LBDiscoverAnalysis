# Create an enhanced network visualization of ABC connections

This function creates an improved network visualization of ABC
connections that displays entity types when available, without enforcing
type constraints.

## Usage

``` r
vis_network(
  abc_results,
  top_n = 25,
  min_score = 0.1,
  show_significance = TRUE,
  node_size_factor = 5,
  color_by = "type",
  title = "ABC Model Network",
  show_entity_types = TRUE,
  label_size = 1
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

  Logical. If TRUE, highlights significant connections.

- node_size_factor:

  Factor for scaling node sizes.

- color_by:

  Column to use for node colors. Default is 'type'.

- title:

  Plot title.

- show_entity_types:

  Logical. If TRUE, includes entity types in node labels.

- label_size:

  Relative size for labels. Default is 1.

## Value

NULL invisibly. The function creates a plot as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
vis_network(abc_results, top_n = 20, show_significance = TRUE,
                     show_entity_types = TRUE)
} # }
```
