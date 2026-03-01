# Create network visualization from results

This function creates a network visualization from ABC results.

## Usage

``` r
plot_network(
  results,
  output_file = "network.png",
  width = 1200,
  height = 900,
  resolution = 120,
  top_n = 15,
  min_score = 1e-04,
  node_size_factor = 5,
  color_by = "type",
  title = "Network Visualization",
  show_entity_types = TRUE,
  label_size = 1,
  verbose = TRUE
)
```

## Arguments

- results:

  The results to visualize

- output_file:

  Filename for the output PNG (default: "network.png")

- width:

  Width of the output image (default: 1200)

- height:

  Height of the output image (default: 900)

- resolution:

  Resolution of the output image (default: 120)

- top_n:

  Maximum number of results to include (default: 15)

- min_score:

  Minimum score threshold (default: 0.0001)

- node_size_factor:

  Factor for scaling node sizes (default: 5)

- color_by:

  Column to use for node colors (default: "type")

- title:

  Plot title (default: "Network Visualization")

- show_entity_types:

  Logical; if TRUE, show entity types (default: TRUE)

- label_size:

  Relative size for labels (default: 1.0)

- verbose:

  Logical; if TRUE, print status messages (default: TRUE)

## Value

Invisible NULL (creates a file as a side effect)

## Examples

``` r
# Create example results for visualization
results <- data.frame(
  a_term = rep("migraine", 4),
  b_term = c("serotonin", "CGRP", "cortisol", "dopamine"),
  c_term = c("sumatriptan", "fremanezumab", "propranolol", "amitriptyline"),
  abc_score = c(0.8, 0.7, 0.6, 0.5),
  b_type = c("chemical", "protein", "hormone", "chemical"),
  c_type = rep("drug", 4)
)

# \donttest{
# These require graphics capabilities
plot_heatmap(results, output_file = tempfile(fileext = ".png"))
#> Warning: Entity types not found in results. Setting show_entity_types = FALSE
#> Created heatmap visualization: /tmp/Rtmp3kBagP/file1b7326e711a3.png 
plot_network(results, output_file = tempfile(fileext = ".png"))
#> Warning: Entity types not found in results. Setting show_entity_types = FALSE
#> Created network visualization: /tmp/Rtmp3kBagP/file1b7367418645.png 
# }
```
