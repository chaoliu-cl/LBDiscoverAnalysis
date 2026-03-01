# Export ABC results to simple HTML network

This function exports ABC results to a simple HTML file with a
visualization. If the visNetwork package is available, it will use it
for a more interactive visualization.

## Usage

``` r
export_network(
  abc_results,
  output_file = "abc_network.html",
  top_n = 50,
  min_score = 0.1,
  open = TRUE
)
```

## Arguments

- abc_results:

  A data frame containing ABC results from apply_abc_model().

- output_file:

  File path for the output HTML file.

- top_n:

  Number of top results to visualize.

- min_score:

  Minimum score threshold for including connections.

- open:

  Logical. If TRUE, opens the HTML file after creation.

## Value

The file path of the created HTML file (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
export_network(abc_results, output_file = "abc_network.html")
} # }
```
