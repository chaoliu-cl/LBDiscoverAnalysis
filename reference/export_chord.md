# Export interactive HTML chord diagram for ABC connections

This function creates an HTML chord diagram visualization for ABC
connections.

## Usage

``` r
export_chord(
  abc_results,
  output_file = "abc_chord.html",
  top_n = 50,
  min_score = 0.1,
  open = TRUE
)
```

## Arguments

- abc_results:

  A data frame containing ABC results.

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
export_chord(abc_results, output_file = "abc_chord.html")
} # }
```
