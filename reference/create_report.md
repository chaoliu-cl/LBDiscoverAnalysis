# Generate a comprehensive discovery report

This function generates an HTML report summarizing discovery results
without enforcing entity type constraints. It includes data validation
to avoid errors with publication years and other data issues.

## Usage

``` r
create_report(
  results,
  visualizations = NULL,
  articles = NULL,
  output_file = "discovery_report.html"
)
```

## Arguments

- results:

  A list containing discovery results from different approaches.

- visualizations:

  A list containing file paths to visualizations.

- articles:

  A data frame containing the original articles.

- output_file:

  File path for the output HTML report.

## Value

The file path of the created HTML report (invisibly).

## Examples

``` r
results <- list(
  abc = data.frame(
    a_term = "migraine",
    b_term = "serotonin",
    c_term = "cgrp",
    abc_score = 0.42,
    stringsAsFactors = FALSE
  )
)
out_file <- tempfile(fileext = ".html")
create_report(results, output_file = out_file)
file.exists(out_file)
#> [1] TRUE
```
