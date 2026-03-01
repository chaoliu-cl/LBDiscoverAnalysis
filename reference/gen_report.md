# Generate comprehensive discovery report

This function creates a comprehensive HTML report from discovery results
and visualizations.

## Usage

``` r
gen_report(
  results_list,
  visualizations = NULL,
  articles = NULL,
  output_file = "discoveries.html",
  verbose = TRUE
)
```

## Arguments

- results_list:

  A list of result data frames from different approaches

- visualizations:

  A list with paths to visualization files

- articles:

  Prepared article data

- output_file:

  Filename for the output HTML report

- verbose:

  Logical; if TRUE, print status messages (default: TRUE)

## Value

Invisible output_file path

## Examples

``` r
# Create example data for report generation
results_list <- list(
  abc_results = data.frame(
    a_term = "migraine",
    c_term = "sumatriptan",
    abc_score = 0.8
  )
)

# \donttest{
# Generate report to temporary file
temp_file <- tempfile(fileext = ".html")
gen_report(results_list, output_file = temp_file)
#> Generated comprehensive report: /tmp/Rtmp3kBagP/file1b73447fea5e.html 
# }
```
