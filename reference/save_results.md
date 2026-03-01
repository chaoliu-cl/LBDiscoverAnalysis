# Save search results to a file

This function saves search results to a file.

## Usage

``` r
save_results(results, file_path, format = c("csv", "rds", "xlsx"))
```

## Arguments

- results:

  A data frame containing search results.

- file_path:

  File path to save the results.

- format:

  File format to use. One of "csv", "rds", or "xlsx".

## Value

The file path (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
save_results(search_results, file_path = "search_results.csv")
} # }
```
