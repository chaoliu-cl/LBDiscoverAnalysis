# Merge multiple search results

This function merges multiple search results into a single data frame.

## Usage

``` r
merge_results(..., remove_duplicates = TRUE)
```

## Arguments

- ...:

  Data frames containing search results.

- remove_duplicates:

  Logical. If TRUE, removes duplicate articles.

## Value

A merged data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
merged_results <- merge_results(results1, results2, results3)
} # }
```
