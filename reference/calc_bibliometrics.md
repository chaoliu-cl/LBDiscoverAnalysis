# Calculate basic bibliometric statistics

This function calculates basic bibliometric statistics from article
data.

## Usage

``` r
calc_bibliometrics(article_data, by_year = TRUE)
```

## Arguments

- article_data:

  A data frame containing article data.

- by_year:

  Logical. If TRUE, calculates statistics by year.

## Value

A list containing bibliometric statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
stats <- calc_bibliometrics(article_data)
} # }
```
