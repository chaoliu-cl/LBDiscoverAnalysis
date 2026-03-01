# Prepare articles for report generation

This function ensures article data is valid for report generation,
particularly handling publication years.

## Usage

``` r
prep_articles(articles, verbose = TRUE)
```

## Arguments

- articles:

  The article data frame (can be NULL)

- verbose:

  Logical; if TRUE, print status messages (default: TRUE)

## Value

A data frame of articles with validated publication years

## Examples

``` r
# Create example article data
articles <- data.frame(
  title = c("Migraine Study 1", "Headache Research"),
  publication_year = c("2020", "not_a_year")
)

# Prepare articles
prepared <- prep_articles(articles)
#> Found 1 articles with valid publication years
print(prepared)
#>              title publication_year
#> 1 Migraine Study 1             2020
```
