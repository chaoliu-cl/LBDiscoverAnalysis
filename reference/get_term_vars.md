# Extract term variations from text corpus

This function identifies variations of a primary term within a corpus of
articles.

## Usage

``` r
get_term_vars(articles, primary_term, text_col = "abstract")
```

## Arguments

- articles:

  A data frame containing article data with text columns

- primary_term:

  The primary term to find variations of

- text_col:

  Name of the column containing the text to search

## Value

A character vector of unique term variations, sorted by length

## Examples

``` r
# Create example articles
articles <- data.frame(
  abstract = c(
    "Migraine headaches are debilitating",
    "Migraines affect quality of life",
    "Migraine disorders require treatment"
  )
)

# Get term variations
variations <- get_term_vars(articles, "migrain")
#> Error in get_term_vars(articles, "migrain"): could not find function "get_term_vars"
print(variations)
#> Error: object 'variations' not found
```
