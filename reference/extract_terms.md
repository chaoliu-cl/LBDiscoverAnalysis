# Extract common terms from a corpus

This function extracts and counts the most common terms in a corpus.

## Usage

``` r
extract_terms(
  article_data,
  text_column = "abstract",
  n = 100,
  remove_stopwords = TRUE,
  min_word_length = 3
)
```

## Arguments

- article_data:

  A data frame containing article data.

- text_column:

  Name of the column containing the text to analyze.

- n:

  Number of top terms to return.

- remove_stopwords:

  Logical. If TRUE, removes stopwords.

- min_word_length:

  Minimum word length to include.

## Value

A data frame containing term counts.

## Examples

``` r
if (FALSE) { # \dontrun{
common_terms <- extract_terms(article_data, text_column = "abstract")
} # }
```
