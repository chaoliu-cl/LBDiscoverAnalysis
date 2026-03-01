# Compare term frequencies between two corpora

This function compares term frequencies between two sets of articles.

## Usage

``` r
compare_terms(
  corpus1,
  corpus2,
  text_column = "abstract",
  corpus1_name = "Corpus1",
  corpus2_name = "Corpus2",
  n = 100,
  remove_stopwords = TRUE
)
```

## Arguments

- corpus1:

  First corpus (data frame).

- corpus2:

  Second corpus (data frame).

- text_column:

  Name of the column containing the text to analyze.

- corpus1_name:

  Name for the first corpus in the output.

- corpus2_name:

  Name for the second corpus in the output.

- n:

  Number of top terms to return.

- remove_stopwords:

  Logical. If TRUE, removes stopwords.

## Value

A data frame containing term frequency comparisons.

## Examples

``` r
if (FALSE) { # \dontrun{
comparison <- compare_terms(corpus1, corpus2,
                                      corpus1_name = "Migraine",
                                      corpus2_name = "Magnesium")
} # }
```
