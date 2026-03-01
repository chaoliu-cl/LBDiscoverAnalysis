# Create a term-document matrix from preprocessed text

This function creates a term-document matrix from preprocessed text
data.

## Usage

``` r
create_tdm(preprocessed_data, min_df = 2, max_df = 0.9)
```

## Arguments

- preprocessed_data:

  A data frame with preprocessed text data.

- min_df:

  Minimum document frequency for a term to be included.

- max_df:

  Maximum document frequency (as a proportion) for a term to be
  included.

## Value

A term-document matrix.

## Examples

``` r
if (FALSE) { # \dontrun{
preprocessed <- preprocess_text(article_data, text_column = "abstract")
tdm <- create_tdm(preprocessed)
} # }
```
