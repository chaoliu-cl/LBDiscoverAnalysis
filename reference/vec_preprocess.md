# Vectorized preprocessing of text

This function preprocesses text data using vectorized operations for
better performance.

## Usage

``` r
vec_preprocess(
  text_data,
  text_column = "abstract",
  remove_stopwords = TRUE,
  custom_stopwords = NULL,
  min_word_length = 3,
  max_word_length = 50,
  chunk_size = 100
)
```

## Arguments

- text_data:

  A data frame containing text data.

- text_column:

  Name of the column containing text to process.

- remove_stopwords:

  Logical. If TRUE, removes stopwords.

- custom_stopwords:

  Character vector of additional stopwords to remove.

- min_word_length:

  Minimum word length to keep.

- max_word_length:

  Maximum word length to keep.

- chunk_size:

  Number of documents to process in each chunk.

## Value

A data frame with processed text.

## Examples

``` r
if (FALSE) { # \dontrun{
processed_data <- vec_preprocess(article_data, text_column = "abstract")
} # }
```
