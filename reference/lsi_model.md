# LSI model with enhanced biomedical term filtering and NLP verification

This function implements an improved LSI model that more rigorously
filters out non-biomedical terms from the results to ensure clinical
relevance.

## Usage

``` r
lsi_model(
  term_doc_matrix,
  a_term,
  n_factors = 100,
  n_results = 100,
  enforce_biomedical_terms = TRUE,
  c_term_types = NULL,
  entity_types = NULL,
  validation_function = is_valid_biomedical_entity,
  min_word_length = 3,
  use_nlp = TRUE,
  nlp_threshold = 0.7
)
```

## Arguments

- term_doc_matrix:

  A term-document matrix.

- a_term:

  Character string, the source term (A).

- n_factors:

  Number of factors to use in LSI.

- n_results:

  Maximum number of results to return.

- enforce_biomedical_terms:

  Logical. If TRUE, enforces strict biomedical term filtering.

- c_term_types:

  Character vector of entity types allowed for C terms.

- entity_types:

  Named vector of entity types (if NULL, will try to detect).

- validation_function:

  Function to validate biomedical terms.

- min_word_length:

  Minimum word length to include.

- use_nlp:

  Logical. If TRUE, uses NLP-based validation for biomedical terms.

- nlp_threshold:

  Numeric between 0 and 1. Minimum confidence for NLP validation.

## Value

A data frame with ranked discovery results.
