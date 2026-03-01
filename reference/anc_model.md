# ANC model for literature-based discovery with biomedical term filtering

This function implements an improved ANC model that ensures only
biomedical terms are used as intermediaries.

## Usage

``` r
anc_model(
  co_matrix,
  a_term,
  n_b_terms = 3,
  c_type = NULL,
  min_score = 0.1,
  n_results = 100,
  enforce_biomedical_terms = TRUE,
  b_term_types = c("protein", "gene", "chemical", "pathway", "drug", "disease",
    "biological_process"),
  validation_function = is_valid_biomedical_entity
)
```

## Arguments

- co_matrix:

  A co-occurrence matrix produced by create_cooccurrence_matrix().

- a_term:

  Character string, the source term (A).

- n_b_terms:

  Number of intermediate B terms to consider.

- c_type:

  Character string, the entity type for C terms. If NULL, all types are
  considered.

- min_score:

  Minimum score threshold for results.

- n_results:

  Maximum number of results to return.

- enforce_biomedical_terms:

  Logical. If TRUE, enforces strict biomedical term filtering.

- b_term_types:

  Character vector of entity types allowed for B terms.

- validation_function:

  Function to validate biomedical terms.

## Value

A data frame with ranked discovery results.
