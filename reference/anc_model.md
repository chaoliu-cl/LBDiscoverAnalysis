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

## Examples

``` r
terms <- c("migraine", "serotonin", "cgrp", "sumatriptan")
co_matrix <- matrix(
  c(
    0.0, 0.8, 0.7, 0.5,
    0.8, 0.0, 0.6, 0.3,
    0.7, 0.6, 0.0, 0.4,
    0.5, 0.3, 0.4, 0.0
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(terms, terms)
)
attr(co_matrix, "entity_types") <- c(
  migraine = "disease",
  serotonin = "chemical",
  cgrp = "protein",
  sumatriptan = "drug"
)
anc_model(
  co_matrix,
  a_term = "migraine",
  n_b_terms = 2,
  min_score = 0.2,
  validation_function = function(term, claimed_type) TRUE
)
#> Validating biomedical relevance of B terms...
#> Retained 3 biomedically relevant B terms after filtering
#> Validating biomedical relevance of C terms...
#> Retained 1 biomedically relevant C terms after filtering
#> Analyzing 1 potential C terms...
#>   |                                                                              |                                                                      |   0%  |                                                                              |======================================================================| 100%
#>     a_term         b_terms      c_term a_b_scores b_c_scores anc_score  a_type
#> 1 migraine serotonin, cgrp sumatriptan   0.8, 0.7   0.3, 0.4 0.5123475 disease
#>   c_type
#> 1   drug
```
