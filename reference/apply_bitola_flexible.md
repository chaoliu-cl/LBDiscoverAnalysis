# Apply a flexible BITOLA-style discovery model without strict type constraints

This function implements a modified BITOLA-style discovery model that
preserves entity type information but doesn't enforce strict type
constraints.

## Usage

``` r
apply_bitola_flexible(co_matrix, a_term, min_score = 0.1, n_results = 100)
```

## Arguments

- co_matrix:

  A co-occurrence matrix with entity types as an attribute.

- a_term:

  Character string, the source term (A).

- min_score:

  Minimum score threshold for results.

- n_results:

  Maximum number of results to return.

## Value

A data frame with ranked discovery results.
