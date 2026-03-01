# Optimize ABC model calculations for large matrices

This function implements an optimized version of the ABC model
calculation that's more efficient for large co-occurrence matrices.

## Usage

``` r
abc_model_opt(
  co_matrix,
  a_term,
  c_term = NULL,
  min_score = 0.1,
  n_results = 100,
  chunk_size = 500
)
```

## Arguments

- co_matrix:

  A co-occurrence matrix produced by create_cooccurrence_matrix().

- a_term:

  Character string, the source term (A).

- c_term:

  Character string, the target term (C). If NULL, all potential C terms
  will be evaluated.

- min_score:

  Minimum score threshold for results.

- n_results:

  Maximum number of results to return.

- chunk_size:

  Number of B terms to process in each chunk.

## Value

A data frame with ranked discovery results.

## Examples

``` r
if (FALSE) { # \dontrun{
abc_results <- abc_model_opt(co_matrix, a_term = "migraine")
} # }
```
