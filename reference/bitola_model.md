# Apply BITOLA-style discovery model

This function implements a BITOLA-style discovery model based on MeSH
term co-occurrence and semantic type filtering.

## Usage

``` r
bitola_model(
  co_matrix,
  a_term,
  a_semantic_type = NULL,
  c_semantic_type = NULL,
  min_score = 0.1,
  n_results = 100
)
```

## Arguments

- co_matrix:

  A co-occurrence matrix produced by create_cooccurrence_matrix().

- a_term:

  Character string, the source term (A).

- a_semantic_type:

  Character string, the semantic type for A term.

- c_semantic_type:

  Character string, the semantic type for C terms.

- min_score:

  Minimum score threshold for results.

- n_results:

  Maximum number of results to return.

## Value

A data frame with ranked discovery results.

## Examples

``` r
if (FALSE) { # \dontrun{
bitola_results <- bitola_model(co_matrix, a_term = "migraine",
                                   a_semantic_type = "Disease",
                                   c_semantic_type = "Gene")
} # }
```
