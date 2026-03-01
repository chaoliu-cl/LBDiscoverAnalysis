# Apply the ABC model with statistical significance testing

This function extends the ABC model with statistical significance
testing to evaluate the strength of discovered connections.

## Usage

``` r
abc_model_sig(
  co_matrix,
  a_term,
  c_term = NULL,
  a_type = NULL,
  c_type = NULL,
  min_score = 0.1,
  n_results = 100,
  n_permutations = 1000,
  scoring_method = c("multiplication", "average", "combined", "jaccard")
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

- a_type:

  Character string, the entity type for A terms. If NULL, all types are
  considered.

- c_type:

  Character string, the entity type for C terms. If NULL, all types are
  considered.

- min_score:

  Minimum score threshold for results.

- n_results:

  Maximum number of results to return.

- n_permutations:

  Number of permutations for significance testing.

- scoring_method:

  Method to use for scoring ABC connections.

## Value

A data frame with ranked discovery results and p-values.

## Examples

``` r
if (FALSE) { # \dontrun{
abc_results <- abc_model_sig(co_matrix, a_term = "migraine",
                                                scoring_method = "combined")
} # }
```
