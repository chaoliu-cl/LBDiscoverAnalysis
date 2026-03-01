# Perform randomization test for ABC model

This function assesses the significance of ABC model results through
randomization. It generates a null distribution by permuting the
co-occurrence matrix.

## Usage

``` r
perm_test_abc(abc_results, co_matrix, n_permutations = 1000, alpha = 0.05)
```

## Arguments

- abc_results:

  A data frame containing ABC results.

- co_matrix:

  The co-occurrence matrix used to generate the ABC results.

- n_permutations:

  Number of permutations to perform.

- alpha:

  Significance level.

## Value

A data frame with ABC results and permutation-based significance
measures.

## Examples

``` r
if (FALSE) { # \dontrun{
randomized_results <- perm_test_abc(abc_results, co_matrix, n_permutations = 1000)
} # }
```
