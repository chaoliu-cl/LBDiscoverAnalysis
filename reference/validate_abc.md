# Apply statistical validation to ABC model results with support for large matrices

This function performs statistical tests to validate ABC model results.
It calculates p-values using hypergeometric tests and applies correction
for multiple testing. The function is optimized to work with very large
co-occurrence matrices.

## Usage

``` r
validate_abc(
  abc_results,
  co_matrix,
  alpha = 0.05,
  correction = c("BH", "bonferroni", "none"),
  filter_by_significance = FALSE
)
```

## Arguments

- abc_results:

  A data frame containing ABC results.

- co_matrix:

  The co-occurrence matrix used to generate the ABC results.

- alpha:

  Significance level (p-value threshold).

- correction:

  Method for multiple testing correction.

- filter_by_significance:

  Logical. If TRUE, only returns significant results.

## Value

A data frame with ABC results and statistical significance measures.

## Examples

``` r
if (FALSE) { # \dontrun{
validated_results <- validate_abc(abc_results, co_matrix)
} # }
```
