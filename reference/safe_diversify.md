# Diversify ABC results with error handling

This function diversifies ABC results to avoid redundancy, with error
handling to ensure results are always returned.

## Usage

``` r
safe_diversify(
  top_results,
  diversity_method = "both",
  max_per_group = 5,
  min_score = 1e-04,
  min_results = 5,
  fallback_count = 15,
  verbose = TRUE
)
```

## Arguments

- top_results:

  The top ABC results to diversify

- diversity_method:

  Method for diversification (default: "both")

- max_per_group:

  Maximum results per group (default: 5)

- min_score:

  Minimum score threshold (default: 0.0001)

- min_results:

  Minimum number of desired results (default: 5)

- fallback_count:

  Number of top results to use if diversification fails (default: 15)

- verbose:

  Logical; if TRUE, print status messages (default: TRUE)

## Value

A data frame of diversified results

## Examples

``` r
# Create example results
top_results <- data.frame(
  a_term = rep("migraine", 6),
  b_term = c("serotonin", "serotonin", "CGRP", "CGRP", "cortisol", "dopamine"),
  c_term = c("sumatriptan", "rizatriptan", "fremanezumab", "galcanezumab", "propranolol", "amitriptyline"),
  abc_score = c(0.8, 0.75, 0.7, 0.65, 0.6, 0.55)
)

# Apply diversification
diverse_results <- safe_diversify(top_results, max_per_group = 2)
print(diverse_results)
#>     a_term    b_term        c_term abc_score
#> 1 migraine serotonin   sumatriptan      0.80
#> 2 migraine serotonin   rizatriptan      0.75
#> 3 migraine      CGRP  fremanezumab      0.70
#> 4 migraine      CGRP  galcanezumab      0.65
#> 5 migraine  cortisol   propranolol      0.60
#> 6 migraine  dopamine amitriptyline      0.55
```
