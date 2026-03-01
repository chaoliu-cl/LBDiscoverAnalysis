# Ensure minimum results for visualization

This function ensures there are sufficient results for visualization,
creating placeholder data if necessary.

## Usage

``` r
min_results(
  diverse_results,
  top_results,
  a_term,
  min_results = 3,
  fallback_count = 15,
  verbose = TRUE
)
```

## Arguments

- diverse_results:

  Current diversified results

- top_results:

  Original top results

- a_term:

  The primary term for the analysis

- min_results:

  Minimum number of desired results (default: 3)

- fallback_count:

  Number of top results to use as fallback (default: 15)

- verbose:

  Logical; if TRUE, print status messages (default: TRUE)

## Value

A data frame with sufficient results for visualization

## Examples

``` r
# Create example diverse results (empty case)
diverse_results <- data.frame()
top_results <- data.frame(
  a_term = rep("migraine", 3),
  b_term = c("serotonin", "CGRP", "cortisol"),
  c_term = c("sumatriptan", "fremanezumab", "propranolol"),
  abc_score = c(0.8, 0.7, 0.6)
)

# Ensure minimum results
final_results <- min_results(diverse_results, top_results, "migraine")
#> No results found. Creating a placeholder result for demonstration.
print(final_results)
#>     a_term                        b_term      c_term a_b_score b_c_score
#> 1 migraine                     serotonin sumatriptan      0.05      0.08
#> 2 migraine                          CGRP  topiramate      0.04      0.07
#> 3 migraine cortical spreading depression propranolol      0.03      0.06
#>   abc_score p_value significant
#> 1      0.04     0.1       FALSE
#> 2      0.03     0.2       FALSE
#> 3      0.02     0.3       FALSE
```
