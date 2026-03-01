# Evaluate literature support for discovery results

This function evaluates the top results by searching for supporting
evidence in the literature for the connections.

## Usage

``` r
eval_evidence(
  results,
  max_results = 5,
  base_term = NULL,
  max_articles = 5,
  verbose = TRUE
)
```

## Arguments

- results:

  The results to evaluate

- max_results:

  Maximum number of results to evaluate (default: 5)

- base_term:

  The base term for direct connection queries (e.g., "migraine")

- max_articles:

  Maximum number of articles to retrieve per search (default: 5)

- verbose:

  Logical; if TRUE, print evaluation results (default: TRUE)

## Value

A list containing evaluation results

## Examples

``` r
# Create example results for evaluation
results <- data.frame(
  a_term = rep("migraine", 2),
  b_term = c("serotonin", "CGRP"),
  c_term = c("sumatriptan", "fremanezumab"),
  abc_score = c(0.8, 0.7),
  c_type = rep("drug", 2)
)

if (FALSE) { # \dontrun{
# This requires internet connection for PubMed search
evaluation <- eval_evidence(results, max_results = 2, base_term = "migraine")
print(names(evaluation))
} # }
```
