# Apply the ABC model for literature-based discovery with improved filtering

This function implements the ABC model for literature-based discovery
with enhanced term filtering and validation.

## Usage

``` r
abc_model(
  co_matrix,
  a_term,
  c_term = NULL,
  min_score = 0.1,
  n_results = 100,
  scoring_method = c("multiplication", "average", "combined", "jaccard"),
  b_term_types = NULL,
  c_term_types = NULL,
  exclude_general_terms = TRUE,
  filter_similar_terms = TRUE,
  similarity_threshold = 0.8,
  enforce_strict_typing = TRUE,
  validation_method = "pattern"
)
```

## Arguments

- co_matrix:

  A co-occurrence matrix produced by create_comat().

- a_term:

  Character string, the source term (A).

- c_term:

  Character string, the target term (C). If NULL, all potential C terms
  will be evaluated.

- min_score:

  Minimum score threshold for results.

- n_results:

  Maximum number of results to return.

- scoring_method:

  Method to use for scoring.

- b_term_types:

  Character vector of entity types allowed for B terms.

- c_term_types:

  Character vector of entity types allowed for C terms.

- exclude_general_terms:

  Logical. If TRUE, excludes common general terms.

- filter_similar_terms:

  Logical. If TRUE, filters out B-terms that are too similar to A-term.

- similarity_threshold:

  Numeric. Maximum allowed string similarity between A and B terms.

- enforce_strict_typing:

  Logical. If TRUE, enforces stricter entity type validation.

- validation_method:

  Character. Method to use for entity validation: "pattern", "nlp",
  "api", or "comprehensive".

## Value

A data frame with ranked discovery results.

## Examples

``` r
# Create a simple example co-occurrence matrix
set.seed(123)
terms <- c("migraine", "headache", "pain", "serotonin", "sumatriptan", "CGRP")
n_terms <- length(terms)
co_matrix <- matrix(runif(n_terms^2, 0, 1), nrow = n_terms, ncol = n_terms)
rownames(co_matrix) <- colnames(co_matrix) <- terms
diag(co_matrix) <- 1  # Perfect self-similarity

# Add entity types
entity_types <- c("disease", "symptom", "symptom", "chemical", "drug", "protein")
names(entity_types) <- terms
attr(co_matrix, "entity_types") <- entity_types

# Apply ABC model
results <- abc_model(co_matrix, a_term = "migraine", min_score = 0.1, n_results = 5)
#> Filtered 1 B terms (20%) that weren't valid biomedical entities
#> Filtered out 0 B terms that were too similar to A term (similarity threshold: 0.8)
#> Filtered 1 potential C terms that weren't valid biomedical entities
#> Identifying potential C terms via 4 B terms...
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> No ABC connections found
print(results)
#> [1] a_term    b_term    c_term    a_b_score b_c_score abc_score
#> <0 rows> (or 0-length row.names)
```
