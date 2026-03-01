# Find all potential ABC connections

This function finds all potential ABC connections in a co-occurrence
matrix.

## Usage

``` r
find_abc_all(
  co_matrix,
  a_type = NULL,
  c_type = NULL,
  min_score = 0.1,
  n_results = 1000
)
```

## Arguments

- co_matrix:

  A co-occurrence matrix produced by create_comat().

- a_type:

  Character string, the entity type for A terms.

- c_type:

  Character string, the entity type for C terms.

- min_score:

  Minimum score threshold for results.

- n_results:

  Maximum number of results to return.

## Value

A data frame with ranked discovery results.

## Examples

``` r
if (FALSE) { # \dontrun{
all_abc <- find_abc_all(co_matrix, a_type = "source", c_type = "target")
} # }
```
