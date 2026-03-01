# Find primary term in co-occurrence matrix

This function verifies that the primary term exists in the co-occurrence
matrix, and if not, attempts to find a suitable variation.

## Usage

``` r
find_term(co_matrix, primary_term, verbose = TRUE)
```

## Arguments

- co_matrix:

  The co-occurrence matrix

- primary_term:

  The primary term to find

- verbose:

  Logical; if TRUE, print status messages (default: TRUE)

## Value

The found term (either exact match or variation)

## Examples

``` r
# Create example co-occurrence matrix
terms <- c("migraine", "headache", "pain", "serotonin")
co_matrix <- matrix(runif(16, 0, 1), nrow = 4, ncol = 4)
rownames(co_matrix) <- colnames(co_matrix) <- terms

# Find term in matrix
found_term <- find_term(co_matrix, "migraine")
#> Found primary term in co-occurrence matrix
print(found_term)
#> [1] "migraine"
```
