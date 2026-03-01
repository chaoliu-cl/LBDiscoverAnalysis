# Filter a co-occurrence matrix by entity type

Filter a co-occurrence matrix by entity type

## Usage

``` r
filter_by_type(co_matrix, types)
```

## Arguments

- co_matrix:

  A co-occurrence matrix produced by create_typed_comat().

- types:

  Character vector of entity types to include.

## Value

A filtered co-occurrence matrix.

## Examples

``` r
if (FALSE) { # \dontrun{
# Keep only disease and drug entities
filtered_matrix <- filter_by_type(co_matrix, types = c("disease", "drug"))
} # }
```
