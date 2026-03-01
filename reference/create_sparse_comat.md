# Create a sparse co-occurrence matrix

This function creates a sparse co-occurrence matrix from entity data,
which is more memory-efficient for large datasets.

## Usage

``` r
create_sparse_comat(
  entity_data,
  doc_id_col = "doc_id",
  entity_col = "entity",
  count_col = NULL,
  type_col = NULL,
  normalize = TRUE
)
```

## Arguments

- entity_data:

  A data frame with document IDs and entities.

- doc_id_col:

  Name of the column containing document IDs.

- entity_col:

  Name of the column containing entity names.

- count_col:

  Name of the column containing entity counts (optional).

- type_col:

  Name of the column containing entity types (optional).

- normalize:

  Logical. If TRUE, normalizes the co-occurrence matrix.

## Value

A sparse matrix of entity co-occurrences.

## Examples

``` r
if (FALSE) { # \dontrun{
co_matrix <- create_sparse_comat(entities,
                                              doc_id_col = "doc_id",
                                              entity_col = "entity")
} # }
```
